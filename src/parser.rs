use std::fmt;
use std::ops::Range;
use std::sync::atomic::{AtomicU64, Ordering};

use crate::types::QualifiedTypeName;

pub(crate) type Span = Range<usize>;

/// Default maximum nesting depth the parser will accept before returning a
/// clean error instead of risking a native stack overflow. The value is kept
/// conservative so that, at the default budget, the produced AST is shallow
/// enough that the *downstream* passes (typecheck and codegen, which still walk
/// the AST recursively) also stay within a typical 2 MiB thread stack.
pub const DEFAULT_MAX_PARSE_DEPTH: usize = 128;

/// Stable identity for one AST node across compiler passes.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub(crate) struct AstId(u64);

static NEXT_AST_ID: AtomicU64 = AtomicU64::new(0);

impl AstId {
  fn fresh() -> Self {
    Self(NEXT_AST_ID.fetch_add(1, Ordering::Relaxed))
  }
}

/// Monotonic counter for unique temporary names created when desugaring `bind`
/// forms. Each `bind` introduces a fresh local to hold the destructured value
/// so that nested or repeated `bind`s never collide.
static NEXT_BIND_TEMP: AtomicU64 = AtomicU64::new(0);

fn fresh_bind_temp_name() -> String {
  let n = NEXT_BIND_TEMP.fetch_add(1, Ordering::Relaxed);
  format!("__bind_tmp_{n}")
}

/// Stable identity for one lexical binding within a resolved module.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub(crate) struct BindingId(u32);

impl BindingId {
  const UNRESOLVED: Self = Self(u32::MAX);

  pub(crate) fn resolved(index: u32) -> Self {
    Self(index)
  }

  pub(crate) fn synthetic(index: u32) -> Self {
    Self(u32::MAX - 1 - index)
  }

  pub(crate) fn is_resolved(self) -> bool {
    self != Self::UNRESOLVED
  }
}

/// A source name paired with the lexical binding it resolves to.
///
/// The parser creates unresolved names; [`crate::resolver::resolve_module_names`]
/// assigns binding IDs before typechecking and lowering.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub(crate) struct ResolvedName {
  pub(crate) name: String,
  pub(crate) binding: BindingId,
}

impl ResolvedName {
  pub(crate) fn unresolved(name: impl Into<String>) -> Self {
    Self {
      name: name.into(),
      binding: BindingId::UNRESOLVED,
    }
  }

  pub(crate) fn resolved(name: impl Into<String>, binding: BindingId) -> Self {
    Self {
      name: name.into(),
      binding,
    }
  }

  pub(crate) fn as_str(&self) -> &str {
    &self.name
  }
}

impl fmt::Display for ResolvedName {
  fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.name.fmt(formatter)
  }
}

impl PartialEq<str> for ResolvedName {
  fn eq(&self, other: &str) -> bool {
    self.name == other
  }
}

impl PartialEq<&str> for ResolvedName {
  fn eq(&self, other: &&str) -> bool {
    self.name == *other
  }
}

impl From<String> for ResolvedName {
  fn from(name: String) -> Self {
    Self::unresolved(name)
  }
}

impl From<&str> for ResolvedName {
  fn from(name: &str) -> Self {
    Self::unresolved(name)
  }
}

#[derive(Clone)]
pub(crate) struct AST {
  id: AstId,
  pub(crate) kind: ASTKind,
  pub(crate) span: Span,
}

impl fmt::Debug for AST {
  fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
    formatter
      .debug_struct("AST")
      .field("kind", &self.kind)
      .field("span", &self.span)
      .finish()
  }
}

impl PartialEq for AST {
  fn eq(&self, other: &Self) -> bool {
    // Compiler metadata is not part of the program's semantics.
    self.kind == other.kind
  }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum ASTKind {
  Let(ResolvedName, Option<TypeAst>, Box<AST>),
  /// Shadow an existing binding by introducing a fresh one. Equivalent to
  /// `let`, but does not error when the name is already in scope: it explicitly
  /// calls out that shadowing is happening, and may change the binding's type.
  Shd(ResolvedName, Option<TypeAst>, Box<AST>),
  /// Assign to an already-bound local name. The name must already be bound in
  /// the current local scope, and the new value must have the same type as the
  /// existing binding.
  Assign(ResolvedName, Option<TypeAst>, Box<AST>),
  DefineFn(Function),
  DefineStruct(Struct),
  DefineEnum(Enum),
  Call(Box<AST>, Vec<AST>),
  CallFixed(Identifier, Vec<AST>),
  Variable(ResolvedName),
  Int(i64),
  Float(f64),
  String(String),
  Bool(bool),
  /// Construct a struct or enum value. `path` is the `::`-separated type path
  /// (1 to 3 segments). Whether `path` names a struct or an enum variant is
  /// resolved by the typechecker and recorded in [`TypecheckInfo`].
  New {
    path: Vec<String>,
    fields: Vec<(String, AST)>,
  },
  /// Construct an anonymous tuple from positional element expressions.
  NewTuple(Vec<AST>),
  FieldAccess(Box<AST>, String),
  Match(Box<AST>, Vec<MatchArm>),

  /// Bind up some arguments with a callable. This is used for closure captures.
  /// Not represented directly in source syntax.
  PartialApply(Box<AST>, Vec<AST>),
  /// Get a reference to a function. Source syntax is `module::function`.
  FunctionRef(String, String),
  /// Conditional: evaluate `cond`; if truthy, evaluate `then`, else evaluate `els`.
  /// An omitted `else` branch yields `Void`.
  If(Box<AST>, Box<AST>, Option<Box<AST>>),
  /// A sequence: evaluate each sub-expression in order, discarding all but the
  /// last, and return the last. Lets a single-expression position (e.g. an `if`
  /// branch) evaluate multiple expressions for side effects.
  Block(Vec<AST>),
  /// Return immediately from the enclosing function. An omitted expression
  /// returns `Void`.
  Return(Option<Box<AST>>),
  /// Boolean conjunction with left-to-right short-circuiting.
  And(Vec<AST>),
  /// Boolean disjunction with left-to-right short-circuiting.
  Or(Vec<AST>),
  /// Iterate over a list, binding each item while evaluating the body.
  For(ResolvedName, Box<AST>, Vec<AST>),
}

impl AST {
  pub(crate) fn new(kind: ASTKind, span: Span) -> Self {
    Self {
      id: AstId::fresh(),
      kind,
      span,
    }
  }

  pub(crate) fn id(&self) -> AstId {
    self.id
  }

  #[cfg(test)]
  pub(crate) fn synthetic(kind: ASTKind) -> Self {
    Self::new(kind, 0..0)
  }

  pub(crate) fn with_kind(&self, kind: ASTKind) -> Self {
    Self {
      id: self.id,
      kind,
      span: self.span.clone(),
    }
  }
}

/// Rebuild an AST node after mapping each of its immediate expression children.
///
/// This keeps traversal policy in the caller: scope-sensitive passes can handle
/// nodes such as `Let`, `DefineFn`, and `Block` themselves, then use this helper
/// for the structurally recursive cases.
pub(crate) fn try_map_ast_children<E>(
  ast: &AST,
  mut map: impl FnMut(&AST) -> Result<AST, E>,
) -> Result<AST, E> {
  let kind = match &ast.kind {
    ASTKind::Let(name, annotation, expression) => {
      ASTKind::Let(name.clone(), annotation.clone(), Box::new(map(expression)?))
    }
    ASTKind::Shd(name, annotation, expression) => {
      ASTKind::Shd(name.clone(), annotation.clone(), Box::new(map(expression)?))
    }
    ASTKind::Assign(name, annotation, expression) => {
      ASTKind::Assign(name.clone(), annotation.clone(), Box::new(map(expression)?))
    }
    ASTKind::DefineFn(function) => {
      let mut function = function.clone();
      function.code = function
        .code
        .iter()
        .map(&mut map)
        .collect::<Result<_, _>>()?;
      ASTKind::DefineFn(function)
    }
    ASTKind::Call(callable, args) => ASTKind::Call(
      Box::new(map(callable)?),
      args.iter().map(&mut map).collect::<Result<_, _>>()?,
    ),
    ASTKind::CallFixed(identifier, args) => ASTKind::CallFixed(
      identifier.clone(),
      args.iter().map(&mut map).collect::<Result<_, _>>()?,
    ),
    ASTKind::New { path, fields } => ASTKind::New {
      path: path.clone(),
      fields: fields
        .iter()
        .map(|(field, expression)| Ok((field.clone(), map(expression)?)))
        .collect::<Result<_, _>>()?,
    },
    ASTKind::NewTuple(elements) => {
      ASTKind::NewTuple(elements.iter().map(&mut map).collect::<Result<_, _>>()?)
    }
    ASTKind::FieldAccess(receiver, field) => {
      ASTKind::FieldAccess(Box::new(map(receiver)?), field.clone())
    }
    ASTKind::Match(scrutinee, arms) => ASTKind::Match(
      Box::new(map(scrutinee)?),
      arms
        .iter()
        .map(|arm| {
          Ok(MatchArm {
            pattern: arm.pattern.clone(),
            body: map(&arm.body)?,
          })
        })
        .collect::<Result<_, _>>()?,
    ),
    ASTKind::PartialApply(callable, args) => ASTKind::PartialApply(
      Box::new(map(callable)?),
      args.iter().map(&mut map).collect::<Result<_, _>>()?,
    ),
    ASTKind::If(condition, then_branch, else_branch) => ASTKind::If(
      Box::new(map(condition)?),
      Box::new(map(then_branch)?),
      match else_branch {
        Some(branch) => Some(Box::new(map(branch)?)),
        None => None,
      },
    ),
    ASTKind::Block(body) => ASTKind::Block(body.iter().map(&mut map).collect::<Result<_, _>>()?),
    ASTKind::Return(value) => {
      ASTKind::Return(value.as_deref().map(&mut map).transpose()?.map(Box::new))
    }
    ASTKind::And(operands) => {
      ASTKind::And(operands.iter().map(&mut map).collect::<Result<_, _>>()?)
    }
    ASTKind::Or(operands) => ASTKind::Or(operands.iter().map(&mut map).collect::<Result<_, _>>()?),
    ASTKind::For(name, iterable, body) => ASTKind::For(
      name.clone(),
      Box::new(map(iterable)?),
      body.iter().map(&mut map).collect::<Result<_, _>>()?,
    ),
    ASTKind::Variable(_)
    | ASTKind::Int(_)
    | ASTKind::Float(_)
    | ASTKind::String(_)
    | ASTKind::Bool(_)
    | ASTKind::FunctionRef(_, _)
    | ASTKind::DefineStruct(_)
    | ASTKind::DefineEnum(_) => return Ok(ast.clone()),
  };
  Ok(ast.with_kind(kind))
}

#[cfg(test)]
#[allow(non_snake_case)]
impl AST {
  pub(crate) fn Let(name: String, value: Box<AST>) -> Self {
    Self::synthetic(ASTKind::Let(name.into(), None, value))
  }

  pub(crate) fn DefineFn(function: Function) -> Self {
    Self::synthetic(ASTKind::DefineFn(function))
  }

  pub(crate) fn CallFixed(identifier: Identifier, args: Vec<AST>) -> Self {
    Self::synthetic(ASTKind::CallFixed(identifier, args))
  }

  pub(crate) fn Variable(name: String) -> Self {
    Self::synthetic(ASTKind::Variable(name.into()))
  }

  pub(crate) fn Int(value: i64) -> Self {
    Self::synthetic(ASTKind::Int(value))
  }
  pub(crate) fn Bool(value: bool) -> Self {
    Self::synthetic(ASTKind::Bool(value))
  }

  pub(crate) fn And(operands: Vec<AST>) -> Self {
    Self::synthetic(ASTKind::And(operands))
  }

  pub(crate) fn Or(operands: Vec<AST>) -> Self {
    Self::synthetic(ASTKind::Or(operands))
  }

  fn Float(value: f64) -> Self {
    Self::synthetic(ASTKind::Float(value))
  }

  fn String(value: String) -> Self {
    Self::synthetic(ASTKind::String(value))
  }

  pub(crate) fn PartialApply(callable: Box<AST>, args: Vec<AST>) -> Self {
    Self::synthetic(ASTKind::PartialApply(callable, args))
  }

  pub(crate) fn FunctionRef(module: String, name: String) -> Self {
    Self::synthetic(ASTKind::FunctionRef(module, name))
  }

  fn FieldAccess(receiver: AST, field: String) -> Self {
    Self::synthetic(ASTKind::FieldAccess(Box::new(receiver), field))
  }

  pub(crate) fn NewTuple(elements: Vec<AST>) -> Self {
    Self::synthetic(ASTKind::NewTuple(elements))
  }
}

#[cfg(test)]
/// Clone an AST and clear its binding IDs so tests can compare structure
/// without depending on resolver allocation order.
pub(crate) fn erase_bindings(asts: &[AST]) -> Vec<AST> {
  fn erase_name(name: &mut ResolvedName) {
    name.binding = BindingId::UNRESOLVED;
  }

  fn erase_ast(ast: &mut AST) {
    match &mut ast.kind {
      ASTKind::Let(name, _, expression)
      | ASTKind::Shd(name, _, expression)
      | ASTKind::Assign(name, _, expression) => {
        erase_name(name);
        erase_ast(expression);
      }
      ASTKind::DefineFn(function) => {
        erase_name(&mut function.name);
        for (param, _) in &mut function.params {
          erase_name(param);
        }
        for expression in &mut function.code {
          erase_ast(expression);
        }
      }
      ASTKind::Call(callable, args) | ASTKind::PartialApply(callable, args) => {
        erase_ast(callable);
        for arg in args {
          erase_ast(arg);
        }
      }
      ASTKind::CallFixed(identifier, args) => {
        if let Identifier::Bare(name) = identifier {
          erase_name(name);
        }
        for arg in args {
          erase_ast(arg);
        }
      }
      ASTKind::Variable(name) => erase_name(name),
      ASTKind::New { fields, .. } => {
        for (_, expression) in fields {
          erase_ast(expression);
        }
      }
      ASTKind::NewTuple(elements) => {
        for expression in elements {
          erase_ast(expression);
        }
      }
      ASTKind::FieldAccess(receiver, _) => erase_ast(receiver),
      ASTKind::Match(scrutinee, arms) => {
        erase_ast(scrutinee);
        for arm in arms {
          if let MatchPattern::Variant { fields, .. } = &mut arm.pattern {
            for field in fields {
              erase_name(field);
            }
          }
          erase_ast(&mut arm.body);
        }
      }
      ASTKind::If(condition, then_branch, else_branch) => {
        erase_ast(condition);
        erase_ast(then_branch);
        if let Some(else_branch) = else_branch {
          erase_ast(else_branch);
        }
      }
      ASTKind::Block(body) => {
        for expression in body {
          erase_ast(expression);
        }
      }
      ASTKind::Return(value) => {
        if let Some(value) = value {
          erase_ast(value);
        }
      }
      ASTKind::And(operands) | ASTKind::Or(operands) => {
        for operand in operands {
          erase_ast(operand);
        }
      }
      ASTKind::For(name, iterable, body) => {
        erase_name(name);
        erase_ast(iterable);
        for expression in body {
          erase_ast(expression);
        }
      }
      ASTKind::Int(_)
      | ASTKind::Float(_)
      | ASTKind::String(_)
      | ASTKind::Bool(_)
      | ASTKind::FunctionRef(_, _)
      | ASTKind::DefineStruct(_)
      | ASTKind::DefineEnum(_) => {}
    }
  }

  let mut asts = asts.to_vec();
  for ast in &mut asts {
    erase_ast(ast);
  }
  asts
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Identifier {
  Bare(ResolvedName),
  Qualified(String, String),
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Function {
  pub(crate) name: ResolvedName,
  pub(crate) params: Vec<(ResolvedName, Option<TypeAst>)>,
  pub(crate) return_type: Option<TypeAst>,
  pub(crate) bounds: Vec<Bound>,
  pub(crate) code: Vec<AST>,
}

impl Function {
  pub(crate) fn returns_void(&self) -> bool {
    match &self.return_type {
      None => true,
      Some(TypeAst::Named(TypeNameAst::Bare(name))) => name == "Void",
      Some(TypeAst::Named(TypeNameAst::Qualified(_))) => false,
      Some(TypeAst::Apply(_, _) | TypeAst::Fn(_, _, _)) => false,
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Struct {
  pub(crate) name: String,
  pub(crate) fields: Vec<(String, TypeAst)>,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Enum {
  pub(crate) name: String,
  pub(crate) variants: Vec<EnumVariant>,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct EnumVariant {
  pub(crate) name: String,
  pub(crate) fields: Vec<(String, TypeAst)>,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct MatchArm {
  pub(crate) pattern: MatchPattern,
  pub(crate) body: AST,
}

/// Which kind of binding a `bind` pattern introduces.
#[derive(Debug, PartialEq, Clone, Copy)]
enum BindKind {
  /// `[let name]` — introduce a fresh binding.
  Let,
  /// `[shd name]` — shadow an existing binding with a fresh one.
  Shd,
  /// `[= name]` — assign to an existing binding (same type).
  Assign,
}

/// One positional binding target inside a `bind` pattern group.
#[derive(Debug, PartialEq, Clone)]
struct BindTarget {
  kind: BindKind,
  name: String,
  span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum MatchPattern {
  Variant {
    variant: String,
    fields: Vec<ResolvedName>,
  },
  Default,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum TypeNameAst {
  Bare(String),
  Qualified(QualifiedTypeName),
}

impl TypeNameAst {
  pub(crate) fn bare(name: impl Into<String>) -> Self {
    Self::Bare(name.into())
  }

  pub(crate) fn qualified(module: impl Into<String>, name: impl Into<String>) -> Self {
    Self::Qualified(QualifiedTypeName::new(module, name))
  }
}

impl From<&str> for TypeNameAst {
  fn from(name: &str) -> Self {
    Self::bare(name)
  }
}

impl From<String> for TypeNameAst {
  fn from(name: String) -> Self {
    Self::bare(name)
  }
}

impl fmt::Display for TypeNameAst {
  fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Bare(name) => name.fmt(formatter),
      Self::Qualified(name) => name.fmt(formatter),
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum TypeAst {
  Named(TypeNameAst),
  Apply(String, Vec<TypeAst>),
  Fn(Vec<TypeAst>, Option<Box<TypeAst>>, Box<TypeAst>),
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Bound {
  pub(crate) var: String,
  pub(crate) traits: Vec<String>,
}

#[derive(Debug, PartialEq, Clone)]
enum TokenKind {
  LBracket,
  RBracket,
  LParen,
  RParen,
  Colon,
  DoubleColon,
  Arrow,
  FatArrow,
  Ellipsis,
  Newline,
  Indent,
  Dedent,
  Let,
  Shd,
  Assign,
  Fn,
  Struct,
  Enum,
  New,
  Match,
  If,
  Else,
  Block,
  Return,
  And,
  Or,
  For,
  Bind,
  In,
  Where,
  Sym(String),
  Bool(bool),
  Int(i64),
  Float(f64),
  Str(String),
  Eof,
}

#[derive(Debug, PartialEq, Clone)]
struct Token {
  kind: TokenKind,
  span: Span,
}

impl fmt::Display for TokenKind {
  fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      TokenKind::LBracket => write!(formatter, "["),
      TokenKind::RBracket => write!(formatter, "]"),
      TokenKind::LParen => write!(formatter, "("),
      TokenKind::RParen => write!(formatter, ")"),
      TokenKind::Colon => write!(formatter, ":"),
      TokenKind::DoubleColon => write!(formatter, "::"),
      TokenKind::Arrow => write!(formatter, "->"),
      TokenKind::FatArrow => write!(formatter, "=>"),
      TokenKind::Ellipsis => write!(formatter, "..."),
      TokenKind::Newline => write!(formatter, "newline"),
      TokenKind::Indent => write!(formatter, "indent"),
      TokenKind::Dedent => write!(formatter, "dedent"),
      TokenKind::Let => write!(formatter, "let"),
      TokenKind::Shd => write!(formatter, "shd"),
      TokenKind::Assign => write!(formatter, "="),
      TokenKind::Fn => write!(formatter, "fn"),
      TokenKind::Struct => write!(formatter, "struct"),
      TokenKind::Enum => write!(formatter, "enum"),
      TokenKind::New => write!(formatter, "new"),
      TokenKind::Match => write!(formatter, "match"),
      TokenKind::If => write!(formatter, "if"),
      TokenKind::Else => write!(formatter, "else"),
      TokenKind::Block => write!(formatter, "block"),
      TokenKind::Return => write!(formatter, "return"),
      TokenKind::And => write!(formatter, "and"),
      TokenKind::Or => write!(formatter, "or"),
      TokenKind::For => write!(formatter, "for"),
      TokenKind::Bind => write!(formatter, "bind"),
      TokenKind::In => write!(formatter, "in"),
      TokenKind::Where => write!(formatter, "where"),
      TokenKind::Sym(name) => write!(formatter, "{name}"),
      TokenKind::Bool(value) => write!(formatter, "{value}"),
      TokenKind::Int(value) => write!(formatter, "{value}"),
      TokenKind::Float(value) => write!(formatter, "{value}"),
      TokenKind::Str(value) => write!(formatter, "\"{value}\""),
      TokenKind::Eof => write!(formatter, "end of input"),
    }
  }
}

impl fmt::Display for Token {
  fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.kind.fmt(formatter)
  }
}

#[derive(Debug, PartialEq)]
struct ParseError {
  span: Span,
  message: String,
  annotations: Vec<String>,
  expected: Vec<String>,
}

impl ParseError {
  fn new(span: Span, message: impl Into<String>) -> Self {
    Self {
      span,
      message: message.into(),
      annotations: Vec::new(),
      expected: Vec::new(),
    }
  }

  fn annotate(mut self, annotation: impl Into<String>) -> Self {
    self.annotations.push(annotation.into());
    self
  }

  fn expected(mut self, expected: impl Into<String>) -> Self {
    self.expected.push(expected.into());
    self
  }

  fn render(&self, source: &str) -> String {
    let (line, column) = source_position(source, self.span.start);
    let mut parts = Vec::with_capacity(2 + self.annotations.len());
    parts.push(self.message.clone());
    parts.extend(self.annotations.iter().cloned());

    if !self.expected.is_empty() {
      parts.push(format!("expected {}", self.expected.join(" or ")));
    }

    format!("line {line}, column {column}: {}", parts.join("; "))
  }

  fn unexpected(token: Token, expected: impl Into<String>) -> Self {
    let message = format!("unexpected {}", token.kind);
    Self::new(token.span, message).expected(expected.into())
  }
}

pub(crate) fn source_position(source: &str, offset: usize) -> (usize, usize) {
  let offset = offset.min(source.len());
  let before = &source[..offset];
  let line = before.bytes().filter(|byte| *byte == b'\n').count() + 1;
  let column = before
    .rsplit_once('\n')
    .map_or(before, |(_, current_line)| current_line)
    .chars()
    .count()
    + 1;
  (line, column)
}

struct Lexer<'a> {
  /// Original source text. Spans are byte offsets into this string.
  source: &'a str,
  /// Current byte offset into `source`.
  offset: usize,
  /// Final token stream consumed by the parser, including layout tokens.
  output: Vec<Token>,
  /// Number of unmatched open-parens. Newlines are only significant when this is 0.
  bracket_depth: usize,
  /// Stack of active layout indentation columns.
  indent_stack: Vec<usize>,
  /// The layout-opening candidate currently being scanned.
  layout_head: Option<LayoutHead>,
  /// Deferred newline from the previous logical line. It is emitted only when
  /// the next line stays in the same layout block.
  pending_line_end: Option<Span>,
  /// Indentation column of a line that opened a layout body. The next real line
  /// must be indented further to produce an `Indent`.
  pending_layout: Option<usize>,
}

struct LayoutHead {
  indent: usize,
  opens_body: bool,
}

impl<'a> Lexer<'a> {
  fn new(source: &'a str) -> Self {
    Self {
      source,
      offset: 0,
      output: Vec::new(),
      bracket_depth: 0,
      indent_stack: vec![0],
      layout_head: None,
      pending_line_end: None,
      pending_layout: None,
    }
  }

  fn lex(mut self) -> Result<Vec<Token>, ParseError> {
    loop {
      self.skip_ignored();
      if self.offset == self.source.len() {
        self.finish_line(None);
        self.pending_layout = None;
        self.pending_line_end = None;
        while self.indent_stack.len() > 1 {
          self.indent_stack.pop();
          self.output.push(Token {
            kind: TokenKind::Dedent,
            span: self.offset..self.offset,
          });
        }
        self.output.push(Token {
          kind: TokenKind::Eof,
          span: self.offset..self.offset,
        });
        return Ok(self.output);
      }

      let start = self.offset;
      let ch = self.peek_char().expect("offset is before end of source");
      let token = match ch {
        '\n' => {
          self.bump_char();
          Token {
            kind: TokenKind::Newline,
            span: start..self.offset,
          }
        }
        '[' => {
          self.bump_char();
          Token {
            kind: TokenKind::LBracket,
            span: start..self.offset,
          }
        }
        ']' => {
          self.bump_char();
          Token {
            kind: TokenKind::RBracket,
            span: start..self.offset,
          }
        }
        '(' => {
          self.bump_char();
          Token {
            kind: TokenKind::LParen,
            span: start..self.offset,
          }
        }
        ')' => {
          self.bump_char();
          Token {
            kind: TokenKind::RParen,
            span: start..self.offset,
          }
        }
        ':' if self.source[self.offset..].starts_with("::") => {
          self.bump_char();
          self.bump_char();
          Token {
            kind: TokenKind::DoubleColon,
            span: start..self.offset,
          }
        }
        ':' => {
          self.bump_char();
          Token {
            kind: TokenKind::Colon,
            span: start..self.offset,
          }
        }
        '-' if self.source[self.offset..].starts_with("->") => {
          self.bump_char();
          self.bump_char();
          Token {
            kind: TokenKind::Arrow,
            span: start..self.offset,
          }
        }
        '=' if self.source[self.offset..].starts_with("=>") => {
          self.bump_char();
          self.bump_char();
          Token {
            kind: TokenKind::FatArrow,
            span: start..self.offset,
          }
        }
        '.' if self.source[self.offset..].starts_with("...") => {
          self.bump_char();
          self.bump_char();
          self.bump_char();
          Token {
            kind: TokenKind::Ellipsis,
            span: start..self.offset,
          }
        }
        '"' => self.lex_string()?,
        _ => self.lex_value()?,
      };

      match token.kind {
        TokenKind::Newline if self.bracket_depth == 0 => self.finish_line(Some(token.span)),
        TokenKind::Newline => {}
        TokenKind::LBracket | TokenKind::LParen => {
          self.push_line_token(token)?;
          self.bracket_depth += 1;
        }
        TokenKind::RBracket | TokenKind::RParen => {
          self.push_line_token(token)?;
          self.bracket_depth = self.bracket_depth.saturating_sub(1);
        }
        _ => {
          self.push_line_token(token)?;
        }
      }
    }
  }

  fn push_line_token(&mut self, token: Token) -> Result<(), ParseError> {
    if self.layout_head.is_none() {
      let indent = self.source_indent(token.span.start)?;
      self.prepare_for_line(indent, token.span.start)?;
      self.layout_head = Some(LayoutHead {
        indent,
        opens_body: matches!(
          token.kind,
          TokenKind::Fn
            | TokenKind::Struct
            | TokenKind::Enum
            | TokenKind::New
            | TokenKind::Match
            | TokenKind::If
            | TokenKind::Else
            | TokenKind::Block
            | TokenKind::For
        ),
      });
    }

    self.output.push(token);
    Ok(())
  }

  fn finish_line(&mut self, newline_span: Option<Span>) {
    let Some(head) = self.layout_head.take() else {
      return;
    };

    self.pending_line_end = newline_span;
    if matches!(
      self.output.last().map(|token| &token.kind),
      Some(TokenKind::FatArrow)
    ) || head.opens_body
    {
      self.pending_layout = Some(head.indent);
    }
  }

  fn prepare_for_line(&mut self, indent: usize, line_start: usize) -> Result<(), ParseError> {
    if let Some(opener_indent) = self.pending_layout.take() {
      self.pending_line_end = None;
      if indent > opener_indent {
        self.indent_stack.push(indent);
        self.output.push(Token {
          kind: TokenKind::Indent,
          span: line_start..line_start,
        });
        return Ok(());
      }
    }

    let current = *self
      .indent_stack
      .last()
      .expect("indent stack always contains root indent");
    if indent > current {
      if self.indent_stack.len() == 1 {
        self.flush_pending_line_end();
        return Ok(());
      }
      return Err(ParseError::new(
        line_start..line_start,
        "unexpected indentation",
      ));
    }

    if indent == current {
      self.flush_pending_line_end();
      return Ok(());
    }

    self.pending_line_end = None;
    while indent
      < *self
        .indent_stack
        .last()
        .expect("indent stack always contains root indent")
    {
      self.indent_stack.pop();
      self.output.push(Token {
        kind: TokenKind::Dedent,
        span: line_start..line_start,
      });
    }

    if indent
      != *self
        .indent_stack
        .last()
        .expect("indent stack always contains root indent")
    {
      return Err(ParseError::new(
        line_start..line_start,
        "inconsistent indentation",
      ));
    }

    Ok(())
  }

  fn flush_pending_line_end(&mut self) {
    if let Some(span) = self.pending_line_end.take() {
      self.output.push(Token {
        kind: TokenKind::Newline,
        span,
      });
    }
  }

  fn skip_ignored(&mut self) {
    loop {
      while self
        .peek_char()
        .is_some_and(|ch| ch.is_whitespace() && ch != '\n')
      {
        self.bump_char();
      }

      if self.peek_char() != Some('#') {
        return;
      }

      while self.peek_char().is_some_and(|ch| ch != '\n') {
        self.bump_char();
      }
    }
  }

  fn lex_string(&mut self) -> Result<Token, ParseError> {
    let start = self.offset;
    self.bump_char();
    let contents_start = self.offset;

    while let Some(ch) = self.peek_char() {
      match ch {
        '"' => {
          let contents_end = self.offset;
          self.bump_char();
          let span = start..self.offset;
          let contents = &self.source[contents_start..contents_end];
          let value = unescape::unescape(contents)
            .ok_or_else(|| ParseError::new(span.clone(), "invalid escape in string literal"))?;
          return Ok(Token {
            kind: TokenKind::Str(value),
            span,
          });
        }
        '\\' => {
          self.bump_char();
          if self.bump_char().is_none() {
            return Err(
              ParseError::new(start..self.offset, "unterminated string literal")
                .expected("a character after `\\`")
                .expected("`\"`"),
            );
          }
        }
        _ => {
          self.bump_char();
        }
      }
    }

    Err(
      ParseError::new(start..self.offset, "unterminated string literal").expected("a closing `\"`"),
    )
  }

  fn lex_value(&mut self) -> Result<Token, ParseError> {
    let start = self.offset;
    while self.peek_char().is_some_and(|ch| !is_delimiter(ch))
      && !self.source[self.offset..].starts_with("->")
      && !self.source[self.offset..].starts_with("=>")
      && !self.source[self.offset..].starts_with("...")
    {
      self.bump_char();
    }

    let text = &self.source[start..self.offset];
    let span = start..self.offset;
    let kind = if is_numeric_candidate(text) {
      parse_number(text, span.clone())?
    } else {
      identifier_token_kind(text)
    };
    Ok(Token { kind, span })
  }

  fn peek_char(&self) -> Option<char> {
    self.source[self.offset..].chars().next()
  }

  fn bump_char(&mut self) -> Option<char> {
    let ch = self.peek_char()?;
    self.offset += ch.len_utf8();
    Some(ch)
  }

  fn source_indent(&self, offset: usize) -> Result<usize, ParseError> {
    let line_start = self.source[..offset]
      .rfind('\n')
      .map_or(0, |index| index + 1);
    let mut columns = 0usize;
    let mut cursor = line_start;

    for ch in self.source[line_start..offset].chars() {
      match ch {
        ' ' => columns += 1,
        '\t' => {
          return Err(ParseError::new(
            cursor..cursor + ch.len_utf8(),
            "tabs are not allowed in indentation",
          ));
        }
        '\r' => {}
        _ => break,
      }
      cursor += ch.len_utf8();
    }

    Ok(columns)
  }
}

fn identifier_token_kind(text: &str) -> TokenKind {
  match text {
    "let" => TokenKind::Let,
    "shd" => TokenKind::Shd,
    "=" => TokenKind::Assign,
    "fn" => TokenKind::Fn,
    "struct" => TokenKind::Struct,
    "enum" => TokenKind::Enum,
    "new" => TokenKind::New,
    "match" => TokenKind::Match,
    "if" => TokenKind::If,
    "else" => TokenKind::Else,
    "block" => TokenKind::Block,
    "return" => TokenKind::Return,
    "and" => TokenKind::And,
    "or" => TokenKind::Or,
    "for" => TokenKind::For,
    "bind" => TokenKind::Bind,
    "in" => TokenKind::In,
    "where" => TokenKind::Where,
    "true" => TokenKind::Bool(true),
    "false" => TokenKind::Bool(false),
    _ => TokenKind::Sym(text.to_string()),
  }
}

fn is_delimiter(ch: char) -> bool {
  ch.is_whitespace() || matches!(ch, '[' | ']' | '(' | ')' | '"' | '#' | ':')
}

fn is_numeric_candidate(text: &str) -> bool {
  let mut chars = text.chars();
  match chars.next() {
    Some(first) if first.is_ascii_digit() => true,
    Some('+' | '-') => chars.next().is_some_and(|ch| ch.is_ascii_digit()),
    _ => false,
  }
}

fn parse_number(text: &str, span: Span) -> Result<TokenKind, ParseError> {
  let unsigned = text.strip_prefix(['+', '-']).unwrap_or(text);

  if unsigned.bytes().all(|byte| byte.is_ascii_digit()) {
    return text.parse::<i64>().map(TokenKind::Int).map_err(|_| {
      ParseError::new(
        span,
        format!("integer literal `{text}` is outside the i64 range"),
      )
    });
  }

  let valid_float = unsigned.split_once('.').is_some_and(|(whole, fractional)| {
    !whole.is_empty()
      && !fractional.is_empty()
      && whole.bytes().all(|byte| byte.is_ascii_digit())
      && fractional.bytes().all(|byte| byte.is_ascii_digit())
  });
  if !valid_float {
    return Err(ParseError::new(
      span,
      format!("invalid decimal number `{text}`"),
    ));
  }

  let value = text
    .parse::<f64>()
    .map_err(|_| ParseError::new(span.clone(), format!("invalid decimal number `{text}`")))?;
  if !value.is_finite() {
    return Err(ParseError::new(
      span,
      format!("float literal `{text}` is not finite"),
    ));
  }
  Ok(TokenKind::Float(value))
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum FormMode {
  Bracket,
  Layout,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum FormEnd {
  RBracket,
  Dedent,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum NonemptyExprContext {
  Form(&'static str),
  IfThenBranch,
  ElseBranch,
  MatchArmBody,
}

struct FnHeader {
  name: String,
  params: Vec<(String, Option<TypeAst>)>,
  return_type: Option<TypeAst>,
  bounds: Vec<Bound>,
}

struct Parser {
  tokens: Vec<Token>,
  current: usize,
  max_depth: usize,
  depth: usize,
}

impl Parser {
  fn new(tokens: Vec<Token>, max_depth: usize) -> Self {
    Self {
      tokens,
      current: 0,
      max_depth,
      depth: 0,
    }
  }

  /// Record one level of nesting, returning a `ParseError` if it exceeds the
  /// configured `max_depth` budget. Make sure to call leave_depth() after.
  fn enter_depth(&mut self) -> Result<(), ParseError> {
    self.depth += 1;
    if self.depth > self.max_depth {
      let span = self.peek().span.clone();
      return Err(ParseError::new(
        span,
        format!(
          "nesting too deep: exceeds maximum parse depth of {} \
           (raise `max_parse_depth` in `CompileOptions` to allow more)",
          self.max_depth
        ),
      ));
    }
    Ok(())
  }

  /// Remove a level of nesting.
  fn leave_depth(&mut self) {
    self.depth -= 1;
  }

  fn parse_multiple(&mut self) -> Result<Vec<AST>, ParseError> {
    let mut result = Vec::new();
    self.skip_newlines();
    while !matches!(self.peek().kind, TokenKind::Eof) {
      result.push(self.parse_expr()?);
      self.skip_newlines();
    }
    Ok(result)
  }

  fn parse_expr(&mut self) -> Result<AST, ParseError> {
    self.enter_depth()?;
    let result = self.parse_expr_inner();
    self.leave_depth();
    result
  }

  fn parse_expr_inner(&mut self) -> Result<AST, ParseError> {
    let layout_line_start = self.at_layout_line_start();
    let token = self.advance();
    match token.kind {
      TokenKind::LBracket => self.parse_list(token.span.start, FormMode::Bracket),
      TokenKind::LParen => self.parse_infix(token.span.start),
      kind @ (TokenKind::Let
      | TokenKind::Shd
      | TokenKind::Assign
      | TokenKind::Fn
      | TokenKind::Struct
      | TokenKind::Enum
      | TokenKind::New
      | TokenKind::Match
      | TokenKind::If
      | TokenKind::Block
      | TokenKind::Return
      | TokenKind::And
      | TokenKind::Or
      | TokenKind::For
      | TokenKind::Bind)
        if layout_line_start && self.starts_layout_form(&kind) =>
      {
        self.parse_form_after_head(
          token.span.start,
          Token {
            kind,
            span: token.span.clone(),
          },
          FormMode::Layout,
        )
      }
      TokenKind::Sym(name) => {
        if let Some(((module, name), span)) =
          self.parse_qualified_identifier(name.clone(), token.span.clone())?
        {
          Ok(AST::new(ASTKind::FunctionRef(module, name), span))
        } else {
          Ok(ast_from_variable_or_field_access(name, token.span))
        }
      }
      TokenKind::Bool(value) => Ok(AST::new(ASTKind::Bool(value), token.span)),
      TokenKind::Int(value) => Ok(AST::new(ASTKind::Int(value), token.span)),
      TokenKind::Float(value) => Ok(AST::new(ASTKind::Float(value), token.span)),
      TokenKind::Str(value) => Ok(AST::new(ASTKind::String(value), token.span)),
      kind => Err(ParseError::unexpected(
        Token {
          kind,
          span: token.span,
        },
        "an expression",
      )),
    }
  }

  /// Parse a parenthesized infix expression.
  fn parse_infix(&mut self, start: usize) -> Result<AST, ParseError> {
    self.enter_depth()?;
    let result = self.parse_infix_climb(0);
    self.leave_depth();
    let mut left = result?;
    if let Some(close) = self.check_token(TokenKind::RParen) {
      left.span = start..close.span.end;
      Ok(left)
    } else {
      Err(
        ParseError::new(
          self.peek().span.clone(),
          "expected `)` to close infix expression",
        )
        .expected("an infix operator")
        .expected("`)`"),
      )
    }
  }

  /// Precedence-climbing core for [`parse_infix`].
  fn parse_infix_climb(&mut self, min_prec: u8) -> Result<AST, ParseError> {
    let mut left = self.parse_expr()?;
    // A lower-precedence operator belongs to an enclosing call, so we must
    // leave it in the token stream.
    loop {
      let token = self.peek().kind.clone();
      let Some(prec) = infix_precedence(&token) else {
        break;
      };
      if prec < min_prec {
        break;
      }
      self.advance();
      let right = self.parse_infix_climb(prec + 1)?;
      let span = left.span.start..right.span.end;
      // `and` and `or` are keywords (not bare `Sym`s) but are also accepted
      // as infix operators inside parentheses, where they desugar to the
      // short-circuiting `And`/`Or` forms rather than ordinary calls.
      left = match token {
        TokenKind::And => AST::new(ASTKind::And(vec![left, right]), span),
        TokenKind::Or => AST::new(ASTKind::Or(vec![left, right]), span),
        TokenKind::Sym(op) => AST::new(
          ASTKind::CallFixed(Identifier::Bare(op.into()), vec![left, right]),
          span,
        ),
        _ => break,
      };
    }
    Ok(left)
  }

  fn parse_list(&mut self, start: usize, mode: FormMode) -> Result<AST, ParseError> {
    if let Some(close) = self.check_token(TokenKind::RBracket) {
      return Err(ParseError::new(start..close.span.end, "Empty call"));
    }
    if matches!(self.peek().kind, TokenKind::Eof) {
      return Err(
        ParseError::new(self.peek().span.clone(), "unexpected end of input")
          .expected("an expression")
          .expected("`]`"),
      );
    }

    if is_form_head_token(&self.peek().kind) || matches!(self.peek().kind, TokenKind::Sym(_)) {
      let head = self.advance();
      self.parse_form_after_head(start, head, mode)
    } else {
      self.parse_dynamic_call(start)
    }
  }

  fn parse_form_after_head(
    &mut self,
    start: usize,
    head: Token,
    mode: FormMode,
  ) -> Result<AST, ParseError> {
    let Token {
      kind,
      span: head_span,
    } = head;
    match kind {
      TokenKind::Let => self.parse_let(start, mode),
      TokenKind::Shd => self.parse_shd(start, mode),
      TokenKind::Assign => self.parse_assign(start, mode),
      TokenKind::Fn => self.parse_fn(start, mode),
      TokenKind::Struct => self.parse_struct(start, mode),
      TokenKind::Enum => self.parse_enum(start, mode),
      TokenKind::New => self.parse_new(start, mode),
      TokenKind::Match => self.parse_match(start, mode),
      TokenKind::If => self.parse_if(start, mode),
      TokenKind::Block => self.parse_block(start, mode),
      TokenKind::Return => self.parse_return(start, mode),
      TokenKind::And => self.parse_boolean_form(start, mode, true),
      TokenKind::Or => self.parse_boolean_form(start, mode, false),
      TokenKind::For => self.parse_for(start, mode),
      TokenKind::Bind => self.parse_bind(start, mode),
      TokenKind::Sym(name) => self.parse_fixed_call_after_head(start, head_span, name, mode),
      _ => unreachable!("caller only passes valid form heads"),
    }
  }

  fn parse_let(&mut self, start: usize, mode: FormMode) -> Result<AST, ParseError> {
    let variable = self.expect_symbol("first argument to `let` must be a symbol")?;
    let annotation = if self.check_token(TokenKind::Colon).is_some() {
      Some(self.parse_type()?)
    } else {
      None
    };
    let expression = self.parse_expr()?;
    let close = match mode {
      FormMode::Bracket => {
        self.expect_form_end(FormEnd::RBracket, "`let` must have exactly two arguments")?
      }
      FormMode::Layout => self.expect_layout_line_end("`let` must have exactly two arguments")?,
    };
    let span = start..close.span.end;
    Ok(AST::new(
      ASTKind::Let(variable.into(), annotation, Box::new(expression)),
      span,
    ))
  }

  fn parse_shd(&mut self, start: usize, mode: FormMode) -> Result<AST, ParseError> {
    let variable = self.expect_symbol("first argument to `shd` must be a symbol")?;
    let annotation = if self.check_token(TokenKind::Colon).is_some() {
      Some(self.parse_type()?)
    } else {
      None
    };
    let expression = self.parse_expr()?;
    let close = match mode {
      FormMode::Bracket => {
        self.expect_form_end(FormEnd::RBracket, "`shd` must have exactly two arguments")?
      }
      FormMode::Layout => self.expect_layout_line_end("`shd` must have exactly two arguments")?,
    };
    let span = start..close.span.end;
    Ok(AST::new(
      ASTKind::Shd(variable.into(), annotation, Box::new(expression)),
      span,
    ))
  }

  fn parse_assign(&mut self, start: usize, mode: FormMode) -> Result<AST, ParseError> {
    let variable = self.expect_symbol("first argument to `=` must be a symbol")?;
    let annotation = if self.check_token(TokenKind::Colon).is_some() {
      Some(self.parse_type()?)
    } else {
      None
    };
    let expression = self.parse_expr()?;
    let close = match mode {
      FormMode::Bracket => {
        self.expect_form_end(FormEnd::RBracket, "`=` must have exactly two arguments")?
      }
      FormMode::Layout => self.expect_layout_line_end("`=` must have exactly two arguments")?,
    };
    let span = start..close.span.end;
    Ok(AST::new(
      ASTKind::Assign(variable.into(), annotation, Box::new(expression)),
      span,
    ))
  }

  fn parse_fn(&mut self, start: usize, mode: FormMode) -> Result<AST, ParseError> {
    let header = self.parse_fn_header()?;
    let (body, close) = self.parse_nonempty_exprs_for_form(mode, "fn")?;
    let span = start..close.span.end;
    Ok(AST::new(
      ASTKind::DefineFn(Function {
        name: header.name.into(),
        params: header
          .params
          .into_iter()
          .map(|(name, annotation)| (name.into(), annotation))
          .collect(),
        return_type: header.return_type,
        bounds: header.bounds,
        code: body,
      }),
      span,
    ))
  }

  fn parse_fn_header(&mut self) -> Result<FnHeader, ParseError> {
    let name = self.expect_symbol("`fn` name must be a symbol")?;
    self.expect(TokenKind::LBracket, "`fn` requires a parameter list")?;

    let mut params = Vec::new();
    while !matches!(self.peek().kind, TokenKind::RBracket) {
      if matches!(self.peek().kind, TokenKind::Eof) {
        return Err(
          ParseError::new(self.peek().span.clone(), "unterminated parameter list")
            .expected("a parameter name")
            .expected("`]`"),
        );
      }
      let param = self.expect_symbol("Parameters must be symbols")?;
      self.expect(
        TokenKind::Colon,
        "function parameters require a type annotation",
      )?;
      params.push((param, Some(self.parse_type()?)));
    }
    self.advance();

    let return_type = if self.check_token(TokenKind::Arrow).is_some() {
      Some(self.parse_type()?)
    } else {
      None
    };

    let mut bounds = Vec::new();
    if self.check_token(TokenKind::Where).is_some() {
      self.expect(
        TokenKind::LBracket,
        "`where` requires a bracketed bound list",
      )?;
      while !matches!(self.peek().kind, TokenKind::RBracket) {
        self.expect(TokenKind::LBracket, "each bound must be bracketed")?;
        let var = self.expect_symbol("a bound must name a type variable")?;
        let mut traits = Vec::new();
        while !matches!(self.peek().kind, TokenKind::RBracket) {
          traits.push(self.expect_symbol("trait names must be symbols")?);
        }
        if traits.is_empty() {
          return Err(ParseError::new(
            self.peek().span.clone(),
            "a bound must name at least one trait",
          ));
        }
        self.advance();
        bounds.push(Bound { var, traits });
      }
      self.advance();
    }

    Ok(FnHeader {
      name,
      params,
      return_type,
      bounds,
    })
  }

  fn parse_struct(&mut self, start: usize, mode: FormMode) -> Result<AST, ParseError> {
    let name = self.expect_symbol("`struct` name must be a symbol")?;
    let end = self.enter_form_body(mode, "struct")?;
    let mut fields = Vec::new();
    self.skip_newlines();
    while !self.at_form_end(end) {
      if matches!(self.peek().kind, TokenKind::Eof) {
        return Err(
          ParseError::new(self.peek().span.clone(), "unterminated `struct` form")
            .expected("a field")
            .expected(form_end_expected(end)),
        );
      }
      let field = self.expect_symbol("struct field names must be symbols")?;
      self.expect(TokenKind::Colon, "struct fields require a type annotation")?;
      fields.push((field, self.parse_type()?));
      self.finish_layout_item_line(mode, "struct fields must end at the end of the line")?;
      self.skip_newlines();
    }
    let close = self.expect_form_end(end, "unterminated `struct` form")?;
    let span = start..close.span.end;
    Ok(AST::new(
      ASTKind::DefineStruct(Struct { name, fields }),
      span,
    ))
  }

  fn parse_enum(&mut self, start: usize, mode: FormMode) -> Result<AST, ParseError> {
    let name = self.expect_symbol("`enum` name must be a symbol")?;
    let end = self.enter_form_body(mode, "enum")?;
    let mut variants = Vec::new();
    self.skip_newlines();
    while !self.at_form_end(end) {
      if matches!(self.peek().kind, TokenKind::Eof) {
        return Err(
          ParseError::new(self.peek().span.clone(), "unterminated `enum` form")
            .expected("a variant")
            .expected(form_end_expected(end)),
        );
      }
      self.expect(TokenKind::LBracket, "enum variants must be bracketed")?;
      let variant = self.expect_symbol("enum variant names must be symbols")?;
      let mut fields = Vec::new();
      while !matches!(self.peek().kind, TokenKind::RBracket) {
        if matches!(self.peek().kind, TokenKind::Eof) {
          return Err(
            ParseError::new(self.peek().span.clone(), "unterminated enum variant")
              .expected("a field")
              .expected("`]`"),
          );
        }
        let field = self.expect_symbol("enum variant field names must be symbols")?;
        self.expect(
          TokenKind::Colon,
          "enum variant fields require a type annotation",
        )?;
        fields.push((field, self.parse_type()?));
      }
      self.advance();
      variants.push(EnumVariant {
        name: variant,
        fields,
      });
      self.finish_layout_item_line(mode, "enum variants must end at the end of the line")?;
      self.skip_newlines();
    }
    let close = self.expect_form_end(end, "unterminated `enum` form")?;
    let span = start..close.span.end;
    Ok(AST::new(ASTKind::DefineEnum(Enum { name, variants }), span))
  }

  fn parse_new(&mut self, start: usize, mode: FormMode) -> Result<AST, ParseError> {
    let path = self.parse_new_head()?;
    let end = self.enter_form_body(mode, "new")?;
    let mut fields = Vec::new();
    self.skip_newlines();
    while !self.at_form_end(end) {
      if matches!(self.peek().kind, TokenKind::Eof) {
        return Err(
          ParseError::new(self.peek().span.clone(), "unterminated `new` form")
            .expected("a field initializer")
            .expected(form_end_expected(end)),
        );
      }
      let field = self.expect_symbol("struct initializer field names must be symbols")?;
      self.expect(TokenKind::Colon, "struct initializer fields require `:`")?;
      fields.push((field, self.parse_expr()?));
      self.finish_layout_item_line(
        mode,
        "struct initializer fields must end at the end of the line",
      )?;
      self.skip_newlines();
    }
    let close = self.expect_form_end(end, "unterminated `new` form")?;
    let span = start..close.span.end;
    Ok(AST::new(ASTKind::New { path, fields }, span))
  }

  /// Parse the `::`-separated type path following the `new` keyword. The
  /// typechecker resolves paths of one to three segments:
  ///
  ///   `new T`         -> `[T]`           (source struct)
  ///   `new T::V`       -> `[T, V]`        (source enum variant, or host struct)
  ///   `new M::T::V`    -> `[M, T, V]`     (library enum variant)
  ///
  /// Whether a 2-segment path names a source enum variant or a host struct is
  /// resolved later by the typechecker; longer paths are rejected then.
  fn parse_new_head(&mut self) -> Result<Vec<String>, ParseError> {
    // `check_token` consumes the match, so it doubles as both the test and the
    // advance past `::`.
    let mut path = vec![self.expect_symbol("`new` requires a struct or enum type name")?];
    while self.check_token(TokenKind::DoubleColon).is_some() {
      path.push(self.expect_symbol("type path requires a name after `::`")?);
    }
    Ok(path)
  }

  fn parse_match(&mut self, start: usize, mode: FormMode) -> Result<AST, ParseError> {
    let scrutinee = self.parse_expr()?;
    let end = self.enter_form_body(mode, "match")?;
    let mut arms = Vec::new();
    self.skip_newlines();
    while !self.at_form_end(end) {
      if matches!(self.peek().kind, TokenKind::Eof) {
        return Err(
          ParseError::new(self.peek().span.clone(), "unterminated `match` form")
            .expected("a match arm")
            .expected(form_end_expected(end)),
        );
      }
      arms.push(self.parse_match_arm(mode)?);
      self.finish_layout_item_line(mode, "match arms must end at the end of the line")?;
      self.skip_newlines();
    }
    if arms.is_empty() {
      return Err(ParseError::new(
        self.peek().span.clone(),
        "`match` must have at least one arm",
      ));
    }
    let close = self.expect_form_end(end, "unterminated `match` form")?;
    let span = start..close.span.end;
    Ok(AST::new(ASTKind::Match(Box::new(scrutinee), arms), span))
  }

  fn parse_match_arm(&mut self, mode: FormMode) -> Result<MatchArm, ParseError> {
    let pattern = if self.check_token(TokenKind::LBracket).is_some() {
      let variant = self.expect_symbol("match variant patterns must name a variant")?;
      let mut fields = Vec::new();
      while !matches!(self.peek().kind, TokenKind::RBracket) {
        if matches!(self.peek().kind, TokenKind::Eof) {
          return Err(
            ParseError::new(self.peek().span.clone(), "unterminated match pattern")
              .expected("a binding name")
              .expected("`]`"),
          );
        }
        fields.push(
          self
            .expect_symbol("match pattern fields must be symbols")?
            .into(),
        );
      }
      self.advance();
      MatchPattern::Variant { variant, fields }
    } else {
      let wildcard = self.expect_symbol("match arms require a pattern")?;
      if wildcard != "_" {
        return Err(ParseError::new(
          self.previous_token().span.clone(),
          "default match arm must use `_`",
        ));
      }
      MatchPattern::Default
    };
    self.expect(TokenKind::FatArrow, "match arms require `=>`")?;
    let body = self.parse_match_arm_body(mode)?;
    Ok(MatchArm { pattern, body })
  }

  fn parse_match_arm_body(&mut self, mode: FormMode) -> Result<AST, ParseError> {
    if mode == FormMode::Layout && matches!(self.peek().kind, TokenKind::Indent) {
      let body_end = self.enter_layout_body("match arm")?;
      let expressions =
        self.parse_nonempty_exprs_until(body_end, NonemptyExprContext::MatchArmBody)?;
      self.expect_form_end(
        body_end,
        nonempty_expr_eof_message(NonemptyExprContext::MatchArmBody),
      )?;
      return Ok(implicit_branch_block(expressions));
    }

    self.parse_expr()
  }

  fn parse_if(&mut self, start: usize, mode: FormMode) -> Result<AST, ParseError> {
    let condition = self.parse_expr()?;
    let (then_branch, else_branch, close) = match mode {
      FormMode::Bracket => {
        let then_branch = self.parse_expr()?;
        if matches!(self.peek().kind, TokenKind::RBracket) {
          let close = self.expect_form_end(
            FormEnd::RBracket,
            "`if` must have two or three arguments: cond, then, else",
          )?;
          (then_branch, None, close)
        } else {
          let else_branch = self.parse_expr()?;
          let close = self.expect_form_end(
            FormEnd::RBracket,
            "`if` must have two or three arguments: cond, then, else",
          )?;
          (then_branch, Some(Box::new(else_branch)), close)
        }
      }
      FormMode::Layout => {
        let then_end = self.enter_layout_body("if")?;
        let then_exprs =
          self.parse_nonempty_exprs_until(then_end, NonemptyExprContext::IfThenBranch)?;
        let then_close = self.expect_form_end(
          then_end,
          nonempty_expr_eof_message(NonemptyExprContext::IfThenBranch),
        )?;

        if self.check_token(TokenKind::Else).is_some() {
          let (else_branch, close) = self.parse_layout_else_branch()?;
          (
            implicit_branch_block(then_exprs),
            Some(Box::new(else_branch)),
            close,
          )
        } else {
          (implicit_branch_block(then_exprs), None, then_close)
        }
      }
    };
    let span = start..close.span.end;
    Ok(AST::new(
      ASTKind::If(Box::new(condition), Box::new(then_branch), else_branch),
      span,
    ))
  }

  fn parse_layout_else_branch(&mut self) -> Result<(AST, Token), ParseError> {
    if let Some(if_token) = self.check_token(TokenKind::If) {
      let branch = self.parse_if(if_token.span.start, FormMode::Layout)?;
      let close = Token {
        span: branch.span.clone(),
        kind: TokenKind::Dedent,
      };
      return Ok((branch, close));
    }

    let else_end = self.enter_layout_body("else")?;
    let else_exprs = self.parse_nonempty_exprs_until(else_end, NonemptyExprContext::ElseBranch)?;
    let close = self.expect_form_end(
      else_end,
      nonempty_expr_eof_message(NonemptyExprContext::ElseBranch),
    )?;
    Ok((implicit_branch_block(else_exprs), close))
  }

  fn parse_block(&mut self, start: usize, mode: FormMode) -> Result<AST, ParseError> {
    let (expressions, close) = self.parse_nonempty_exprs_for_form(mode, "block")?;
    let span = start..close.span.end;
    Ok(AST::new(ASTKind::Block(expressions), span))
  }

  fn parse_return(&mut self, start: usize, mode: FormMode) -> Result<AST, ParseError> {
    let value = match mode {
      FormMode::Bracket if matches!(self.peek().kind, TokenKind::RBracket) => None,
      FormMode::Layout
        if matches!(
          self.peek().kind,
          TokenKind::Newline | TokenKind::Dedent | TokenKind::Eof
        ) =>
      {
        None
      }
      _ => Some(Box::new(self.parse_expr()?)),
    };
    let close = match mode {
      FormMode::Bracket => {
        self.expect_form_end(FormEnd::RBracket, "`return` accepts at most one expression")?
      }
      FormMode::Layout => self.expect_layout_line_end("`return` accepts at most one expression")?,
    };
    Ok(AST::new(ASTKind::Return(value), start..close.span.end))
  }

  fn parse_boolean_form(
    &mut self,
    start: usize,
    mode: FormMode,
    is_and: bool,
  ) -> Result<AST, ParseError> {
    let (operands, close) = match mode {
      FormMode::Bracket => self.parse_call_args(mode)?,
      FormMode::Layout => {
        let mut operands = Vec::new();
        while !matches!(
          self.peek().kind,
          TokenKind::Newline | TokenKind::Dedent | TokenKind::Eof
        ) {
          operands.push(self.parse_expr()?);
        }
        let close = self.expect_layout_line_end("boolean operands must end with the line")?;
        (operands, close)
      }
    };
    if operands.len() < 2 {
      let form = if is_and { "and" } else { "or" };
      return Err(ParseError::new(
        start..close.span.end,
        format!("`{form}` requires at least two operands"),
      ));
    }
    let kind = if is_and {
      ASTKind::And(operands)
    } else {
      ASTKind::Or(operands)
    };
    Ok(AST::new(kind, start..close.span.end))
  }

  fn parse_for(&mut self, start: usize, mode: FormMode) -> Result<AST, ParseError> {
    let variable = self.expect_symbol("`for` binding must be a symbol")?;
    self.expect(TokenKind::In, "`for` requires `in` after its binding")?;
    let iterable = self.parse_expr()?;
    let (body, close) = self.parse_nonempty_exprs_for_form(mode, "for")?;
    Ok(AST::new(
      ASTKind::For(variable.into(), Box::new(iterable), body),
      start..close.span.end,
    ))
  }

  /// Parse a `bind` form, which destructures a tuple into positional bindings.
  ///
  /// `bind [[shd list] [let result]] [mktup]` desugars to:
  ///
  /// ```text
  /// [block
  ///   [let __bind_tmp_N [mktup]]
  ///   [shd list __bind_tmp_N.0]
  ///   [let result __bind_tmp_N.1]]
  /// ```
  ///
  /// Each pattern is `[let name]` (introduce a binding) or `[shd name]`
  /// (reassign an existing binding). The `bind` expression evaluates to the
  /// value of the last binding, matching the tuple element accessed by the
  /// last pattern; in a `Void` function that value is discarded.
  fn parse_bind(&mut self, start: usize, mode: FormMode) -> Result<AST, ParseError> {
    self.expect(
      TokenKind::LBracket,
      "`bind` requires a bracketed pattern group",
    )?;
    let mut targets = Vec::new();
    while !matches!(self.peek().kind, TokenKind::RBracket) {
      if matches!(self.peek().kind, TokenKind::Eof) {
        return Err(
          ParseError::new(
            self.peek().span.clone(),
            "unterminated `bind` pattern group",
          )
          .expected("a binding pattern")
          .expected("`]`"),
        );
      }
      targets.push(self.parse_bind_target()?);
    }
    let group_close = self.advance();
    if targets.is_empty() {
      return Err(ParseError::new(
        start..group_close.span.end,
        "`bind` requires at least one binding pattern",
      ));
    }
    let expression = self.parse_expr()?;
    let close = match mode {
      FormMode::Bracket => self.expect_form_end(
        FormEnd::RBracket,
        "`bind` must have a pattern group and an expression",
      )?,
      FormMode::Layout => {
        self.expect_layout_line_end("`bind` must have a pattern group and an expression")?
      }
    };
    let span = start..close.span.end;

    let temp_name = fresh_bind_temp_name();
    let temp_var = |span: Span| AST::new(ASTKind::Variable(temp_name.clone().into()), span);
    let field_access = |index: usize, span: Span| {
      AST::new(
        ASTKind::FieldAccess(Box::new(temp_var(span.clone())), index.to_string()),
        span,
      )
    };

    // One temp `let`, one binding per pattern, plus a trailing reference to
    // the temp so the `bind` evaluates to the whole tuple value.
    let mut body = Vec::with_capacity(targets.len() + 2);
    body.push(AST::new(
      ASTKind::Let(temp_name.clone().into(), None, Box::new(expression)),
      span.clone(),
    ));
    for (index, target) in targets.into_iter().enumerate() {
      let target_span = target.span;
      let access = field_access(index, target_span.clone());
      let kind = match target.kind {
        BindKind::Let => ASTKind::Let(target.name.into(), None, Box::new(access)),
        BindKind::Shd => ASTKind::Shd(target.name.into(), None, Box::new(access)),
        BindKind::Assign => ASTKind::Assign(target.name.into(), None, Box::new(access)),
      };
      body.push(AST::new(kind, target_span));
    }
    body.push(temp_var(span.clone()));

    Ok(AST::new(ASTKind::Block(body), span))
  }

  fn parse_bind_target(&mut self) -> Result<BindTarget, ParseError> {
    let open = self.expect(TokenKind::LBracket, "binding pattern must be bracketed")?;
    let head = self.advance();
    let kind = match head.kind {
      TokenKind::Let => BindKind::Let,
      TokenKind::Shd => BindKind::Shd,
      TokenKind::Assign => BindKind::Assign,
      _ => {
        return Err(
          ParseError::new(
            head.span,
            "binding pattern must start with `let`, `shd`, or `=`",
          )
          .expected("`[let name]`")
          .expected("`[shd name]`")
          .expected("`[= name]`"),
        )
      }
    };
    let name = self.expect_symbol("binding pattern requires a name")?;
    let close = self.expect(TokenKind::RBracket, "binding pattern must end with `]`")?;
    Ok(BindTarget {
      kind,
      name,
      span: open.span.start..close.span.end,
    })
  }

  fn parse_fixed_call_after_head(
    &mut self,
    start: usize,
    head_span: Span,
    name: String,
    mode: FormMode,
  ) -> Result<AST, ParseError> {
    let qualified = self.parse_qualified_identifier(name.clone(), head_span.clone())?;
    let (args, close) = self.parse_call_args(mode)?;
    let span = start..close.span.end;
    if qualified.is_none() && name == "Tuple" {
      return Ok(AST::new(ASTKind::NewTuple(args), span));
    }
    if let Some(((module, name), _)) = qualified {
      Ok(AST::new(
        ASTKind::CallFixed(Identifier::Qualified(module, name), args),
        span,
      ))
    } else {
      let callee = ast_from_variable_or_field_access(name, head_span);
      match callee.kind {
        ASTKind::Variable(name) => Ok(AST::new(
          ASTKind::CallFixed(Identifier::Bare(name), args),
          span,
        )),
        _ => Ok(AST::new(ASTKind::Call(Box::new(callee), args), span)),
      }
    }
  }

  fn parse_dynamic_call(&mut self, start: usize) -> Result<AST, ParseError> {
    let callee = self.parse_expr()?;
    let (args, close) = self.parse_call_args(FormMode::Bracket)?;
    let span = start..close.span.end;
    Ok(AST::new(ASTKind::Call(Box::new(callee), args), span))
  }

  fn parse_call_args(&mut self, mode: FormMode) -> Result<(Vec<AST>, Token), ParseError> {
    let mut args = Vec::new();
    let end = self.enter_form_body(mode, "call")?;
    while !self.at_form_end(end) {
      if matches!(self.peek().kind, TokenKind::Eof) {
        return Err(
          ParseError::new(self.peek().span.clone(), "unterminated call")
            .expected("an argument")
            .expected(form_end_expected(end)),
        );
      }
      args.push(self.parse_expr()?);
    }
    Ok((args, self.expect_form_end(end, "unterminated call")?))
  }

  fn enter_form_body(&mut self, mode: FormMode, form: &'static str) -> Result<FormEnd, ParseError> {
    match mode {
      FormMode::Bracket => Ok(FormEnd::RBracket),
      FormMode::Layout => self.enter_layout_body(form),
    }
  }

  fn enter_layout_body(&mut self, form: &'static str) -> Result<FormEnd, ParseError> {
    self.expect(TokenKind::Indent, layout_requires_indent_message(form))?;
    Ok(FormEnd::Dedent)
  }

  fn parse_nonempty_exprs_for_form(
    &mut self,
    mode: FormMode,
    form: &'static str,
  ) -> Result<(Vec<AST>, Token), ParseError> {
    let context = NonemptyExprContext::Form(form);
    let end = self.enter_form_body(mode, form)?;
    let expressions = self.parse_nonempty_exprs_until(end, context)?;
    let close = self.expect_form_end(end, nonempty_expr_eof_message(context))?;
    Ok((expressions, close))
  }

  fn parse_nonempty_exprs_until(
    &mut self,
    end: FormEnd,
    context: NonemptyExprContext,
  ) -> Result<Vec<AST>, ParseError> {
    self.skip_newlines();
    if self.at_form_end(end) {
      return Err(ParseError::new(
        self.peek().span.clone(),
        nonempty_expr_empty_message(context),
      ));
    }

    let mut expressions = Vec::new();
    while !self.at_form_end(end) {
      if matches!(self.peek().kind, TokenKind::Eof) {
        return Err(
          ParseError::new(self.peek().span.clone(), nonempty_expr_eof_message(context))
            .expected(nonempty_expr_eof_expected(context))
            .expected(form_end_expected(end)),
        );
      }
      expressions.push(self.parse_expr()?);
      if end == FormEnd::Dedent
        && !matches!(self.peek().kind, TokenKind::Dedent | TokenKind::Eof)
        && !matches!(
          self.tokens[self.current.saturating_sub(1)].kind,
          TokenKind::Newline | TokenKind::Dedent
        )
      {
        self.expect_layout_line_end("layout expressions must end at the end of the line")?;
      }
      self.skip_newlines();
    }
    Ok(expressions)
  }

  fn at_form_end(&self, end: FormEnd) -> bool {
    match end {
      FormEnd::RBracket => matches!(self.peek().kind, TokenKind::RBracket),
      FormEnd::Dedent => matches!(self.peek().kind, TokenKind::Dedent),
    }
  }

  fn expect_form_end(&mut self, end: FormEnd, message: &'static str) -> Result<Token, ParseError> {
    let token = self.advance();
    let matches = match end {
      FormEnd::RBracket => matches!(token.kind, TokenKind::RBracket),
      FormEnd::Dedent => matches!(token.kind, TokenKind::Dedent),
    };
    if matches {
      Ok(token)
    } else {
      Err(ParseError::new(token.span, message).expected(form_end_expected(end)))
    }
  }

  fn expect_layout_line_end(&mut self, message: &'static str) -> Result<Token, ParseError> {
    match self.peek().kind {
      TokenKind::Newline => Ok(self.advance()),
      TokenKind::Dedent | TokenKind::Eof => Ok(self.previous_token().clone()),
      _ => Err(ParseError::new(self.peek().span.clone(), message).expected("end of line")),
    }
  }

  fn finish_layout_item_line(
    &mut self,
    mode: FormMode,
    message: &'static str,
  ) -> Result<(), ParseError> {
    if mode == FormMode::Layout {
      if matches!(
        self.previous_token().kind,
        TokenKind::Dedent | TokenKind::Newline
      ) {
        return Ok(());
      }
      self.expect_layout_line_end(message)?;
    }
    Ok(())
  }

  fn parse_qualified_identifier(
    &mut self,
    module: String,
    start_span: Span,
  ) -> Result<Option<((String, String), Span)>, ParseError> {
    if self.check_token(TokenKind::DoubleColon).is_none() {
      return Ok(None);
    }
    let token = self.advance();
    match token.kind {
      TokenKind::Sym(name) => {
        let span = start_span.start..token.span.end;
        Ok(Some(((module, name), span)))
      }
      _ => {
        Err(ParseError::new(token.span, "`::` must be followed by a symbol").expected("a symbol"))
      }
    }
  }

  fn expect_symbol(&mut self, message: &'static str) -> Result<String, ParseError> {
    let token = self.advance();
    match token.kind {
      TokenKind::Sym(name) => Ok(name),
      kind => Err(
        ParseError::unexpected(
          Token {
            kind,
            span: token.span,
          },
          "a symbol",
        )
        .annotate(message),
      ),
    }
  }

  fn parse_type(&mut self) -> Result<TypeAst, ParseError> {
    self.enter_depth()?;
    let result = self.parse_type_inner();
    self.leave_depth();
    result
  }

  fn parse_type_inner(&mut self) -> Result<TypeAst, ParseError> {
    let token = self.advance();
    match token.kind {
      TokenKind::Sym(name) => {
        if let Some(((module, name), _)) =
          self.parse_qualified_identifier(name.clone(), token.span)?
        {
          Ok(TypeAst::Named(TypeNameAst::qualified(module, name)))
        } else {
          Ok(TypeAst::Named(TypeNameAst::bare(name)))
        }
      }
      TokenKind::LBracket => {
        let constructor = self.expect_symbol("type application requires a constructor name")?;
        if constructor == "Fn" {
          self.expect(
            TokenKind::LBracket,
            "`Fn` requires a bracketed parameter type list",
          )?;
          let (params, rest) = self.parse_fn_type_params()?;
          self.advance();
          self.expect(TokenKind::Arrow, "function type requires `->`")?;
          let ret = self.parse_type()?;
          self.expect(TokenKind::RBracket, "function type must end with `]`")?;
          Ok(TypeAst::Fn(params, rest.map(Box::new), Box::new(ret)))
        } else {
          let mut args = Vec::new();
          while !matches!(self.peek().kind, TokenKind::RBracket) {
            if matches!(self.peek().kind, TokenKind::Eof) {
              return Err(ParseError::new(
                self.peek().span.clone(),
                "unterminated type application",
              ));
            }
            args.push(self.parse_type()?);
          }
          self.advance();
          Ok(TypeAst::Apply(constructor, args))
        }
      }
      _ => Err(ParseError::new(token.span, "expected a type").expected("a type name")),
    }
  }

  fn parse_fn_type_params(&mut self) -> Result<(Vec<TypeAst>, Option<TypeAst>), ParseError> {
    let mut params = Vec::new();
    let mut rest = None;
    while !matches!(self.peek().kind, TokenKind::RBracket) {
      if matches!(self.peek().kind, TokenKind::Eof) {
        return Err(
          ParseError::new(
            self.peek().span.clone(),
            "unterminated function parameter type list",
          )
          .expected("a type")
          .expected("`]`"),
        );
      }
      if let Some(ellipsis) = self.check_token(TokenKind::Ellipsis) {
        let span = ellipsis.span;
        if rest.is_some() {
          return Err(ParseError::new(
            span,
            "function type can only have one rest parameter",
          ));
        }
        rest = Some(self.parse_type()?);
        if !matches!(self.peek().kind, TokenKind::RBracket) {
          return Err(ParseError::new(
            self.peek().span.clone(),
            "function rest parameter type must be last",
          ));
        }
        continue;
      }
      params.push(self.parse_type()?);
    }
    Ok((params, rest))
  }

  fn expect(&mut self, token_kind: TokenKind, message: &'static str) -> Result<Token, ParseError> {
    let token = self.advance();
    if token.kind == token_kind {
      Ok(token)
    } else {
      Err(ParseError::unexpected(token, format!("{token_kind}")).annotate(message))
    }
  }

  fn check_token(&mut self, token_kind: TokenKind) -> Option<Token> {
    (self.peek().kind == token_kind).then(|| self.advance())
  }

  fn peek(&self) -> &Token {
    &self.tokens[self.current]
  }

  fn skip_newlines(&mut self) {
    while self.check_token(TokenKind::Newline).is_some() {}
  }

  fn previous_token(&self) -> &Token {
    &self.tokens[self.current.saturating_sub(1)]
  }

  fn at_layout_line_start(&self) -> bool {
    self.current == 0
      || matches!(
        self.previous_token().kind,
        TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent
      )
  }

  fn starts_layout_form(&self, kind: &TokenKind) -> bool {
    if !is_form_head_token(kind) {
      return false;
    }
    matches!(kind, TokenKind::Block | TokenKind::Return) || self.has_more_tokens_on_current_line()
  }

  fn has_more_tokens_on_current_line(&self) -> bool {
    self.tokens[self.current..]
      .iter()
      .take_while(|token| {
        !matches!(
          token.kind,
          TokenKind::Newline | TokenKind::Dedent | TokenKind::Eof
        )
      })
      .next()
      .is_some()
  }

  fn advance(&mut self) -> Token {
    let token = self.peek().clone();
    if !matches!(token.kind, TokenKind::Eof) {
      self.current += 1;
    }
    token
  }
}

fn ast_from_variable_or_field_access(name: String, span: Span) -> AST {
  if !name.contains('.') || name.split('.').any(str::is_empty) {
    return AST::new(ASTKind::Variable(name.into()), span);
  }

  let mut parts = name.split('.');
  let first = parts
    .next()
    .expect("contains('.') ensures at least one component");
  let mut ast = AST::new(ASTKind::Variable(first.into()), span.clone());
  for field in parts {
    ast = AST::new(
      ASTKind::FieldAccess(Box::new(ast), field.to_string()),
      span.clone(),
    );
  }
  ast
}

fn implicit_branch_block(expressions: Vec<AST>) -> AST {
  if expressions.len() == 1 {
    return expressions
      .into_iter()
      .next()
      .expect("len() proves one expression exists");
  }

  let start = expressions
    .first()
    .expect("caller only builds branches from nonempty expression lists")
    .span
    .start;
  let end = expressions
    .last()
    .expect("caller only builds branches from nonempty expression lists")
    .span
    .end;
  AST::new(ASTKind::Block(expressions), start..end)
}

fn is_form_head_token(kind: &TokenKind) -> bool {
  matches!(
    kind,
    TokenKind::Let
      | TokenKind::Shd
      | TokenKind::Assign
      | TokenKind::Fn
      | TokenKind::Struct
      | TokenKind::Enum
      | TokenKind::New
      | TokenKind::Match
      | TokenKind::If
      | TokenKind::Block
      | TokenKind::Return
      | TokenKind::And
      | TokenKind::Or
      | TokenKind::For
      | TokenKind::Bind
  )
}

/// The fixed set of infix operators accepted inside parentheses, paired with
/// their binding precedence. Higher numbers bind tighter.
fn infix_precedence(op: &TokenKind) -> Option<u8> {
  match op {
    TokenKind::Sym(s) => match s.as_str() {
      "*" | "/" => Some(4),
      "+" | "-" => Some(3),
      "==" | "!=" | "<" | ">" | "<=" | ">=" => Some(2),
      _ => None,
    },
    TokenKind::And => Some(1),
    TokenKind::Or => Some(0),
    _ => None,
  }
}

fn layout_requires_indent_message(form: &'static str) -> &'static str {
  match form {
    "let" => "`let` layout body must be indented",
    "fn" => "`fn` layout body must be indented",
    "struct" => "`struct` layout body must be indented",
    "enum" => "`enum` layout body must be indented",
    "new" => "`new` layout body must be indented",
    "match" => "`match` layout body must be indented",
    "match arm" => "match arm body must be indented",
    "if" => "`if` then branch must be indented",
    "else" => "`else` branch must be indented",
    "block" => "`block` layout body must be indented",
    "for" => "`for` layout body must be indented",
    "call" => "layout call body must be indented",
    _ => "layout body must be indented",
  }
}

fn form_end_expected(end: FormEnd) -> &'static str {
  match end {
    FormEnd::RBracket => "`]`",
    FormEnd::Dedent => "dedent",
  }
}

fn nonempty_expr_empty_message(context: NonemptyExprContext) -> &'static str {
  match context {
    NonemptyExprContext::Form("fn") => "`fn` must have at least one body expression",
    NonemptyExprContext::Form("block") => "`block` must have at least one expression",
    NonemptyExprContext::Form(_) => "form must have at least one expression",
    NonemptyExprContext::IfThenBranch => "`if` then branch must have at least one expression",
    NonemptyExprContext::ElseBranch => "`else` branch must have at least one expression",
    NonemptyExprContext::MatchArmBody => "match arm body must have at least one expression",
  }
}

fn nonempty_expr_eof_message(context: NonemptyExprContext) -> &'static str {
  match context {
    NonemptyExprContext::Form("fn") => "unterminated `fn` form",
    NonemptyExprContext::Form("block") => "unterminated `block` form",
    NonemptyExprContext::Form(_) => "unterminated form",
    NonemptyExprContext::IfThenBranch => "unterminated `if` then branch",
    NonemptyExprContext::ElseBranch => "unterminated `else` branch",
    NonemptyExprContext::MatchArmBody => "unterminated match arm body",
  }
}

fn nonempty_expr_eof_expected(context: NonemptyExprContext) -> &'static str {
  match context {
    NonemptyExprContext::Form("fn") => "a body expression",
    NonemptyExprContext::Form(_)
    | NonemptyExprContext::ElseBranch
    | NonemptyExprContext::MatchArmBody => "an expression",
    NonemptyExprContext::IfThenBranch => "a then-branch expression",
  }
}

fn parse_internal(source: &str, max_depth: usize) -> Result<Vec<AST>, ParseError> {
  let tokens = Lexer::new(source).lex()?;
  Parser::new(tokens, max_depth).parse_multiple()
}

/// Parse `source` into top-level ASTs using the default maximum nesting depth.
///
/// Used by the test suites for convenience; the compiler itself goes through
/// [`read_multiple_with_depth`].
#[allow(dead_code)]
pub(crate) fn read_multiple(source: &str) -> Result<Vec<AST>, String> {
  read_multiple_with_depth(source, DEFAULT_MAX_PARSE_DEPTH)
}

/// Parse `source` into top-level ASTs using a caller-chosen maximum nesting
/// depth. Returns a rendered error string when the nesting exceeds `max_depth`
/// instead of risking a native stack overflow.
pub(crate) fn read_multiple_with_depth(source: &str, max_depth: usize) -> Result<Vec<AST>, String> {
  parse_internal(source, max_depth).map_err(|error| error.render(source))
}

#[cfg(test)]
mod parser_tests;
