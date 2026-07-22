use std::fmt;
use std::ops::Range;
use std::sync::atomic::{AtomicU64, Ordering};

use crate::types::QualifiedTypeName;

pub type Span = Range<usize>;

/// Stable identity for one AST node across compiler passes.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct AstId(u64);

static NEXT_AST_ID: AtomicU64 = AtomicU64::new(0);

impl AstId {
  fn fresh() -> Self {
    Self(NEXT_AST_ID.fetch_add(1, Ordering::Relaxed))
  }
}

/// Stable identity for one lexical binding within a resolved module.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct BindingId(u32);

impl BindingId {
  const UNRESOLVED: Self = Self(u32::MAX);

  pub(crate) fn resolved(index: u32) -> Self {
    Self(index)
  }

  pub(crate) fn synthetic(index: u32) -> Self {
    Self(u32::MAX - 1 - index)
  }

  pub fn is_resolved(self) -> bool {
    self != Self::UNRESOLVED
  }
}

/// A source name paired with the lexical binding it resolves to.
///
/// The parser creates unresolved names; [`crate::prelude::resolve_module_names`]
/// assigns binding IDs before typechecking and lowering.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct ResolvedName {
  pub name: String,
  pub binding: BindingId,
}

impl ResolvedName {
  pub fn unresolved(name: impl Into<String>) -> Self {
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

  pub fn as_str(&self) -> &str {
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
pub struct AST {
  id: AstId,
  pub kind: ASTKind,
  pub span: Span,
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
pub enum ASTKind {
  Let(ResolvedName, Option<TypeAst>, Box<AST>),
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
  NewStruct(String, Vec<(String, AST)>),
  NewEnum(String, String, Vec<(String, AST)>),
  FieldAccess(Box<AST>, String),
  Match(Box<AST>, Vec<MatchArm>),

  /// Bind up some arguments with a callable. This is used for closure captures.
  /// Not represented directly in source syntax.
  PartialApply(Box<AST>, Vec<AST>),
  /// Get a reference to a function. Source syntax is `module::function`.
  FunctionRef(String, String),
  /// Conditional: evaluate `cond`; if truthy, evaluate `then`, else evaluate `els`.
  If(Box<AST>, Box<AST>, Box<AST>),
  /// A sequence: evaluate each sub-expression in order, discarding all but the
  /// last, and return the last. Lets a single-expression position (e.g. an `if`
  /// branch) evaluate multiple expressions for side effects.
  Block(Vec<AST>),
}

impl AST {
  pub fn new(kind: ASTKind, span: Span) -> Self {
    Self {
      id: AstId::fresh(),
      kind,
      span,
    }
  }

  pub fn id(&self) -> AstId {
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
    ASTKind::NewStruct(name, fields) => ASTKind::NewStruct(
      name.clone(),
      fields
        .iter()
        .map(|(field, expression)| Ok((field.clone(), map(expression)?)))
        .collect::<Result<_, _>>()?,
    ),
    ASTKind::NewEnum(name, variant, fields) => ASTKind::NewEnum(
      name.clone(),
      variant.clone(),
      fields
        .iter()
        .map(|(field, expression)| Ok((field.clone(), map(expression)?)))
        .collect::<Result<_, _>>()?,
    ),
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
      Box::new(map(else_branch)?),
    ),
    ASTKind::Block(body) => ASTKind::Block(body.iter().map(&mut map).collect::<Result<_, _>>()?),
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
      ASTKind::Let(name, _, expression) => {
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
      ASTKind::NewStruct(_, fields) => {
        for (_, expression) in fields {
          erase_ast(expression);
        }
      }
      ASTKind::NewEnum(_, _, fields) => {
        for (_, expression) in fields {
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
        erase_ast(else_branch);
      }
      ASTKind::Block(body) => {
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
pub enum Identifier {
  Bare(ResolvedName),
  Qualified(String, String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
  pub name: ResolvedName,
  pub params: Vec<(ResolvedName, Option<TypeAst>)>,
  pub return_type: Option<TypeAst>,
  pub bounds: Vec<Bound>,
  pub code: Vec<AST>,
}

impl Function {
  pub fn returns_void(&self) -> bool {
    match &self.return_type {
      None => true,
      Some(TypeAst::Named(TypeNameAst::Bare(name))) => name == "Void",
      Some(TypeAst::Named(TypeNameAst::Qualified(_))) => false,
      Some(TypeAst::Apply(_, _) | TypeAst::Fn(_, _, _)) => false,
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Struct {
  pub name: String,
  pub fields: Vec<(String, TypeAst)>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Enum {
  pub name: String,
  pub variants: Vec<EnumVariant>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnumVariant {
  pub name: String,
  pub fields: Vec<(String, TypeAst)>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct MatchArm {
  pub pattern: MatchPattern,
  pub body: AST,
}

#[derive(Debug, PartialEq, Clone)]
pub enum MatchPattern {
  Variant {
    variant: String,
    fields: Vec<ResolvedName>,
  },
  Default,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeNameAst {
  Bare(String),
  Qualified(QualifiedTypeName),
}

impl TypeNameAst {
  pub fn bare(name: impl Into<String>) -> Self {
    Self::Bare(name.into())
  }

  pub fn qualified(module: impl Into<String>, name: impl Into<String>) -> Self {
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
pub enum TypeAst {
  Named(TypeNameAst),
  Apply(String, Vec<TypeAst>),
  Fn(Vec<TypeAst>, Option<Box<TypeAst>>, Box<TypeAst>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Bound {
  pub var: String,
  pub traits: Vec<String>,
}

#[derive(Debug, PartialEq, Clone)]
enum TokenKind {
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
  Fn,
  Struct,
  Enum,
  New,
  Match,
  If,
  Else,
  Block,
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
      TokenKind::Fn => write!(formatter, "fn"),
      TokenKind::Struct => write!(formatter, "struct"),
      TokenKind::Enum => write!(formatter, "enum"),
      TokenKind::New => write!(formatter, "new"),
      TokenKind::Match => write!(formatter, "match"),
      TokenKind::If => write!(formatter, "if"),
      TokenKind::Else => write!(formatter, "else"),
      TokenKind::Block => write!(formatter, "block"),
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
  paren_depth: usize,
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
      paren_depth: 0,
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
        TokenKind::Newline if self.paren_depth == 0 => self.finish_line(Some(token.span)),
        TokenKind::Newline => {}
        TokenKind::LParen => {
          self.push_line_token(token)?;
          self.paren_depth += 1;
        }
        TokenKind::RParen => {
          self.push_line_token(token)?;
          self.paren_depth = self.paren_depth.saturating_sub(1);
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
    "fn" => TokenKind::Fn,
    "struct" => TokenKind::Struct,
    "enum" => TokenKind::Enum,
    "new" => TokenKind::New,
    "match" => TokenKind::Match,
    "if" => TokenKind::If,
    "else" => TokenKind::Else,
    "block" => TokenKind::Block,
    "where" => TokenKind::Where,
    "true" => TokenKind::Bool(true),
    "false" => TokenKind::Bool(false),
    _ => TokenKind::Sym(text.to_string()),
  }
}

fn is_delimiter(ch: char) -> bool {
  ch.is_whitespace() || matches!(ch, '(' | ')' | '"' | '#' | ':')
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
  Paren,
  Layout,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum FormEnd {
  RParen,
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
}

impl Parser {
  fn new(tokens: Vec<Token>) -> Self {
    Self { tokens, current: 0 }
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
    let layout_line_start = self.at_layout_line_start();
    let token = self.advance();
    match token.kind {
      TokenKind::LParen => self.parse_list(token.span.start, FormMode::Paren),
      kind @ (TokenKind::Let
      | TokenKind::Fn
      | TokenKind::Struct
      | TokenKind::Enum
      | TokenKind::New
      | TokenKind::Match
      | TokenKind::If
      | TokenKind::Block)
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

  fn parse_list(&mut self, start: usize, mode: FormMode) -> Result<AST, ParseError> {
    if let Some(close) = self.check_token(TokenKind::RParen) {
      return Err(ParseError::new(start..close.span.end, "Empty call"));
    }
    if matches!(self.peek().kind, TokenKind::Eof) {
      return Err(
        ParseError::new(self.peek().span.clone(), "unexpected end of input")
          .expected("an expression")
          .expected("`)`"),
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
      TokenKind::Fn => self.parse_fn(start, mode),
      TokenKind::Struct => self.parse_struct(start, mode),
      TokenKind::Enum => self.parse_enum(start, mode),
      TokenKind::New => self.parse_new(start, mode),
      TokenKind::Match => self.parse_match(start, mode),
      TokenKind::If => self.parse_if(start, mode),
      TokenKind::Block => self.parse_block(start, mode),
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
      FormMode::Paren => {
        self.expect_form_end(FormEnd::RParen, "`let` must have exactly two arguments")?
      }
      FormMode::Layout => self.expect_layout_line_end("`let` must have exactly two arguments")?,
    };
    let span = start..close.span.end;
    Ok(AST::new(
      ASTKind::Let(variable.into(), annotation, Box::new(expression)),
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
    self.expect(TokenKind::LParen, "`fn` requires a parameter list")?;

    let mut params = Vec::new();
    while !matches!(self.peek().kind, TokenKind::RParen) {
      if matches!(self.peek().kind, TokenKind::Eof) {
        return Err(
          ParseError::new(self.peek().span.clone(), "unterminated parameter list")
            .expected("a parameter name")
            .expected("`)`"),
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
        TokenKind::LParen,
        "`where` requires a parenthesized bound list",
      )?;
      while !matches!(self.peek().kind, TokenKind::RParen) {
        self.expect(TokenKind::LParen, "each bound must be parenthesized")?;
        let var = self.expect_symbol("a bound must name a type variable")?;
        let mut traits = Vec::new();
        while !matches!(self.peek().kind, TokenKind::RParen) {
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
      self.expect(TokenKind::LParen, "enum variants must be parenthesized")?;
      let variant = self.expect_symbol("enum variant names must be symbols")?;
      let mut fields = Vec::new();
      while !matches!(self.peek().kind, TokenKind::RParen) {
        if matches!(self.peek().kind, TokenKind::Eof) {
          return Err(
            ParseError::new(self.peek().span.clone(), "unterminated enum variant")
              .expected("a field")
              .expected("`)`"),
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
    let name = self.expect_symbol("`new` requires a struct name")?;
    let variant = if self.check_token(TokenKind::DoubleColon).is_some() {
      Some(self.expect_symbol("enum construction requires a variant name after `::`")?)
    } else {
      None
    };
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
    if let Some(variant) = variant {
      Ok(AST::new(ASTKind::NewEnum(name, variant, fields), span))
    } else {
      Ok(AST::new(ASTKind::NewStruct(name, fields), span))
    }
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
    let pattern = if self.check_token(TokenKind::LParen).is_some() {
      let variant = self.expect_symbol("match variant patterns must name a variant")?;
      let mut fields = Vec::new();
      while !matches!(self.peek().kind, TokenKind::RParen) {
        if matches!(self.peek().kind, TokenKind::Eof) {
          return Err(
            ParseError::new(self.peek().span.clone(), "unterminated match pattern")
              .expected("a binding name")
              .expected("`)`"),
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
      FormMode::Paren => {
        let then_branch = self.parse_expr()?;
        let else_branch = self.parse_expr()?;
        let close = self.expect_form_end(
          FormEnd::RParen,
          "`if` must have exactly three arguments: cond, then, else",
        )?;
        (then_branch, else_branch, close)
      }
      FormMode::Layout => {
        let then_end = self.enter_layout_body("if")?;
        let then_exprs =
          self.parse_nonempty_exprs_until(then_end, NonemptyExprContext::IfThenBranch)?;
        self.expect_form_end(
          then_end,
          nonempty_expr_eof_message(NonemptyExprContext::IfThenBranch),
        )?;

        self.expect(TokenKind::Else, "`if` layout requires an `else` clause")?;

        let (else_branch, close) = self.parse_layout_else_branch()?;
        (implicit_branch_block(then_exprs), else_branch, close)
      }
    };
    let span = start..close.span.end;
    Ok(AST::new(
      ASTKind::If(
        Box::new(condition),
        Box::new(then_branch),
        Box::new(else_branch),
      ),
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
    let (args, close) = self.parse_call_args(FormMode::Paren)?;
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
      FormMode::Paren => Ok(FormEnd::RParen),
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
      FormEnd::RParen => matches!(self.peek().kind, TokenKind::RParen),
      FormEnd::Dedent => matches!(self.peek().kind, TokenKind::Dedent),
    }
  }

  fn expect_form_end(&mut self, end: FormEnd, message: &'static str) -> Result<Token, ParseError> {
    let token = self.advance();
    let matches = match end {
      FormEnd::RParen => matches!(token.kind, TokenKind::RParen),
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
      TokenKind::LParen => {
        let constructor = self.expect_symbol("type application requires a constructor name")?;
        if constructor == "Fn" {
          self.expect(
            TokenKind::LParen,
            "`Fn` requires a parenthesized parameter type list",
          )?;
          let (params, rest) = self.parse_fn_type_params()?;
          self.advance();
          self.expect(TokenKind::Arrow, "function type requires `->`")?;
          let ret = self.parse_type()?;
          self.expect(TokenKind::RParen, "function type must end with `)`")?;
          Ok(TypeAst::Fn(params, rest.map(Box::new), Box::new(ret)))
        } else {
          let mut args = Vec::new();
          while !matches!(self.peek().kind, TokenKind::RParen) {
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
    while !matches!(self.peek().kind, TokenKind::RParen) {
      if matches!(self.peek().kind, TokenKind::Eof) {
        return Err(
          ParseError::new(
            self.peek().span.clone(),
            "unterminated function parameter type list",
          )
          .expected("a type")
          .expected("`)`"),
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
        if !matches!(self.peek().kind, TokenKind::RParen) {
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
    matches!(kind, TokenKind::Block) || self.has_more_tokens_on_current_line()
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
      | TokenKind::Fn
      | TokenKind::Struct
      | TokenKind::Enum
      | TokenKind::New
      | TokenKind::Match
      | TokenKind::If
      | TokenKind::Block
  )
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
    "call" => "layout call body must be indented",
    _ => "layout body must be indented",
  }
}

fn form_end_expected(end: FormEnd) -> &'static str {
  match end {
    FormEnd::RParen => "`)`",
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

fn parse_internal(source: &str) -> Result<Vec<AST>, ParseError> {
  let tokens = Lexer::new(source).lex()?;
  Parser::new(tokens).parse_multiple()
}

pub fn read_multiple(source: &str) -> Result<Vec<AST>, String> {
  parse_internal(source).map_err(|error| error.render(source))
}

#[cfg(test)]
mod parser_tests;
