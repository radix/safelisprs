use std::fmt;
use std::ops::Range;
use std::sync::atomic::{AtomicU64, Ordering};

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
      Some(TypeAst::Named(TypeNameAst::Qualified { .. })) => false,
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
  Qualified { module: String, name: String },
}

impl TypeNameAst {
  pub fn bare(name: impl Into<String>) -> Self {
    Self::Bare(name.into())
  }

  pub fn qualified(module: impl Into<String>, name: impl Into<String>) -> Self {
    Self::Qualified {
      module: module.into(),
      name: name.into(),
    }
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
      Self::Qualified { module, name } => write!(formatter, "{module}::{name}"),
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
  Sym(String),
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

#[derive(Debug, PartialEq)]
struct ParseError {
  span: Span,
  message: String,
  expected: Vec<&'static str>,
}

impl ParseError {
  fn new(span: Span, message: impl Into<String>) -> Self {
    Self {
      span,
      message: message.into(),
      expected: Vec::new(),
    }
  }

  fn expected(mut self, expected: &'static str) -> Self {
    self.expected.push(expected);
    self
  }

  fn render(&self, source: &str) -> String {
    let (line, column) = source_position(source, self.span.start);

    if self.expected.is_empty() {
      format!("line {line}, column {column}: {}", self.message)
    } else {
      format!(
        "line {line}, column {column}: {}; expected {}",
        self.message,
        self.expected.join(" or ")
      )
    }
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
  source: &'a str,
  offset: usize,
}

impl<'a> Lexer<'a> {
  fn new(source: &'a str) -> Self {
    Self { source, offset: 0 }
  }

  fn lex(mut self) -> Result<Vec<Token>, ParseError> {
    let mut tokens = Vec::new();

    loop {
      self.skip_ignored();
      if self.offset == self.source.len() {
        tokens.push(Token {
          kind: TokenKind::Eof,
          span: self.offset..self.offset,
        });
        return Ok(tokens);
      }

      let start = self.offset;
      let ch = self.peek_char().expect("offset is before end of source");
      let token = match ch {
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
        '.' if self.source[self.offset..].starts_with("...") => {
          self.bump_char();
          self.bump_char();
          self.bump_char();
          Token {
            kind: TokenKind::Sym("...".to_string()),
            span: start..self.offset,
          }
        }
        '"' => self.lex_string()?,
        _ => self.lex_value()?,
      };
      tokens.push(token);
    }
  }

  fn skip_ignored(&mut self) {
    loop {
      while self.peek_char().is_some_and(char::is_whitespace) {
        self.bump_char();
      }

      if self.peek_char() != Some('#') {
        return;
      }

      while let Some(ch) = self.bump_char() {
        if ch == '\n' {
          break;
        }
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
      && !self.source[self.offset..].starts_with("...")
    {
      self.bump_char();
    }

    let text = &self.source[start..self.offset];
    let span = start..self.offset;
    let kind = if is_numeric_candidate(text) {
      parse_number(text, span.clone())?
    } else {
      TokenKind::Sym(text.to_string())
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
    while !matches!(self.peek().kind, TokenKind::Eof) {
      result.push(self.parse_expr()?);
    }
    Ok(result)
  }

  fn parse_expr(&mut self) -> Result<AST, ParseError> {
    let token = self.advance();
    match token.kind {
      TokenKind::LParen => self.parse_list(token.span.start),
      TokenKind::RParen => {
        Err(ParseError::new(token.span, "unexpected `)`").expected("an expression"))
      }
      TokenKind::Colon | TokenKind::DoubleColon | TokenKind::Arrow => {
        Err(ParseError::new(token.span, "unexpected type-syntax token").expected("an expression"))
      }
      TokenKind::Sym(name) => {
        if let Some(((module, name), span)) =
          self.parse_qualified_identifier(name.clone(), token.span.clone())?
        {
          Ok(AST::new(ASTKind::FunctionRef(module, name), span))
        } else {
          Ok(ast_from_symbol(name, token.span))
        }
      }
      TokenKind::Int(value) => Ok(AST::new(ASTKind::Int(value), token.span)),
      TokenKind::Float(value) => Ok(AST::new(ASTKind::Float(value), token.span)),
      TokenKind::Str(value) => Ok(AST::new(ASTKind::String(value), token.span)),
      TokenKind::Eof => {
        Err(ParseError::new(token.span, "unexpected end of input").expected("an expression"))
      }
    }
  }

  fn parse_list(&mut self, start: usize) -> Result<AST, ParseError> {
    if matches!(self.peek().kind, TokenKind::RParen) {
      let close = self.advance();
      return Err(ParseError::new(start..close.span.end, "Empty call"));
    }
    if matches!(self.peek().kind, TokenKind::Eof) {
      return Err(
        ParseError::new(self.peek().span.clone(), "unexpected end of input")
          .expected("an expression")
          .expected("`)`"),
      );
    }

    match self.peek().kind.clone() {
      TokenKind::Sym(name) if name == "let" => {
        self.advance();
        self.parse_let(start)
      }
      TokenKind::Sym(name) if name == "fn" => {
        self.advance();
        self.parse_fn(start)
      }
      TokenKind::Sym(name) if name == "struct" => {
        self.advance();
        self.parse_struct(start)
      }
      TokenKind::Sym(name) if name == "enum" => {
        self.advance();
        self.parse_enum(start)
      }
      TokenKind::Sym(name) if name == "new" => {
        self.advance();
        self.parse_new(start)
      }
      TokenKind::Sym(name) if name == "match" => {
        self.advance();
        self.parse_match(start)
      }
      TokenKind::Sym(name) if name == "if" => {
        self.advance();
        self.parse_if(start)
      }
      TokenKind::Sym(name) if name == "block" => {
        self.advance();
        self.parse_block(start)
      }
      TokenKind::Sym(name) => self.parse_fixed_call(start, name),
      _ => self.parse_dynamic_call(start),
    }
  }

  fn parse_let(&mut self, start: usize) -> Result<AST, ParseError> {
    let variable = self.expect_symbol("first argument to `let` must be a symbol")?;
    let annotation = if matches!(self.peek().kind, TokenKind::Colon) {
      self.advance();
      Some(self.parse_type()?)
    } else {
      None
    };
    let expression = self.parse_expr()?;
    let close = self.expect_close("`let` must have exactly two arguments")?;
    let span = start..close.span.end;
    Ok(AST::new(
      ASTKind::Let(variable.into(), annotation, Box::new(expression)),
      span,
    ))
  }

  fn parse_fn(&mut self, start: usize) -> Result<AST, ParseError> {
    let name = self.expect_symbol("`fn` name must be a symbol")?;
    let params_open = self.advance();
    if !matches!(params_open.kind, TokenKind::LParen) {
      return Err(
        ParseError::new(params_open.span, "`fn` requires a parameter list").expected("`(`"),
      );
    }

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
      self.expect_colon("function parameters require a type annotation")?;
      params.push((param, Some(self.parse_type()?)));
    }
    self.advance();

    let return_type = if matches!(self.peek().kind, TokenKind::Arrow) {
      self.advance();
      Some(self.parse_type()?)
    } else {
      None
    };

    let mut bounds = Vec::new();
    if matches!(&self.peek().kind, TokenKind::Sym(name) if name == "where") {
      self.advance();
      self.expect_open("`where` requires a parenthesized bound list")?;
      while !matches!(self.peek().kind, TokenKind::RParen) {
        self.expect_open("each bound must be parenthesized")?;
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

    if matches!(self.peek().kind, TokenKind::RParen) {
      return Err(ParseError::new(
        self.peek().span.clone(),
        "`fn` must have at least one body expression",
      ));
    }

    let mut body = Vec::new();
    while !matches!(self.peek().kind, TokenKind::RParen) {
      if matches!(self.peek().kind, TokenKind::Eof) {
        return Err(
          ParseError::new(self.peek().span.clone(), "unterminated `fn` form")
            .expected("a body expression")
            .expected("`)`"),
        );
      }
      body.push(self.parse_expr()?);
    }
    let close = self.advance();
    let span = start..close.span.end;
    Ok(AST::new(
      ASTKind::DefineFn(Function {
        name: name.into(),
        params: params
          .into_iter()
          .map(|(name, annotation)| (name.into(), annotation))
          .collect(),
        return_type,
        bounds,
        code: body,
      }),
      span,
    ))
  }

  fn parse_struct(&mut self, start: usize) -> Result<AST, ParseError> {
    let name = self.expect_symbol("`struct` name must be a symbol")?;
    let mut fields = Vec::new();
    while !matches!(self.peek().kind, TokenKind::RParen) {
      if matches!(self.peek().kind, TokenKind::Eof) {
        return Err(
          ParseError::new(self.peek().span.clone(), "unterminated `struct` form")
            .expected("a field")
            .expected("`)`"),
        );
      }
      let field = self.expect_symbol("struct field names must be symbols")?;
      self.expect_colon("struct fields require a type annotation")?;
      fields.push((field, self.parse_type()?));
    }
    let close = self.advance();
    let span = start..close.span.end;
    Ok(AST::new(
      ASTKind::DefineStruct(Struct { name, fields }),
      span,
    ))
  }

  fn parse_enum(&mut self, start: usize) -> Result<AST, ParseError> {
    let name = self.expect_symbol("`enum` name must be a symbol")?;
    let mut variants = Vec::new();
    while !matches!(self.peek().kind, TokenKind::RParen) {
      if matches!(self.peek().kind, TokenKind::Eof) {
        return Err(
          ParseError::new(self.peek().span.clone(), "unterminated `enum` form")
            .expected("a variant")
            .expected("`)`"),
        );
      }
      self.expect_open("enum variants must be parenthesized")?;
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
        self.expect_colon("enum variant fields require a type annotation")?;
        fields.push((field, self.parse_type()?));
      }
      self.advance();
      variants.push(EnumVariant {
        name: variant,
        fields,
      });
    }
    let close = self.advance();
    let span = start..close.span.end;
    Ok(AST::new(ASTKind::DefineEnum(Enum { name, variants }), span))
  }

  fn parse_new(&mut self, start: usize) -> Result<AST, ParseError> {
    let name = self.expect_symbol("`new` requires a struct name")?;
    let variant = if matches!(self.peek().kind, TokenKind::DoubleColon) {
      self.advance();
      Some(self.expect_symbol("enum construction requires a variant name after `::`")?)
    } else {
      None
    };
    let mut fields = Vec::new();
    while !matches!(self.peek().kind, TokenKind::RParen) {
      if matches!(self.peek().kind, TokenKind::Eof) {
        return Err(
          ParseError::new(self.peek().span.clone(), "unterminated `new` form")
            .expected("a field initializer")
            .expected("`)`"),
        );
      }
      let field = self.expect_symbol("struct initializer field names must be symbols")?;
      self.expect_colon("struct initializer fields require `:`")?;
      fields.push((field, self.parse_expr()?));
    }
    let close = self.advance();
    let span = start..close.span.end;
    if let Some(variant) = variant {
      Ok(AST::new(ASTKind::NewEnum(name, variant, fields), span))
    } else {
      Ok(AST::new(ASTKind::NewStruct(name, fields), span))
    }
  }

  fn parse_match(&mut self, start: usize) -> Result<AST, ParseError> {
    let scrutinee = self.parse_expr()?;
    let mut arms = Vec::new();
    while !matches!(self.peek().kind, TokenKind::RParen) {
      if matches!(self.peek().kind, TokenKind::Eof) {
        return Err(
          ParseError::new(self.peek().span.clone(), "unterminated `match` form")
            .expected("a match arm")
            .expected("`)`"),
        );
      }
      arms.push(self.parse_match_arm()?);
    }
    if arms.is_empty() {
      return Err(ParseError::new(
        self.peek().span.clone(),
        "`match` must have at least one arm",
      ));
    }
    let close = self.advance();
    let span = start..close.span.end;
    Ok(AST::new(ASTKind::Match(Box::new(scrutinee), arms), span))
  }

  fn parse_match_arm(&mut self) -> Result<MatchArm, ParseError> {
    let pattern = if matches!(self.peek().kind, TokenKind::LParen) {
      self.advance();
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
          self.previous_span(),
          "default match arm must use `_`",
        ));
      }
      MatchPattern::Default
    };
    self.expect_arrow_arm()?;
    let body = self.parse_expr()?;
    Ok(MatchArm { pattern, body })
  }

  fn parse_if(&mut self, start: usize) -> Result<AST, ParseError> {
    let condition = self.parse_expr()?;
    let then_branch = self.parse_expr()?;
    let else_branch = self.parse_expr()?;
    let close = self.expect_close("`if` must have exactly three arguments: cond, then, else")?;
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

  fn parse_block(&mut self, start: usize) -> Result<AST, ParseError> {
    if matches!(self.peek().kind, TokenKind::RParen) {
      return Err(ParseError::new(
        self.peek().span.clone(),
        "`block` must have at least one expression",
      ));
    }

    let mut expressions = Vec::new();
    while !matches!(self.peek().kind, TokenKind::RParen) {
      if matches!(self.peek().kind, TokenKind::Eof) {
        return Err(
          ParseError::new(self.peek().span.clone(), "unterminated `block` form")
            .expected("an expression")
            .expected("`)`"),
        );
      }
      expressions.push(self.parse_expr()?);
    }
    let close = self.advance();
    let span = start..close.span.end;
    Ok(AST::new(ASTKind::Block(expressions), span))
  }

  fn parse_fixed_call(&mut self, start: usize, name: String) -> Result<AST, ParseError> {
    let head = self.advance();
    let qualified = self.parse_qualified_identifier(name.clone(), head.span.clone())?;
    let (args, close) = self.parse_call_args()?;
    let span = start..close.span.end;
    if let Some(((module, name), _)) = qualified {
      Ok(AST::new(
        ASTKind::CallFixed(Identifier::Qualified(module, name), args),
        span,
      ))
    } else {
      let callee = ast_from_symbol(name, head.span);
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
    let (args, close) = self.parse_call_args()?;
    let span = start..close.span.end;
    Ok(AST::new(ASTKind::Call(Box::new(callee), args), span))
  }

  fn parse_call_args(&mut self) -> Result<(Vec<AST>, Token), ParseError> {
    let mut args = Vec::new();
    while !matches!(self.peek().kind, TokenKind::RParen) {
      if matches!(self.peek().kind, TokenKind::Eof) {
        return Err(
          ParseError::new(self.peek().span.clone(), "unterminated call")
            .expected("an argument")
            .expected("`)`"),
        );
      }
      args.push(self.parse_expr()?);
    }
    Ok((args, self.advance()))
  }

  fn parse_qualified_identifier(
    &mut self,
    module: String,
    start_span: Span,
  ) -> Result<Option<((String, String), Span)>, ParseError> {
    if !matches!(self.peek().kind, TokenKind::DoubleColon) {
      return Ok(None);
    }
    self.advance();
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
      _ => Err(ParseError::new(token.span, message).expected("a symbol")),
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
          self.expect_open("`Fn` requires a parenthesized parameter type list")?;
          let (params, rest) = self.parse_fn_type_params()?;
          self.advance();
          let arrow = self.advance();
          if !matches!(arrow.kind, TokenKind::Arrow) {
            return Err(
              ParseError::new(arrow.span, "function type requires `->`").expected("`->`"),
            );
          }
          let ret = self.parse_type()?;
          self.expect_close("function type must end with `)`")?;
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
      if let TokenKind::Sym(name) = self.peek().kind.clone() {
        if name == "..." {
          let span = self.advance().span;
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
      }
      params.push(self.parse_type()?);
    }
    Ok((params, rest))
  }

  fn expect_open(&mut self, message: &'static str) -> Result<Token, ParseError> {
    let token = self.advance();
    if matches!(token.kind, TokenKind::LParen) {
      Ok(token)
    } else {
      Err(ParseError::new(token.span, message).expected("`(`"))
    }
  }

  fn expect_colon(&mut self, message: &'static str) -> Result<Token, ParseError> {
    let token = self.advance();
    if matches!(token.kind, TokenKind::Colon) {
      Ok(token)
    } else {
      Err(ParseError::new(token.span, message).expected("`:`"))
    }
  }

  fn expect_close(&mut self, message: &'static str) -> Result<Token, ParseError> {
    let token = self.advance();
    if matches!(token.kind, TokenKind::RParen) {
      Ok(token)
    } else {
      Err(ParseError::new(token.span, message).expected("`)`"))
    }
  }

  fn expect_arrow_arm(&mut self) -> Result<Token, ParseError> {
    let token = self.advance();
    match token.kind {
      TokenKind::Sym(ref separator) if separator == "=>" => Ok(token),
      _ => Err(ParseError::new(token.span, "match arms require `=>`").expected("`=>`")),
    }
  }

  fn peek(&self) -> &Token {
    &self.tokens[self.current]
  }

  fn previous_span(&self) -> Span {
    self.tokens[self.current.saturating_sub(1)].span.clone()
  }

  fn advance(&mut self) -> Token {
    let token = self.peek().clone();
    if !matches!(token.kind, TokenKind::Eof) {
      self.current += 1;
    }
    token
  }
}

fn ast_from_symbol(name: String, span: Span) -> AST {
  let kind = match name.as_str() {
    "true" => ASTKind::Bool(true),
    "false" => ASTKind::Bool(false),
    _ => return ast_from_variable_or_field_access(name, span),
  };
  AST::new(kind, span)
}

fn ast_from_variable_or_field_access(name: String, span: Span) -> AST {
  if name == "..." || !name.contains('.') || name.split('.').any(str::is_empty) {
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

fn parse_internal(source: &str) -> Result<Vec<AST>, ParseError> {
  let tokens = Lexer::new(source).lex()?;
  Parser::new(tokens).parse_multiple()
}

pub fn read_multiple(source: &str) -> Result<Vec<AST>, String> {
  parse_internal(source).map_err(|error| error.render(source))
}

#[cfg(test)]
mod parser_tests;
