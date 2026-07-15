use std::ops::Range;

pub type Span = Range<usize>;

#[derive(Debug, Clone)]
pub struct AST {
  pub kind: ASTKind,
  pub span: Span,
}

impl PartialEq for AST {
  fn eq(&self, other: &Self) -> bool {
    // Source location is metadata, not part of the program's semantics.
    self.kind == other.kind
  }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ASTKind {
  Let(String, Option<TypeAst>, Box<AST>),
  DefineFn(Function),
  DefineStruct(Struct),
  Call(Box<AST>, Vec<AST>),
  CallFixed(Identifier, Vec<AST>),
  Variable(String),
  Int(i64),
  Float(f64),
  String(String),
  Bool(bool),
  NewStruct(String, Vec<(String, AST)>),
  FieldAccess(Box<AST>, String),

  // The following variants aren't represented in the syntax, but are produced
  // by transformations on the previous variants.
  /// Bind up some arguments with a callable. This is used for closure captures.
  PartialApply(Box<AST>, Vec<AST>),
  /// Get a reference to a function.
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
    Self { kind, span }
  }

  pub(crate) fn synthetic(kind: ASTKind) -> Self {
    Self::new(kind, 0..0)
  }

  pub(crate) fn with_kind(&self, kind: ASTKind) -> Self {
    Self::new(kind, self.span.clone())
  }
}

#[cfg(test)]
#[allow(non_snake_case)]
impl AST {
  pub(crate) fn Let(name: String, value: Box<AST>) -> Self {
    Self::synthetic(ASTKind::Let(name, None, value))
  }

  pub(crate) fn DefineFn(function: Function) -> Self {
    Self::synthetic(ASTKind::DefineFn(function))
  }

  pub(crate) fn CallFixed(identifier: Identifier, args: Vec<AST>) -> Self {
    Self::synthetic(ASTKind::CallFixed(identifier, args))
  }

  pub(crate) fn Variable(name: String) -> Self {
    Self::synthetic(ASTKind::Variable(name))
  }

  pub(crate) fn Int(value: i64) -> Self {
    Self::synthetic(ASTKind::Int(value))
  }

  pub(crate) fn Float(value: f64) -> Self {
    Self::synthetic(ASTKind::Float(value))
  }

  pub(crate) fn String(value: String) -> Self {
    Self::synthetic(ASTKind::String(value))
  }

  pub(crate) fn PartialApply(callable: Box<AST>, args: Vec<AST>) -> Self {
    Self::synthetic(ASTKind::PartialApply(callable, args))
  }

  pub(crate) fn FunctionRef(module: String, name: String) -> Self {
    Self::synthetic(ASTKind::FunctionRef(module, name))
  }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Identifier {
  Bare(String),
  Qualified(String, String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
  pub name: String,
  pub params: Vec<(String, Option<TypeAst>)>,
  pub return_type: Option<TypeAst>,
  pub bounds: Vec<Bound>,
  pub code: Vec<AST>,
}

impl Function {
  pub fn returns_void(&self) -> bool {
    match &self.return_type {
      None => true,
      Some(TypeAst::Named(name)) => name == "Void",
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
pub enum TypeAst {
  Named(String),
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
      TokenKind::Colon | TokenKind::Arrow => {
        Err(ParseError::new(token.span, "unexpected type-syntax token").expected("an expression"))
      }
      TokenKind::Sym(name) => Ok(ast_from_symbol(name, token.span)),
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

    match &self.peek().kind {
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
      TokenKind::Sym(name) if name == "new" => {
        self.advance();
        self.parse_new(start)
      }
      TokenKind::Sym(name) if name == "if" => {
        self.advance();
        self.parse_if(start)
      }
      TokenKind::Sym(name) if name == "block" => {
        self.advance();
        self.parse_block(start)
      }
      TokenKind::Sym(_) => self.parse_fixed_call(start),
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
      ASTKind::Let(variable, annotation, Box::new(expression)),
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
        name,
        params,
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

  fn parse_new(&mut self, start: usize) -> Result<AST, ParseError> {
    let name = self.expect_symbol("`new` requires a struct name")?;
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
    Ok(AST::new(ASTKind::NewStruct(name, fields), span))
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

  fn parse_fixed_call(&mut self, start: usize) -> Result<AST, ParseError> {
    let head = self.advance();
    let TokenKind::Sym(name) = head.kind else {
      unreachable!("fixed calls have symbol heads")
    };
    let identifier = parse_identifier(&name);
    let (args, close) = self.parse_call_args()?;
    let span = start..close.span.end;
    Ok(AST::new(ASTKind::CallFixed(identifier, args), span))
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
      TokenKind::Sym(name) => Ok(TypeAst::Named(name)),
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

  fn peek(&self) -> &Token {
    &self.tokens[self.current]
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
    _ => match parse_identifier(&name) {
      Identifier::Bare(name) => ASTKind::Variable(name),
      Identifier::Qualified(module, name) => ASTKind::FunctionRef(module, name),
    },
  };
  AST::new(kind, span)
}

fn parse_identifier(name: &str) -> Identifier {
  if name == "..." {
    return Identifier::Bare(name.to_string());
  }
  match name.split_once('.') {
    Some((module, name)) => Identifier::Qualified(module.to_string(), name.to_string()),
    None => Identifier::Bare(name.to_string()),
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
mod test {
  use super::*;

  #[test]
  fn test_read_multiple() {
    let result = read_multiple("5 3").unwrap();
    assert_eq!(result, vec![AST::Int(5), AST::Int(3)]);
  }

  #[test]
  fn dotted_symbol_in_value_position_is_a_function_ref() {
    let result = read_multiple("std.len").unwrap();
    assert_eq!(
      result,
      vec![AST::FunctionRef("std".to_string(), "len".to_string())]
    );
  }

  #[test]
  fn dotted_symbol_in_call_head_is_a_qualified_fixed_call() {
    let result = read_multiple("(std.+ 1 2)").unwrap();
    assert_eq!(
      result,
      vec![AST::CallFixed(
        Identifier::Qualified("std".to_string(), "+".to_string()),
        vec![AST::Int(1), AST::Int(2)],
      )]
    );
  }

  #[test]
  fn set_is_parsed_as_an_ordinary_call() {
    let result = read_multiple("(set! x 2)").unwrap();
    assert_eq!(
      result,
      vec![AST::CallFixed(
        Identifier::Bare("set!".to_string()),
        vec![AST::Variable("x".to_string()), AST::Int(2)],
      )]
    );
  }

  #[test]
  fn parses_negative_numbers_and_operator_symbols() {
    assert_eq!(
      read_multiple("-12 +3 -1.25 +2.5 - +").unwrap(),
      vec![
        AST::Int(-12),
        AST::Int(3),
        AST::Float(-1.25),
        AST::Float(2.5),
        AST::Variable("-".to_string()),
        AST::Variable("+".to_string()),
      ]
    );
  }

  #[test]
  fn parses_nested_functions() {
    let result = read_multiple("(fn outer (x:Int) (fn inner (y:Int) ->Int (std.+ x y)) inner)");
    assert!(result.is_ok(), "got: {result:?}");
  }

  #[test]
  fn parses_mandatory_parameter_and_return_types_without_whitespace() {
    let asts = read_multiple("(fn id (a:A xs:(List Int))->A where ((A Eq)) a)").unwrap();
    let ASTKind::DefineFn(function) = &asts[0].kind else {
      panic!("expected function")
    };
    assert_eq!(
      function.params,
      vec![
        ("a".to_string(), Some(TypeAst::Named("A".to_string()))),
        (
          "xs".to_string(),
          Some(TypeAst::Apply(
            "List".to_string(),
            vec![TypeAst::Named("Int".to_string())]
          ))
        )
      ]
    );
    assert_eq!(function.return_type, Some(TypeAst::Named("A".to_string())));
    assert_eq!(
      function.bounds,
      vec![Bound {
        var: "A".to_string(),
        traits: vec!["Eq".to_string()]
      }]
    );
  }

  #[test]
  fn parses_function_types_and_annotated_lets() {
    let asts = read_multiple(
      "(fn apply (f:(Fn (Int) -> Int) x:Int) ->Int
         (let result:Int (f x)))",
    )
    .unwrap();
    let ASTKind::DefineFn(function) = &asts[0].kind else {
      panic!("expected function")
    };
    assert_eq!(
      function.params[0].1,
      Some(TypeAst::Fn(
        vec![TypeAst::Named("Int".to_string())],
        None,
        Box::new(TypeAst::Named("Int".to_string()))
      ))
    );
    assert!(matches!(function.code[0].kind, ASTKind::Let(_, Some(_), _)));
  }

  #[test]
  fn parses_variadic_function_type() {
    let asts = read_multiple("(fn use-list (make:(Fn (...Int) -> (List Int))) ->Int 1)").unwrap();
    let ASTKind::DefineFn(function) = &asts[0].kind else {
      panic!("expected function")
    };
    assert_eq!(
      function.params[0].1,
      Some(TypeAst::Fn(
        vec![],
        Some(Box::new(TypeAst::Named("Int".to_string()))),
        Box::new(TypeAst::Apply(
          "List".to_string(),
          vec![TypeAst::Named("Int".to_string())]
        ))
      ))
    );
  }

  #[test]
  fn ellipsis_is_lexed_as_its_own_symbol() {
    assert_eq!(
      read_multiple("...Int").unwrap(),
      vec![
        AST::Variable("...".to_string()),
        AST::Variable("Int".to_string())
      ]
    );
  }

  #[test]
  fn rejects_unannotated_parameters() {
    let error = read_multiple("(fn id (a) a)").unwrap_err();
    assert!(error.contains("require a type annotation"), "{error}");
  }

  #[test]
  fn strings_are_unescaped_with_unescape_crate() {
    assert_eq!(
      read_multiple(r#""line\nquote: \" slash: \\""#).unwrap(),
      vec![AST::String("line\nquote: \" slash: \\".to_string())]
    );

    let error = read_multiple(r#""bad: \q""#).unwrap_err();
    assert!(error.contains("invalid escape"), "got: {error}");
  }

  #[test]
  fn hash_starts_a_comment_but_semicolon_does_not() {
    assert_eq!(
      read_multiple("1 # ignored\n2").unwrap(),
      vec![AST::Int(1), AST::Int(2)]
    );
    assert_eq!(
      read_multiple("; comment").unwrap(),
      vec![
        AST::Variable(";".to_string()),
        AST::Variable("comment".to_string()),
      ]
    );
  }

  #[test]
  fn functions_require_a_body() {
    let error = read_multiple("(fn main ())").unwrap_err();
    assert!(
      error.contains("`fn` must have at least one body expression"),
      "got: {error}"
    );
  }

  #[test]
  fn rejects_non_decimal_and_number_prefixed_tokens() {
    for source in ["1e3", "1.2.3", "123abc"] {
      let error = read_multiple(source).unwrap_err();
      assert!(
        error.contains("invalid decimal number"),
        "{source}: {error}"
      );
    }
  }

  #[test]
  fn integer_boundaries_do_not_fall_back_to_floats() {
    assert_eq!(
      read_multiple("9223372036854775807 -9223372036854775808").unwrap(),
      vec![AST::Int(i64::MAX), AST::Int(i64::MIN)]
    );

    for source in ["9223372036854775808", "-9223372036854775809"] {
      let error = read_multiple(source).unwrap_err();
      assert!(error.contains("outside the i64 range"), "{source}: {error}");
      assert!(error.contains("line 1, column 1"), "{source}: {error}");
    }
  }

  #[test]
  fn malformed_and_non_finite_floats_are_rejected() {
    for source in ["1.", "1.2.3"] {
      let error = read_multiple(source).unwrap_err();
      assert!(
        error.contains("invalid decimal number"),
        "{source}: {error}"
      );
    }

    let source = format!("{}.0", "9".repeat(400));
    let error = read_multiple(&source).unwrap_err();
    assert!(error.contains("is not finite"), "got: {error}");
  }

  #[test]
  fn backslashes_do_not_escape_symbol_delimiters() {
    assert_eq!(
      read_multiple(r"one\ two").unwrap(),
      vec![
        AST::Variable(r"one\".to_string()),
        AST::Variable("two".to_string()),
      ]
    );
  }

  #[test]
  fn special_form_arity_errors_are_positioned() {
    let error = read_multiple("(if true 1)").unwrap_err();
    assert!(error.contains("line 1, column 11"), "got: {error}");
    assert!(error.contains("expected an expression"), "got: {error}");

    let error = read_multiple("(let x 1 2)").unwrap_err();
    assert!(error.contains("exactly two arguments"), "got: {error}");
    assert!(error.contains("expected `)`"), "got: {error}");
  }

  #[test]
  fn ast_nodes_keep_full_byte_spans() {
    let atom = parse_internal("  42").unwrap().remove(0);
    assert_eq!(atom.span, 2..4);

    let conditional = parse_internal("(if true (f 1) 2)").unwrap().remove(0);
    assert_eq!(conditional.span, 0..17);
    let ASTKind::If(condition, then_branch, else_branch) = &conditional.kind else {
      panic!("expected if, got {conditional:?}");
    };
    assert_eq!(condition.span, 4..8);
    assert_eq!(then_branch.span, 9..14);
    assert_eq!(else_branch.span, 15..16);

    let ASTKind::CallFixed(_, args) = &then_branch.kind else {
      panic!("expected fixed call, got {then_branch:?}");
    };
    assert_eq!(args[0].span, 12..13);
  }

  #[test]
  fn semantic_equality_ignores_spans() {
    assert_eq!(
      AST::new(ASTKind::Int(1), 0..1),
      AST::new(ASTKind::Int(1), 20..21)
    );
  }

  #[test]
  fn eof_errors_use_an_empty_span_at_source_end() {
    let source = "(f 1";
    let error = parse_internal(source).unwrap_err();
    assert_eq!(error.span, source.len()..source.len());
  }

  #[test]
  fn source_positions_count_characters_instead_of_utf8_bytes() {
    assert_eq!(source_position("ééx", 4), (1, 3));
    assert_eq!(source_position("é\n  x", 5), (2, 3));
  }
}
