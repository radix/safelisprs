use std::ops::Range;

type Span = Range<usize>;

#[derive(Debug, PartialEq, Clone)]
pub enum AST {
  Let(String, Box<AST>),
  DefineFn(Function),
  Call(Box<AST>, Vec<AST>),
  CallFixed(Identifier, Vec<AST>),
  Variable(String),
  Int(i64),
  Float(f64),
  String(String),
  Bool(bool),

  // The following variants aren't represented in the syntax, but are produced
  // by transformations on the previous variants.
  /// A Cell wraps a value in a box. This is used to provide closures with
  /// access to values in outer variables.
  Cell(Box<AST>),
  /// And we can deref these cells to get their inner value.
  DerefCell(Box<AST>),
  /// Set the contents of a Cell. The first expression must evaluate to a
  /// Cell; the second is the new value to store in it. Evaluates to the new
  /// value.
  SetCell(Box<AST>, Box<AST>),
  /// Bind up some arguments with a callable. (this is used for passing cells to closures!)
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

#[derive(Debug, PartialEq, Clone)]
pub enum Identifier {
  Bare(String),
  Qualified(String, String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
  pub name: String,
  pub params: Vec<String>,
  pub code: Vec<AST>,
}

#[derive(Debug, PartialEq, Clone)]
enum TokenKind {
  LParen,
  RParen,
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
struct SpannedAst {
  ast: AST,
  span: Span,
}

impl SpannedAst {
  fn new(ast: AST, span: Span) -> Self {
    Self { ast, span }
  }
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
    let offset = self.span.start.min(source.len());
    let before = &source[..offset];
    let line = before.bytes().filter(|byte| *byte == b'\n').count() + 1;
    let column = before
      .rsplit_once('\n')
      .map_or(before, |(_, current_line)| current_line)
      .chars()
      .count()
      + 1;

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
    while self.peek_char().is_some_and(|ch| !is_delimiter(ch)) {
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
  ch.is_whitespace() || matches!(ch, '(' | ')' | '"' | '#')
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

  fn parse_multiple(&mut self) -> Result<Vec<SpannedAst>, ParseError> {
    let mut result = Vec::new();
    while !matches!(self.peek().kind, TokenKind::Eof) {
      result.push(self.parse_expr()?);
    }
    Ok(result)
  }

  fn parse_expr(&mut self) -> Result<SpannedAst, ParseError> {
    let token = self.advance();
    match token.kind {
      TokenKind::LParen => self.parse_list(token.span.start),
      TokenKind::RParen => {
        Err(ParseError::new(token.span, "unexpected `)`").expected("an expression"))
      }
      TokenKind::Sym(name) => Ok(spanned_ast_from_symbol(name, token.span)),
      TokenKind::Int(value) => Ok(SpannedAst::new(AST::Int(value), token.span)),
      TokenKind::Float(value) => Ok(SpannedAst::new(AST::Float(value), token.span)),
      TokenKind::Str(value) => Ok(SpannedAst::new(AST::String(value), token.span)),
      TokenKind::Eof => {
        Err(ParseError::new(token.span, "unexpected end of input").expected("an expression"))
      }
    }
  }

  fn parse_list(&mut self, start: usize) -> Result<SpannedAst, ParseError> {
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

  fn parse_let(&mut self, start: usize) -> Result<SpannedAst, ParseError> {
    let variable = self.expect_symbol("first argument to `let` must be a symbol")?;
    let expression = self.parse_expr()?;
    let close = self.expect_close("`let` must have exactly two arguments")?;
    let span = start..close.span.end;
    let SpannedAst {
      ast: expression,
      span: _expression_span,
    } = expression;
    Ok(SpannedAst::new(
      AST::Let(variable, Box::new(expression)),
      span,
    ))
  }

  fn parse_fn(&mut self, start: usize) -> Result<SpannedAst, ParseError> {
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
      params.push(self.expect_symbol("Parameters must be symbols")?);
    }
    self.advance();

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
    let code = body
      .into_iter()
      .map(|SpannedAst { ast, span: _span }| ast)
      .collect();
    Ok(SpannedAst::new(
      AST::DefineFn(Function { name, params, code }),
      span,
    ))
  }

  fn parse_if(&mut self, start: usize) -> Result<SpannedAst, ParseError> {
    let condition = self.parse_expr()?;
    let then_branch = self.parse_expr()?;
    let else_branch = self.parse_expr()?;
    let close = self.expect_close("`if` must have exactly three arguments: cond, then, else")?;
    let span = start..close.span.end;
    let SpannedAst {
      ast: condition,
      span: _condition_span,
    } = condition;
    let SpannedAst {
      ast: then_branch,
      span: _then_span,
    } = then_branch;
    let SpannedAst {
      ast: else_branch,
      span: _else_span,
    } = else_branch;
    Ok(SpannedAst::new(
      AST::If(
        Box::new(condition),
        Box::new(then_branch),
        Box::new(else_branch),
      ),
      span,
    ))
  }

  fn parse_block(&mut self, start: usize) -> Result<SpannedAst, ParseError> {
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
    let expressions = expressions
      .into_iter()
      .map(|SpannedAst { ast, span: _span }| ast)
      .collect();
    Ok(SpannedAst::new(AST::Block(expressions), span))
  }

  fn parse_fixed_call(&mut self, start: usize) -> Result<SpannedAst, ParseError> {
    let head = self.advance();
    let TokenKind::Sym(name) = head.kind else {
      unreachable!("fixed calls have symbol heads")
    };
    let identifier = parse_identifier(&name);
    let (args, close) = self.parse_call_args()?;
    let span = start..close.span.end;
    let args = args
      .into_iter()
      .map(|SpannedAst { ast, span: _span }| ast)
      .collect();
    Ok(SpannedAst::new(AST::CallFixed(identifier, args), span))
  }

  fn parse_dynamic_call(&mut self, start: usize) -> Result<SpannedAst, ParseError> {
    let callee = self.parse_expr()?;
    let (args, close) = self.parse_call_args()?;
    let span = start..close.span.end;
    let SpannedAst {
      ast: callee,
      span: _callee_span,
    } = callee;
    let args = args
      .into_iter()
      .map(|SpannedAst { ast, span: _span }| ast)
      .collect();
    Ok(SpannedAst::new(AST::Call(Box::new(callee), args), span))
  }

  fn parse_call_args(&mut self) -> Result<(Vec<SpannedAst>, Token), ParseError> {
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

fn spanned_ast_from_symbol(name: String, span: Span) -> SpannedAst {
  let ast = match name.as_str() {
    "true" => AST::Bool(true),
    "false" => AST::Bool(false),
    _ => match parse_identifier(&name) {
      Identifier::Bare(name) => AST::Variable(name),
      Identifier::Qualified(module, name) => AST::FunctionRef(module, name),
    },
  };
  SpannedAst::new(ast, span)
}

fn parse_identifier(name: &str) -> Identifier {
  match name.split_once('.') {
    Some((module, name)) => Identifier::Qualified(module.to_string(), name.to_string()),
    None => Identifier::Bare(name.to_string()),
  }
}

fn parse_spanned_multiple(source: &str) -> Result<Vec<SpannedAst>, ParseError> {
  let tokens = Lexer::new(source).lex()?;
  Parser::new(tokens).parse_multiple()
}

pub fn read_multiple(source: &str) -> Result<Vec<AST>, String> {
  parse_spanned_multiple(source)
    .map(|forms| {
      forms
        .into_iter()
        .map(|SpannedAst { ast, span: _span }| ast)
        .collect()
    })
    .map_err(|error| error.render(source))
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
    let result = read_multiple("(fn outer (x) (fn inner (y) (std.+ x y)) inner)");
    assert!(result.is_ok(), "got: {result:?}");
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
  fn parser_keeps_full_byte_spans_until_ast_construction() {
    let atom = parse_spanned_multiple("  42").unwrap().remove(0);
    assert_eq!(atom.span, 2..4);

    let nested_call = parse_spanned_multiple("(f (g 1))").unwrap().remove(0);
    assert_eq!(nested_call.span, 0..9);

    let special_form = parse_spanned_multiple("(if true 1 2)").unwrap().remove(0);
    assert_eq!(special_form.span, 0..13);
  }

  #[test]
  fn eof_errors_use_an_empty_span_at_source_end() {
    let source = "(f 1";
    let error = parse_spanned_multiple(source).unwrap_err();
    assert_eq!(error.span, source.len()..source.len());
  }
}
