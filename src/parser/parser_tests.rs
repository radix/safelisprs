use super::*;
use rstest::rstest;

#[test]
fn test_read_multiple() {
  let result = read_multiple("5 3").unwrap();
  assert_eq!(result, vec![AST::Int(5), AST::Int(3)]);
}

#[test]
fn qualified_symbol_in_value_position_is_a_function_ref() {
  let result = read_multiple("std::len").unwrap();
  assert_eq!(
    result,
    vec![AST::FunctionRef("std".to_string(), "len".to_string())]
  );
}

#[test]
fn dotted_symbol_in_value_position_is_field_access() {
  let result = read_multiple("foo.bar.baz").unwrap();
  assert_eq!(
    result,
    vec![AST::FieldAccess(
      AST::FieldAccess(AST::Variable("foo".to_string()), "bar".to_string()),
      "baz".to_string(),
    )]
  );
}

#[test]
fn qualified_symbol_in_call_head_is_a_qualified_fixed_call() {
  let result = read_multiple("(std::+ 1 2)").unwrap();
  assert_eq!(
    result,
    vec![AST::CallFixed(
      Identifier::Qualified("std".to_string(), "+".to_string()),
      vec![AST::Int(1), AST::Int(2)],
    )]
  );
}

#[test]
fn dotted_symbol_in_call_head_is_a_dynamic_field_call() {
  let result = read_multiple("(foo.bar 3)").unwrap();
  assert_eq!(
    result,
    vec![AST::synthetic(ASTKind::Call(
      Box::new(AST::FieldAccess(
        AST::Variable("foo".to_string()),
        "bar".to_string(),
      )),
      vec![AST::Int(3)],
    ))]
  );
}

#[test]
fn set_is_parsed_as_an_ordinary_call() {
  let result = read_multiple("(set! x 2)").unwrap();
  assert_eq!(
    result,
    vec![AST::CallFixed(
      Identifier::Bare("set!".into()),
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
fn parses_boolean_literals() {
  assert_eq!(
    read_multiple("true false").unwrap(),
    vec![
      AST::synthetic(ASTKind::Bool(true)),
      AST::synthetic(ASTKind::Bool(false))
    ]
  );
}

#[test]
fn parses_nested_functions() {
  let result = read_multiple("(fn outer (x:Int) (fn inner (y:Int) ->Int (std::+ x y)) inner)");
  assert!(result.is_ok(), "got: {result:?}");
}

#[test]
fn parses_layout_function_like_parenthesized_function() {
  let layout = read_multiple(
    "fn foo (x:Int) -> Int
  (+ x 1)",
  )
  .unwrap();
  let parenthesized = read_multiple("(fn foo (x:Int) -> Int (+ x 1))").unwrap();

  assert_eq!(layout, parenthesized);
}

#[test]
fn parses_layout_if_like_parenthesized_if() {
  let layout = read_multiple(
    "if (> x 0)
  x
else
  (- 0 x)",
  )
  .unwrap();
  let parenthesized = read_multiple("(if (> x 0) x (- 0 x))").unwrap();

  assert_eq!(layout, parenthesized);
}

#[test]
fn parses_nested_layout_forms() {
  let layout = read_multiple(
    "fn main () -> Int
  if true
    1
  else
    2",
  )
  .unwrap();
  let parenthesized = read_multiple("(fn main () -> Int (if true 1 2))").unwrap();

  assert_eq!(layout, parenthesized);
}

#[test]
fn layout_if_branches_with_multiple_expressions_are_implicit_blocks() {
  let layout = read_multiple(
    "fn main () -> Int
  if true
    let x 1
    x
  else
    2",
  )
  .unwrap();
  let parenthesized = read_multiple("(fn main () -> Int (if true (block (let x 1) x) 2))").unwrap();

  assert_eq!(layout, parenthesized);
}

#[test]
fn parses_layout_else_if_as_nested_if() {
  let layout = read_multiple(
    "if a
  1
else if b
  2
else
  3",
  )
  .unwrap();
  let parenthesized = read_multiple("(if a 1 (if b 2 3))").unwrap();

  assert_eq!(layout, parenthesized);
}

#[test]
fn parses_parenless_layout_let() {
  let layout = read_multiple("let x 3").unwrap();
  let parenthesized = read_multiple("(let x 3)").unwrap();

  assert_eq!(layout, parenthesized);
}

#[test]
fn parses_return_forms() {
  let asts = read_multiple("(return) (return 3)").unwrap();
  assert!(matches!(asts[0].kind, ASTKind::Return(None)));
  let ASTKind::Return(Some(value)) = &asts[1].kind else {
    panic!("expected valued return");
  };
  assert_eq!(**value, AST::Int(3));
}

#[test]
fn parses_boolean_special_forms() {
  let asts = read_multiple("(and true false true) (or false true)").unwrap();
  let ASTKind::And(operands) = &asts[0].kind else {
    panic!("expected and form");
  };
  assert_eq!(operands.len(), 3);
  let ASTKind::Or(operands) = &asts[1].kind else {
    panic!("expected or form");
  };
  assert_eq!(operands.len(), 2);
}

#[test]
fn boolean_special_forms_require_two_operands() {
  assert_eq!(
    read_multiple("(and)").unwrap_err(),
    "line 1, column 1: `and` requires at least two operands"
  );
  assert_eq!(
    read_multiple("(or)").unwrap_err(),
    "line 1, column 1: `or` requires at least two operands"
  );
  assert_eq!(
    read_multiple("(and true)").unwrap_err(),
    "line 1, column 1: `and` requires at least two operands"
  );
  assert_eq!(
    read_multiple("(or false)").unwrap_err(),
    "line 1, column 1: `or` requires at least two operands"
  );
}

#[test]
fn parses_layout_boolean_forms() {
  assert_eq!(
    read_multiple("and true false true").unwrap(),
    read_multiple("(and true false true)").unwrap()
  );
  assert_eq!(
    read_multiple("or false true").unwrap(),
    read_multiple("(or false true)").unwrap()
  );
}

#[test]
fn parses_layout_for_like_parenthesized_for() {
  let layout = read_multiple(
    "fn main ()
  for x in (list 1 2 3)
    (set! target x)",
  )
  .unwrap();
  let parenthesized =
    read_multiple("(fn main () (for x in (list 1 2 3) (set! target x)))").unwrap();

  assert_eq!(layout, parenthesized);
}

#[test]
fn parses_layout_return_with_and_without_value() {
  let asts = read_multiple(
    "fn valued () -> Int
  return 3
fn empty ()
  return",
  )
  .unwrap();
  let ASTKind::DefineFn(valued) = &asts[0].kind else {
    panic!("expected function");
  };
  assert!(matches!(valued.code[0].kind, ASTKind::Return(Some(_))));
  let ASTKind::DefineFn(empty) = &asts[1].kind else {
    panic!("expected function");
  };
  assert!(matches!(empty.code[0].kind, ASTKind::Return(None)));
}

#[rstest]
#[case(
  "let new 3",
  "new",
  Some("first argument to `let` must be a symbol"),
  "a symbol",
  1,
  5
)]
#[case(
  "fn new () -> Int 3",
  "new",
  Some("`fn` name must be a symbol"),
  "a symbol",
  1,
  4
)]
#[case(
  "struct Point\n  match:Int",
  "match",
  Some("struct field names must be symbols"),
  "a symbol",
  2,
  3
)]
#[case(
  "enum Maybe\n  (Some else:Int)",
  "else",
  Some("enum variant field names must be symbols"),
  "a symbol",
  2,
  9
)]
#[case(
  "fn main () -> Int\n  let x 1\n  else",
  "else",
  None,
  "an expression",
  3,
  3
)]
#[case(
  "fn main (where:Int) -> Int 1",
  "where",
  Some("Parameters must be symbols"),
  "a symbol",
  1,
  10
)]
#[case(
  "let return 3",
  "return",
  Some("first argument to `let` must be a symbol"),
  "a symbol",
  1,
  5
)]
#[case(
  "let and 3",
  "and",
  Some("first argument to `let` must be a symbol"),
  "a symbol",
  1,
  5
)]
#[case(
  "let or 3",
  "or",
  Some("first argument to `let` must be a symbol"),
  "a symbol",
  1,
  5
)]
#[case(
  "let for 3",
  "for",
  Some("first argument to `let` must be a symbol"),
  "a symbol",
  1,
  5
)]
#[case(
  "let in 3",
  "in",
  Some("first argument to `let` must be a symbol"),
  "a symbol",
  1,
  5
)]
fn syntax_tokens_cannot_be_used_as_names(
  #[case] source: &str,
  #[case] token_name: &str,
  #[case] annotation: Option<&str>,
  #[case] expected: &str,
  #[case] line: usize,
  #[case] column: usize,
) {
  let error = read_multiple(source).unwrap_err();
  let annotation = annotation.map_or(String::new(), |annotation| format!("; {annotation}"));
  assert_eq!(
    error,
    format!(
      "line {line}, column {column}: unexpected {token_name}{annotation}; expected {expected}"
    ),
  );
}

#[test]
fn parenless_ordinary_calls_are_not_layout_calls() {
  let error = read_multiple(
    "fn main () -> Int
  foo 1",
  )
  .unwrap_err();

  assert!(
    error.contains("layout expressions must end at the end of the line"),
    "got: {error}"
  );
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
      ("a".into(), Some(TypeAst::Named("A".into()))),
      (
        "xs".into(),
        Some(TypeAst::Apply(
          "List".to_string(),
          vec![TypeAst::Named("Int".into())]
        ))
      )
    ]
  );
  assert_eq!(function.return_type, Some(TypeAst::Named("A".into())));
  assert_eq!(
    function.bounds,
    vec![Bound {
      var: "A".to_string(),
      traits: vec!["Eq".to_string()]
    }]
  );
}

#[test]
fn parses_qualified_type_names() {
  let asts = read_multiple("(fn use-box (box:left::Box) -> right::Box box)").unwrap();
  let ASTKind::DefineFn(function) = &asts[0].kind else {
    panic!("expected function")
  };
  assert_eq!(
    function.params[0].1,
    Some(TypeAst::Named(TypeNameAst::qualified("left", "Box")))
  );
  assert_eq!(
    function.return_type,
    Some(TypeAst::Named(TypeNameAst::qualified("right", "Box")))
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
      vec![TypeAst::Named("Int".into())],
      None,
      Box::new(TypeAst::Named("Int".into()))
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
      Some(Box::new(TypeAst::Named("Int".into()))),
      Box::new(TypeAst::Apply(
        "List".to_string(),
        vec![TypeAst::Named("Int".into())]
      ))
    ))
  );
}

#[test]
fn parses_enum_definitions_and_construction() {
  let asts = read_multiple(
    "(enum Foo
       (Var1)
       (Var2 x:Int)
       (Var3 y:String z:(Cell Int)))
     (fn main () ->Foo (new Foo::Var2 x:3))",
  )
  .unwrap();

  let ASTKind::DefineEnum(enum_) = &asts[0].kind else {
    panic!("expected enum definition");
  };
  assert_eq!(enum_.name, "Foo");
  assert_eq!(enum_.variants.len(), 3);
  assert_eq!(enum_.variants[0].name, "Var1");
  assert!(enum_.variants[0].fields.is_empty());
  assert_eq!(
    enum_.variants[2].fields,
    vec![
      ("y".to_string(), TypeAst::Named("String".into())),
      (
        "z".to_string(),
        TypeAst::Apply("Cell".to_string(), vec![TypeAst::Named("Int".into())])
      )
    ]
  );

  let ASTKind::DefineFn(main) = &asts[1].kind else {
    panic!("expected function");
  };
  assert!(matches!(
    main.code[0].kind,
    ASTKind::NewEnum(ref enum_name, ref variant, _)
      if enum_name == "Foo" && variant == "Var2"
  ));
}

#[test]
fn parses_match_with_variant_and_default_arms() {
  let asts = read_multiple(
    "(fn main (foo:Foo) ->Int
       (match foo
         (Var1) => 1
         (Var2 x) => x
         _ => 5))",
  )
  .unwrap();

  let ASTKind::DefineFn(main) = &asts[0].kind else {
    panic!("expected function");
  };
  let ASTKind::Match(scrutinee, arms) = &main.code[0].kind else {
    panic!("expected match");
  };
  assert!(matches!(scrutinee.kind, ASTKind::Variable(_)));
  assert_eq!(arms.len(), 3);
  assert_eq!(
    arms[0].pattern,
    MatchPattern::Variant {
      variant: "Var1".to_string(),
      fields: vec![],
    }
  );
  assert_eq!(
    arms[1].pattern,
    MatchPattern::Variant {
      variant: "Var2".to_string(),
      fields: vec!["x".into()],
    }
  );
  assert_eq!(arms[2].pattern, MatchPattern::Default);
}

#[test]
fn layout_match_arm_bodies_with_multiple_expressions_are_implicit_blocks() {
  let layout = read_multiple(
    "match x
  (Some value) =>
    let next (+ value 1)
    next
  (None) => 0",
  )
  .unwrap();
  let parenthesized =
    read_multiple("(match x (Some value) => (block (let next (+ value 1)) next) (None) => 0)")
      .unwrap();

  assert_eq!(layout, parenthesized);
}

#[test]
fn ellipsis_is_syntax_not_a_symbol_expression() {
  let error = read_multiple("...Int").unwrap_err();
  assert_eq!(
    error,
    "line 1, column 1: unexpected ...; expected an expression"
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
  let error = read_multiple("(if true 1 2 3)").unwrap_err();
  assert!(error.contains("line 1, column 14"), "got: {error}");
  assert!(error.contains("two or three arguments"), "got: {error}");
  assert!(error.contains("expected `)`"), "got: {error}");

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
  let Some(else_branch) = else_branch else {
    panic!("expected else branch");
  };
  assert_eq!(else_branch.span, 15..16);

  let ASTKind::CallFixed(_, args) = &then_branch.kind else {
    panic!("expected fixed call, got {then_branch:?}");
  };
  assert_eq!(args[0].span, 12..13);
}

#[test]
fn layout_preserves_real_token_spans() {
  let source = "fn main () -> Int\n    (+ 1\n       \"bad\")";
  let function = parse_internal(source).unwrap().remove(0);
  let ASTKind::DefineFn(function) = function.kind else {
    panic!("expected function");
  };
  let ASTKind::CallFixed(_, args) = &function.code[0].kind else {
    panic!("expected call");
  };
  let expected_start = source.find("\"bad\"").unwrap();
  assert_eq!(args[1].span, expected_start..expected_start + 5);
}

#[test]
fn lexer_omits_newlines_next_to_indents_and_dedents() {
  let tokens = Lexer::new(
    "fn main () -> Int
  let x 1
  x
let y 2",
  )
  .lex()
  .unwrap();

  assert!(
    tokens.windows(2).all(|window| !matches!(
      (&window[0].kind, &window[1].kind),
      (TokenKind::Newline, TokenKind::Indent | TokenKind::Dedent)
        | (TokenKind::Indent | TokenKind::Dedent, TokenKind::Newline)
    )),
    "tokens: {tokens:?}",
  );
  assert!(
    tokens.windows(3).any(|window| matches!(
      (&window[0].kind, &window[1].kind, &window[2].kind),
      (TokenKind::Int(1), TokenKind::Newline, TokenKind::Sym(name)) if name == "x"
    )),
    "tokens: {tokens:?}",
  );
}

#[test]
fn layout_requires_an_indented_body() {
  let error = read_multiple("fn main () -> Int").unwrap_err();
  assert!(
    error.contains("`fn` layout body must be indented"),
    "{error}"
  );
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

#[test]
fn bare_tuple_head_parses_as_new_tuple() {
  let result = read_multiple("(Tuple 3 \"foo\")").unwrap();
  assert_eq!(
    result,
    vec![AST::NewTuple(vec![
      AST::Int(3),
      AST::String("foo".to_string())
    ])],
  );
}

#[test]
fn qualified_tuple_head_is_not_the_tuple_constructor() {
  // `module::Tuple` is an ordinary qualified call, not the tuple constructor.
  let result = read_multiple("(std::Tuple 1 2)").unwrap();
  assert_eq!(
    result,
    vec![AST::CallFixed(
      Identifier::Qualified("std".to_string(), "Tuple".to_string()),
      vec![AST::Int(1), AST::Int(2)],
    )],
  );
}

#[test]
fn bind_desugars_to_a_block_of_let_and_shd_field_accesses() {
  let result = read_multiple("bind ((shd list) (let result)) (Tuple 1 2)").unwrap();
  assert_eq!(result.len(), 1);
  let ASTKind::Block(body) = &result[0].kind else {
    panic!("`bind` should desugar to a block, got {:?}", result[0].kind);
  };
  // One temp `let`, one binding per pattern, plus a trailing reference to the
  // temp so the block evaluates to the whole tuple.
  assert_eq!(body.len(), 4);
  // First: the temp binding holding the destructured value.
  let ASTKind::Let(temp_name, None, expr) = &body[0].kind else {
    panic!("first body item should be a `let`, got {:?}", body[0].kind);
  };
  assert!(
    temp_name.as_str().starts_with("__bind_tmp_"),
    "temp name should be prefixed, got {temp_name}"
  );
  let _ = expr;
  // Second: `shd list <temp>.0`.
  let ASTKind::Shd(name, None, access) = &body[1].kind else {
    panic!("second body item should be a `shd`, got {:?}", body[1].kind);
  };
  assert_eq!(name.as_str(), "list");
  let ASTKind::FieldAccess(receiver, field) = &access.kind else {
    panic!("shd value should be a field access, got {:?}", access.kind);
  };
  assert_eq!(field, "0");
  let _ = receiver;
  // Third: `let result <temp>.1`.
  let ASTKind::Let(name, None, access) = &body[2].kind else {
    panic!("third body item should be a `let`, got {:?}", body[2].kind);
  };
  assert_eq!(name.as_str(), "result");
  let ASTKind::FieldAccess(_, field) = &access.kind else {
    panic!("let value should be a field access, got {:?}", access.kind);
  };
  assert_eq!(field, "1");
  // Fourth: the trailing reference to the temp (the whole tuple value).
  let ASTKind::Variable(trailing) = &body[3].kind else {
    panic!(
      "trailing body item should be a variable, got {:?}",
      body[3].kind
    );
  };
  assert_eq!(trailing.as_str(), temp_name.as_str());
}

#[test]
fn bind_requires_parenthesized_pattern_group() {
  let err = parse_internal("bind (shd list) (Tuple 1 2)").unwrap_err();
  assert!(
    format!("{err:?}").contains("binding pattern must be parenthesized"),
    "{err:?}"
  );
}
