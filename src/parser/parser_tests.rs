use super::*;

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
fn parses_nested_functions() {
  let result = read_multiple("(fn outer (x:Int) (fn inner (y:Int) ->Int (std::+ x y)) inner)");
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
      ("a".into(), Some(TypeAst::Named("A".to_string()))),
      (
        "xs".into(),
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
fn parses_qualified_type_names() {
  let asts = read_multiple("(fn use-box (box:left::Box) -> right::Box box)").unwrap();
  let ASTKind::DefineFn(function) = &asts[0].kind else {
    panic!("expected function")
  };
  assert_eq!(
    function.params[0].1,
    Some(TypeAst::Named("left::Box".to_string()))
  );
  assert_eq!(
    function.return_type,
    Some(TypeAst::Named("right::Box".to_string()))
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
      ("y".to_string(), TypeAst::Named("String".to_string())),
      (
        "z".to_string(),
        TypeAst::Apply("Cell".to_string(), vec![TypeAst::Named("Int".to_string())])
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
