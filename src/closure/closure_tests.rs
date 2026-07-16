
use super::*;
use crate::parser::{erase_bindings, read_multiple, Identifier};

#[allow(non_snake_case)]
fn Let(name: String, value: Box<AST>) -> AST {
  AST::Let(name, value)
}

#[allow(non_snake_case)]
fn Variable(name: String) -> AST {
  AST::Variable(name)
}

#[allow(non_snake_case)]
fn Int(value: i64) -> AST {
  AST::Int(value)
}

#[allow(non_snake_case)]
fn PartialApply(callable: Box<AST>, args: Vec<AST>) -> AST {
  AST::PartialApply(callable, args)
}

#[allow(non_snake_case)]
fn FunctionRef(module: String, name: String) -> AST {
  AST::FunctionRef(module, name)
}

#[allow(non_snake_case)]
fn CallFixed(identifier: Identifier, args: Vec<AST>) -> AST {
  AST::CallFixed(identifier, args)
}

fn erased(ast: &AST) -> AST {
  erase_bindings(std::slice::from_ref(ast))
    .pop()
    .expect("one AST was provided")
}

#[test]
fn transformed_closure() -> Result<(), String> {
  let source = "
      (fn outer ()
        (let a 1)
        (fn inner () a))";
  let asts = read_multiple(source)?;
  let new_asts = transform_closures_in_module("main", &asts)?;
  let expected = vec![
    AST::DefineFn(Function {
      name: "outer:inner:(closure)".into(),
      params: vec![("a".into(), None)],
      return_type: None,
      bounds: vec![],
      code: vec![Variable("a".to_string())],
    }),
    AST::DefineFn(Function {
      name: "outer".into(),
      params: vec![],
      return_type: None,
      bounds: vec![],
      code: vec![
        Let("a".to_string(), Box::new(Int(1))),
        Let(
          "inner".to_string(),
          Box::new(PartialApply(
            Box::new(FunctionRef(
              "main".to_string(),
              "outer:inner:(closure)".to_string(),
            )),
            vec![AST::Variable("a".to_string())],
          )),
        ),
      ],
    }),
  ];
  assert_eq!(erase_bindings(&new_asts), expected);
  Ok(())
}

#[test]
fn captured_outer_parameters_are_plain_values() -> Result<(), String> {
  //! When an outer function takes a parameter that is used by an inner
  //! function, that parameter is passed through directly as a closure
  //! capture.
  let source = "
      (fn outer (par:Int)
        (let b (+ par 1))
        (fn inner () par))";
  let asts = read_multiple(source)?;
  let new_asts = transform_closures_in_module("main", &asts)?;
  let expected = vec![
    AST::DefineFn(Function {
      name: "outer:inner:(closure)".into(),
      params: vec![("par".into(), None)],
      return_type: None,
      bounds: vec![],
      code: vec![Variable("par".to_string())],
    }),
    AST::DefineFn(Function {
      name: "outer".into(),
      params: vec![("par".into(), Some(TypeAst::Named("Int".to_string())))],
      return_type: None,
      bounds: vec![],
      code: vec![
        Let(
          "b".to_string(),
          Box::new(CallFixed(
            Identifier::Bare("+".into()),
            vec![Variable("par".to_string()), Int(1)],
          )),
        ),
        Let(
          "inner".to_string(),
          Box::new(PartialApply(
            Box::new(FunctionRef(
              "main".to_string(),
              "outer:inner:(closure)".to_string(),
            )),
            vec![AST::Variable("par".to_string())],
          )),
        ),
      ],
    }),
  ];
  assert_eq!(erase_bindings(&new_asts), expected);
  Ok(())
}

#[test]
fn non_closure_inner_fn() -> Result<(), String> {
  //! If an inner function doesn't use any variables from the outer function's
  //! environment, then it is not PartialApply'd.
  let source = "
      (fn outer ()
        (fn inner () 1))";
  let asts = read_multiple(source)?;
  let new_asts = transform_closures_in_module("main", &asts)?;
  let expected = vec![
    AST::DefineFn(Function {
      name: "outer:inner:(closure)".into(),
      params: vec![],
      return_type: None,
      bounds: vec![],
      code: vec![Int(1)],
    }),
    AST::DefineFn(Function {
      name: "outer".into(),
      params: vec![],
      return_type: None,
      bounds: vec![],
      code: vec![Let(
        "inner".to_string(),
        Box::new(FunctionRef(
          "main".to_string(),
          "outer:inner:(closure)".to_string(),
        )),
      )],
    }),
  ];
  assert_eq!(erase_bindings(&new_asts), expected);
  Ok(())
}

#[test]
fn nested_self_recursion_targets_lifted_function_with_captures() -> Result<(), String> {
  let source = "
      (fn outer (base:Int)
        (fn countdown (n:Int) ->Int
          (if (std::== n 0) base (countdown (std::- n 1)))))";
  let asts = read_multiple(source)?;
  let new_asts = transform_closures_in_module("main", &asts)?;

  let ASTKind::DefineFn(countdown) = &new_asts[0].kind else {
    panic!("expected lifted countdown function");
  };
  assert_eq!(countdown.name, "outer:countdown:(closure)");
  assert_eq!(
    countdown
      .params
      .iter()
      .map(|(name, _)| name.as_str())
      .collect::<Vec<_>>(),
    vec!["base", "n"]
  );

  let ASTKind::Let(self_name, _, self_value) = &countdown.code[0].kind else {
    panic!("expected local self binding");
  };
  assert_eq!(self_name, "countdown");
  assert_eq!(
    erased(self_value),
    PartialApply(
      Box::new(FunctionRef(
        "main".to_string(),
        "outer:countdown:(closure)".to_string()
      )),
      vec![Variable("base".to_string())]
    )
  );

  let ASTKind::If(_, _, else_branch) = &countdown.code[1].kind else {
    panic!("expected conditional body");
  };
  let ASTKind::Call(callable, args) = &else_branch.kind else {
    panic!("expected recursive call through local self binding");
  };
  let ASTKind::Variable(name) = &callable.kind else {
    panic!("expected recursive local variable");
  };
  assert_eq!(name, "countdown");
  assert_eq!(args.len(), 1);
  Ok(())
}

#[test]
fn nested_mutual_recursion_propagates_group_captures() -> Result<(), String> {
  let source = "
      (fn outer (even-result:Int odd-result:Int)
        (fn even (n:Int) ->Int
          (if (std::== n 0) even-result (odd (std::- n 1))))
        (fn odd (n:Int) ->Int
          (if (std::== n 0) odd-result (even (std::- n 1))))
        even)";
  let asts = read_multiple(source)?;
  let transformed = transform_closures_in_module("main", &asts)?;

  let ASTKind::DefineFn(even) = &transformed[0].kind else {
    panic!("expected lifted even function");
  };
  let ASTKind::DefineFn(odd) = &transformed[1].kind else {
    panic!("expected lifted odd function");
  };
  assert_eq!(
    even
      .params
      .iter()
      .map(|(name, _)| name.as_str())
      .collect::<Vec<_>>(),
    vec!["even-result", "odd-result", "n"]
  );
  assert_eq!(
    odd
      .params
      .iter()
      .map(|(name, _)| name.as_str())
      .collect::<Vec<_>>(),
    vec!["odd-result", "even-result", "n"]
  );

  let ASTKind::Let(name, _, value) = &even.code[0].kind else {
    panic!("expected local odd closure");
  };
  assert_eq!(name, "odd");
  assert_eq!(
    erased(value),
    PartialApply(
      Box::new(FunctionRef(
        "main".to_string(),
        "outer:odd:(closure)".to_string()
      )),
      vec![
        Variable("odd-result".to_string()),
        Variable("even-result".to_string())
      ]
    )
  );

  let ASTKind::Let(name, _, value) = &odd.code[0].kind else {
    panic!("expected local even closure");
  };
  assert_eq!(name, "even");
  assert_eq!(
    erased(value),
    PartialApply(
      Box::new(FunctionRef(
        "main".to_string(),
        "outer:even:(closure)".to_string()
      )),
      vec![
        Variable("even-result".to_string()),
        Variable("odd-result".to_string())
      ]
    )
  );
  Ok(())
}

#[test]
fn tricksy_inner_var_non_closure() -> Result<(), String> {
  //! Inner functions which *define* variables that happen to have the same
  //! name as a variable in an outer function will not bring that outer
  //! variable in as a capture.
  let source = "
      (fn outer ()
        (let a 1)
        (fn inner ()
          (let a 2)
          a))";
  let asts = read_multiple(source)?;
  let new_asts = transform_closures_in_module("main", &asts)?;
  let expected = vec![
    AST::DefineFn(Function {
      name: "outer:inner:(closure)".into(),
      params: vec![],
      return_type: None,
      bounds: vec![],
      code: vec![
        Let("a".to_string(), Box::new(Int(2))),
        Variable("a".to_string()),
      ],
    }),
    AST::DefineFn(Function {
      name: "outer".into(),
      params: vec![],
      return_type: None,
      bounds: vec![],
      code: vec![
        Let("a".to_string(), Box::new(Int(1))),
        Let(
          "inner".to_string(),
          Box::new(FunctionRef(
            "main".to_string(),
            "outer:inner:(closure)".to_string(),
          )),
        ),
      ],
    }),
  ];
  assert_eq!(erase_bindings(&new_asts), expected);
  Ok(())
}

#[test]
fn tricksier_inner_var_closure() -> Result<(), String> {
  //! Inner functions which use and *then* shadow outer variables will still
  //! be treated as closures, but only the variable usages *before* the
  //! rebinding will be treated as captures.
  let source = "
      (fn outer ()
        (let a 1)
        (fn inner ()
          (let a (+ a 1))
          a))";
  let asts = read_multiple(source)?;
  let new_asts = transform_closures_in_module("main", &asts)?;
  let expected = vec![
    AST::DefineFn(Function {
      name: "outer:inner:(closure)".into(),
      params: vec![("a".into(), None)],
      return_type: None,
      bounds: vec![],
      code: vec![
        Let(
          "a".to_string(),
          Box::new(CallFixed(
            Identifier::Bare("+".into()),
            vec![Variable("a".to_string()), Int(1)],
          )),
        ),
        Variable("a".to_string()),
      ],
    }),
    AST::DefineFn(Function {
      name: "outer".into(),
      params: vec![],
      return_type: None,
      bounds: vec![],
      code: vec![
        Let("a".to_string(), Box::new(Int(1))),
        Let(
          "inner".to_string(),
          Box::new(PartialApply(
            Box::new(FunctionRef(
              "main".to_string(),
              "outer:inner:(closure)".to_string(),
            )),
            vec![AST::Variable("a".to_string())],
          )),
        ),
      ],
    }),
  ];
  assert_eq!(erase_bindings(&new_asts), expected);
  Ok(())
}

#[test]
fn nested_closures() -> Result<(), String> {
  let source = "
      (fn outer ()
        (let a 1)
        (fn intermediate ()
          (fn inner () a)
        )
        intermediate
      )
    ";
  let asts = read_multiple(source)?;
  let new_asts = transform_closures_in_module("main", &asts)?;

  let expected = vec![
    AST::DefineFn(Function {
      name: "outer:intermediate:inner:(closure)".into(),
      params: vec![("a".into(), None)],
      return_type: None,
      bounds: vec![],
      code: vec![AST::Variable("a".to_string())],
    }),
    AST::DefineFn(Function {
      name: "outer:intermediate:(closure)".into(),
      params: vec![("a".into(), None)],
      return_type: None,
      bounds: vec![],
      code: vec![AST::Let(
        "inner".to_string(),
        Box::new(AST::PartialApply(
          Box::new(AST::FunctionRef(
            "main".to_string(),
            "outer:intermediate:inner:(closure)".to_string(),
          )),
          vec![AST::Variable("a".to_string())],
        )),
      )],
    }),
    AST::DefineFn(Function {
      name: "outer".into(),
      params: vec![],
      return_type: None,
      bounds: vec![],
      code: vec![
        AST::Let("a".to_string(), Box::new(AST::Int(1))),
        AST::Let(
          "intermediate".to_string(),
          Box::new(AST::PartialApply(
            Box::new(AST::FunctionRef(
              "main".to_string(),
              "outer:intermediate:(closure)".to_string(),
            )),
            vec![AST::Variable("a".to_string())],
          )),
        ),
        AST::Variable("intermediate".to_string()),
      ],
    }),
  ];
  assert_eq!(erase_bindings(&new_asts), expected);
  Ok(())
}

#[test]
fn closure_transforms_preserve_function_spans() -> Result<(), String> {
  let source = "(fn outer () (fn inner () 1))";
  let asts = read_multiple(source)?;
  let outer_span = asts[0].span.clone();
  let ASTKind::DefineFn(outer) = &asts[0].kind else {
    panic!("expected outer function");
  };
  let inner_span = outer.code[0].span.clone();

  let transformed = transform_closures_in_module("main", &asts)?;
  assert_eq!(transformed[0].span, inner_span);
  assert_eq!(transformed[1].span, outer_span);
  Ok(())
}
