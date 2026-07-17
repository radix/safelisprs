use super::*;

fn returns(name: &str) -> Option<parser::TypeAst> {
  Some(parser::TypeAst::Named(name.to_string()))
}

fn compile_test_function(f: &parser::Function) -> Result<(String, CompiledCallable), String> {
  let asts = resolve_module_names("main", &[AST::DefineFn(f.clone())], &[], &[])?;
  let type_info = TypecheckInfo::default();
  let module = ModuleCompiler::new("main", &asts, &type_info);
  let ASTKind::DefineFn(f) = &asts[0].kind else {
    unreachable!("the test input is a function");
  };
  module.compile_function(f)
}

#[test]
fn compile_id() {
  let func = parser::Function {
    name: "id".into(),
    params: vec![("a".into(), None)],
    return_type: returns("Int"),
    bounds: vec![],
    code: vec![AST::Variable("a".to_string())],
  };
  let code = compile_test_function(&func).unwrap();
  assert_eq!(
    code,
    (
      "id".to_string(),
      Callable::Function(Function {
        num_params: 1,
        num_locals: 1,
        instructions: vec![Instruction::LoadLocal(0), Instruction::Return],
      }),
    )
  );
}

#[test]
fn compile_call_to_local_function_ref() {
  let func = parser::Function {
    name: "main".into(),
    params: vec![],
    return_type: returns("Int"),
    bounds: vec![],
    code: vec![
      AST::Let(
        "alias".to_string(),
        Box::new(AST::FunctionRef("main".to_string(), "x".to_string())),
      ),
      AST::CallFixed(Identifier::Bare("alias".into()), vec![]),
    ],
  };
  let code = compile_test_function(&func).unwrap();
  assert_eq!(
    code,
    (
      "main".to_string(),
      Callable::Function(Function {
        num_params: 0,
        num_locals: 1,
        instructions: vec![
          Instruction::MakeFunctionRef(("main".to_string(), "x".to_string())),
          Instruction::SetLocal(0),
          Instruction::LoadLocal(0),
          Instruction::Pop,
          Instruction::LoadLocal(0),
          Instruction::CallDynamic(0),
          Instruction::Return,
        ],
      }),
    )
  );
}

#[test]
fn compile_let_returns_bound_value() {
  let func = parser::Function {
    name: "main".into(),
    params: vec![],
    return_type: returns("Int"),
    bounds: vec![],
    code: vec![AST::Let("a".to_string(), Box::new(AST::Int(1)))],
  };
  let code = compile_test_function(&func).unwrap();
  assert_eq!(
    code,
    (
      "main".to_string(),
      Callable::Function(Function {
        num_params: 0,
        num_locals: 1,
        instructions: vec![
          Instruction::PushInt(1),
          Instruction::SetLocal(0),
          Instruction::LoadLocal(0),
          Instruction::Return,
        ],
      }),
    )
  );
}

#[test]
fn compile_void_discards_body_value() {
  let func = parser::Function {
    name: "main".into(),
    params: vec![],
    return_type: None,
    bounds: vec![],
    code: vec![AST::Int(1)],
  };
  let code = compile_test_function(&func).unwrap();
  assert_eq!(
    code,
    (
      "main".to_string(),
      Callable::Function(Function {
        num_params: 0,
        num_locals: 0,
        instructions: vec![
          Instruction::PushInt(1),
          Instruction::Pop,
          Instruction::PushVoid,
          Instruction::Return,
        ],
      }),
    )
  );
}

#[test]
fn compile_unbound_variable_still_errors() {
  let func = parser::Function {
    name: "main".into(),
    params: vec![],
    return_type: None,
    bounds: vec![],
    code: vec![AST::Variable("missing".to_string())],
  };
  let err = compile_test_function(&func).unwrap_err();
  assert_eq!(err, "Function accesses unbound variable missing");
}

#[test]
fn linked_struct_bytecode_uses_indices() {
  let builtins = crate::builtins::default_builtins();
  let source = "
      (struct Foo x:Int y:Int)
      (fn main () ->Int
        (let foo (new Foo y:2 x:3))
        foo.x)";
  let package =
    compile_executable_from_source(source, ("main", "main"), &builtins.specs(), &[]).unwrap();
  let Callable::Function(main) = package.get_function(0, 0).unwrap() else {
    panic!("expected main function");
  };
  assert_eq!(
    main.instructions,
    vec![
      Instruction::PushInt(3),
      Instruction::PushInt(2),
      Instruction::NewStruct((0, 0)),
      Instruction::SetLocal(0),
      Instruction::LoadLocal(0),
      Instruction::Pop,
      Instruction::LoadLocal(0),
      Instruction::GetField(0),
      Instruction::Return,
    ]
  );
  assert_eq!(
    package.modules[0].types,
    vec![TypeDef {
      name: "Foo".to_string(),
      constructors: vec![ConstructorDef {
        name: "Foo".to_string(),
        fields: vec!["x".to_string(), "y".to_string()],
      }],
    }]
  );
}

#[test]
fn linked_enum_bytecode_uses_indices() {
  let builtins = crate::builtins::default_builtins();
  let source = "
      (enum Foo
        (Var1)
        (Var2 x:Int y:Int))
      (fn main () ->Foo
        (new Foo::Var2 y:2 x:3))";
  let package =
    compile_executable_from_source(source, ("main", "main"), &builtins.specs(), &[]).unwrap();
  let Callable::Function(main) = package.get_function(0, 0).unwrap() else {
    panic!("expected main function");
  };
  assert_eq!(
    main.instructions,
    vec![
      Instruction::PushInt(3),
      Instruction::PushInt(2),
      Instruction::NewEnum((0, 0), 1),
      Instruction::Return,
    ]
  );
  assert_eq!(
    package.modules[0].types,
    vec![TypeDef {
      name: "Foo".to_string(),
      constructors: vec![
        ConstructorDef {
          name: "Var1".to_string(),
          fields: vec![],
        },
        ConstructorDef {
          name: "Var2".to_string(),
          fields: vec!["x".to_string(), "y".to_string()],
        },
      ],
    }]
  );
}

#[test]
fn enum_match_lowers_to_variant_tests_and_field_loads() {
  let builtins = crate::builtins::default_builtins();
  let source = "
      (enum Foo
        (Var1)
        (Var2 x:Int y:Int))
      (fn main () ->Int
        (match (new Foo::Var2 x:3 y:7)
          (Var1) => 1
          (Var2 y x) => (std::- y x)))";
  let package =
    compile_executable_from_source(source, ("main", "main"), &builtins.specs(), &[]).unwrap();
  let Callable::Function(main) = package.get_function(0, 0).unwrap() else {
    panic!("expected main function");
  };
  assert_eq!(
    main.instructions,
    vec![
      Instruction::PushInt(3),
      Instruction::PushInt(7),
      Instruction::NewEnum((0, 0), 1),
      Instruction::SetLocal(0),
      Instruction::LoadLocal(0),
      Instruction::IsEnumVariant(0),
      Instruction::JumpIfFalse(2),
      Instruction::PushInt(1),
      Instruction::Jump(13),
      Instruction::LoadLocal(0),
      Instruction::IsEnumVariant(1),
      Instruction::JumpIfFalse(10),
      Instruction::LoadLocal(0),
      Instruction::GetEnumField(1),
      Instruction::SetLocal(1),
      Instruction::LoadLocal(0),
      Instruction::GetEnumField(0),
      Instruction::SetLocal(2),
      Instruction::LoadLocal(1),
      Instruction::LoadLocal(2),
      Instruction::Call((1, 1), 2),
      Instruction::Jump(0),
      Instruction::Return,
    ]
  );
  assert_eq!(main.num_locals, 3);
}

#[test]
fn chained_field_access_emits_multiple_field_indices() {
  let builtins = crate::builtins::default_builtins();
  let source = "
      (struct Point x:Int y:Int)
      (struct Box origin:Point size:Int)
      (fn main () ->Int
        (let b (new Box size:10 origin:(new Point x:4 y:5)))
        (std::+ b.origin.x b.origin.y))";
  let package =
    compile_executable_from_source(source, ("main", "main"), &builtins.specs(), &[]).unwrap();
  let Callable::Function(main) = package.get_function(0, 0).unwrap() else {
    panic!("expected main function");
  };
  assert_eq!(
    main.instructions,
    vec![
      Instruction::PushInt(4),
      Instruction::PushInt(5),
      Instruction::NewStruct((0, 0)),
      Instruction::PushInt(10),
      Instruction::NewStruct((0, 1)),
      Instruction::SetLocal(0),
      Instruction::LoadLocal(0),
      Instruction::Pop,
      Instruction::LoadLocal(0),
      Instruction::GetField(0),
      Instruction::GetField(0),
      Instruction::LoadLocal(0),
      Instruction::GetField(0),
      Instruction::GetField(1),
      Instruction::Call((1, 0), 2),
      Instruction::Return,
    ]
  );
}

#[test]
fn field_access_after_function_return_uses_return_annotation() {
  let builtins = crate::builtins::default_builtins();
  let source = "
      (struct Foo x:Int)
      (fn make () ->Foo (new Foo x:9))
      (fn main () ->Int
        (let foo (make))
        foo.x)";
  let package =
    compile_executable_from_source(source, ("main", "main"), &builtins.specs(), &[]).unwrap();
  let Callable::Function(main) = package.get_function(0, 1).unwrap() else {
    panic!("expected main function");
  };
  assert!(main
    .instructions
    .iter()
    .any(|instruction| matches!(instruction, Instruction::GetField(0))));
}

#[test]
fn let_annotation_preserves_struct_type_after_generic_call() {
  let builtins = crate::builtins::default_builtins();
  let source = "
      (struct Foo x:Int)
      (fn id (x:A) ->A x)
      (fn main () ->Int
        (let foo:Foo (id (new Foo x:9)))
        foo.x)";
  let package =
    compile_executable_from_source(source, ("main", "main"), &builtins.specs(), &[]).unwrap();
  let Callable::Function(main) = package.get_function(0, 1).unwrap() else {
    panic!("expected main function");
  };
  assert!(main
    .instructions
    .iter()
    .any(|instruction| matches!(instruction, Instruction::GetField(0))));
}

#[test]
fn inferred_generic_return_preserves_struct_type_for_field_access() {
  let builtins = crate::builtins::default_builtins();
  let source = "
      (struct Foo x:Int)
      (fn id (x:A) ->A x)
      (fn main () ->Int
        (let foo (id (new Foo x:9)))
        foo.x)";
  let package =
    compile_executable_from_source(source, ("main", "main"), &builtins.specs(), &[]).unwrap();
  let Callable::Function(main) = package.get_function(0, 1).unwrap() else {
    panic!("expected main function");
  };
  assert!(main
    .instructions
    .iter()
    .any(|instruction| matches!(instruction, Instruction::GetField(0))));
}

#[test]
fn closure_capture_preserves_struct_type_for_field_access() {
  let builtins = crate::builtins::default_builtins();
  let source = "
      (struct Foo x:Int)
      (fn main () ->Int
        (let foo (new Foo x:9))
        (fn get () ->Int foo.x)
        (get))";
  let package =
    compile_executable_from_source(source, ("main", "main"), &builtins.specs(), &[]).unwrap();
  assert!(package.modules[0].functions.iter().any(|(_, callable)| {
    let Callable::Function(function) = callable else {
      return false;
    };
    function
      .instructions
      .iter()
      .any(|instruction| matches!(instruction, Instruction::GetField(0)))
  }));
}

#[test]
fn source_compilation_rejects_type_errors_before_codegen() {
  let builtins = crate::builtins::default_builtins();
  let source = "(fn main () ->Int\n  (std::+ 1\n    \"not-an-int\"))";
  let error =
    compile_executable_from_source(source, ("main", "main"), &builtins.specs(), &[]).unwrap_err();
  assert!(error.starts_with("line 3, column 5: TypeError:"), "{error}");
  assert!(
    error.contains("type `String` does not satisfy trait `Add`")
      || error.contains("expected `Int`, got `String`"),
    "{error}"
  );
}
