use super::*;
use crate::builtins::{default_builtins, sig, Builtin, Builtins, TypeConst};
use crate::compiler::{self, *};
use std::time::Duration;

fn eval_main(source: &str) -> SLValue {
  eval_main_with(source, default_builtins())
}

/// Like `eval_main`, but with a custom builtin registry.
fn eval_main_with(source: &str, builtins: Builtins) -> SLValue {
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &builtins.specs(), &[]).unwrap();
  let interp = Interpreter::with_builtins(pkg, builtins);
  let mut exec = interp.call_main().unwrap();
  exec.run_until_done().unwrap()
}

fn test_module(name: &str, functions: Vec<(String, LinkedCallable)>) -> LinkedModule {
  LinkedModule {
    name: name.to_string(),
    functions,
    types: vec![],
  }
}

fn package_with_main_function(function: LinkedFunction) -> Package {
  Package {
    modules: vec![test_module(
      "main",
      vec![("main".to_string(), Callable::Function(function))],
    )],
    main: Some((0, 0)),
  }
}

fn exec_with_function(function: LinkedFunction, pre_bound: Vec<SLValue>) -> Execution {
  let mut exec = Execution::new(package_with_main_function(function), default_builtins());
  exec.enter_function_at(0, 0, pre_bound).unwrap();
  exec
}

/// Test for a simple "identity" function that returns its argument
#[test]
fn test_interpret_identity() {
  // id takes one argument and returns it. main calls id with 42.
  let id = compiler::Function {
    num_locals: 1,
    num_params: 1,
    instructions: vec![Instruction::LoadLocal(0), Instruction::Return],
  };
  let main = compiler::Function {
    num_locals: 0,
    num_params: 0,
    instructions: vec![
      Instruction::PushInt(42),
      Instruction::Call((0, 0), 1),
      Instruction::Return,
    ],
  };
  let pkg = Package {
    modules: vec![test_module(
      "main",
      vec![
        ("id".to_string(), Callable::Function(id)),
        ("main".to_string(), Callable::Function(main)),
      ],
    )],
    main: Some((0, 1)),
  };
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();
  assert_eq!(exec.run_until_done().unwrap(), SLValue::Int(42));
}

#[test]
fn std_eq_strings() {
  assert_eq!(
    eval_main("(fn main () ->Bool (std::== \"abc\" \"abc\"))"),
    SLValue::Bool(true)
  );
  assert_eq!(
    eval_main("(fn main () ->Bool (std::== \"abc\" \"abd\"))"),
    SLValue::Bool(false)
  );
}

#[test]
fn std_concat_strings() {
  assert_eq!(
    eval_main("(fn main () ->String (std::concat \"foo\" \"bar\"))"),
    SLValue::String("foobar".to_string())
  );
  assert_eq!(
    eval_main("(fn main () ->String (std::concat \"ab\" \"CDE\"))"),
    SLValue::String("abCDE".to_string())
  );
  assert_eq!(
    eval_main("(fn main () ->String (std::concat \"\" \"x\"))"),
    SLValue::String("x".to_string())
  );
}

#[test]
fn constructs_structs_and_reads_fields_without_parentheses() {
  let source = "
      (struct Foo
        x:Int
        y:(Cell Int))
      (fn main () ->Int
        (let foo (new Foo x:3 y:(std::cell 2)))
        foo.x)";
  assert_eq!(eval_main(source), SLValue::Int(3));
}

#[test]
fn struct_cell_fields_can_be_mutated() {
  let source = "
      (struct Foo
        x:Int
        y:(Cell Int))
      (fn main () ->Int
        (let foo (new Foo x:3 y:(std::cell 2)))
        (std::set! foo.y 7)
        (std::get foo.y))";
  assert_eq!(eval_main(source), SLValue::Int(7));
}

#[test]
fn struct_field_initializers_are_named() {
  let source = "
      (struct Foo
        x:Int
        y:Int)
      (fn main () ->Int
        (let foo (new Foo y:2 x:3))
        (std::+ foo.x foo.y))";
  assert_eq!(eval_main(source), SLValue::Int(5));
}

#[test]
fn constructs_enum_values_with_named_fields() {
  let source = "
      (enum Foo
        (Var1)
        (Var2 x:Int)
        (Var3 y:String z:(Cell Int)))
      (fn main () ->Foo
        (new Foo::Var3 z:(std::cell 7) y:\"hi\"))";
  assert_eq!(
    eval_main(source),
    SLValue::Enum {
      enum_: (0, 0),
      variant: 2,
      fields: vec![
        SLValue::String("hi".to_string()),
        SLValue::Cell(Box::new(SLValue::Int(7))),
      ],
    }
  );
}

#[test]
fn matches_enum_variants_and_binds_fields() {
  let source = "
      (enum Foo
        (Var1)
        (Var2 x:Int)
        (Var3 y:String z:(Cell Int)))
      (fn main () ->Int
        (let foo (new Foo::Var2 x:42))
        (match foo
          (Var1) => 1
          (Var2 x) => x
          (Var3 y z) => 2))";
  assert_eq!(eval_main(source), SLValue::Int(42));
}

#[test]
fn match_binds_fields_by_name_not_position() {
  let source = "
      (enum Foo
        (Var x:Int y:Int))
      (fn main () ->Int
        (let foo (new Foo::Var x:3 y:9))
        (match foo
          (Var y x) => (std::- y x)))";
  assert_eq!(eval_main(source), SLValue::Int(6));
}

#[test]
fn match_default_arm_handles_remaining_variants() {
  let source = "
      (enum Foo
        (Var1)
        (Var2 x:Int))
      (fn main () ->Int
        (let foo (new Foo::Var1))
        (match foo
          (Var2 x) => x
          _ => 5))";
  assert_eq!(eval_main(source), SLValue::Int(5));
}

#[test]
fn chained_struct_field_access() {
  let source = "
      (struct Point
        x:Int
        y:Int)
      (struct Box
        origin:Point
        size:Int)
      (fn main () ->Int
        (let b (new Box size:10 origin:(new Point x:4 y:5)))
        (std::+ b.origin.x b.origin.y))";
  assert_eq!(eval_main(source), SLValue::Int(9));
}

#[test]
fn extending_builtins() {
  let builtins = Builtins::new().with_builtin(Builtin::unary(
    "main",
    "add2",
    sig(&[], vec![TypeConst::Int], None, TypeConst::Int),
    |a| match a {
      Value::Int(n) => Ok(Value::Int(n + 2)),
      other => Err(format!("nope: {:?}", other)),
    },
  ));
  let source = "(fn main () ->Int (add2 3))".to_string();
  let package =
    compile_executable_from_source(&source, ("main", "main"), &builtins.specs(), &[]).unwrap();

  let interpreter = Interpreter::with_builtins(package, builtins);
  let mut exec = interpreter.call_main().unwrap();
  assert_eq!(exec.run_until_done().unwrap(), SLValue::Int(5));
}

#[test]
fn closure_bytecode() {
  let main = compiler::Function {
    num_locals: 1,
    num_params: 0,
    instructions: vec![
      Instruction::PushInt(42),
      Instruction::MakeFunctionRef((0, 0)),
      Instruction::PartialApply(1),
      Instruction::Return,
    ],
  };

  let inner = compiler::Function {
    num_locals: 1,
    num_params: 0,
    instructions: vec![Instruction::LoadLocal(0), Instruction::Return],
  };

  let pkg = Package {
    modules: vec![test_module(
      "main",
      vec![
        ("inner".to_string(), Callable::Function(inner)),
        ("main".to_string(), Callable::Function(main)),
      ],
    )],
    main: Some((0, 1)),
  };

  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();
  let result = exec.run_until_done().unwrap();
  let mut exec2 = interp.call_value(result, vec![]).unwrap();
  let result2 = exec2.run_until_done().unwrap();
  assert_eq!(result2, SLValue::Int(42));
}

#[test]
fn closure_end_to_end() {
  let source = "
      (fn outer () ->(Fn () -> Int)
        (let a 1)
        (fn inner () ->Int a)
        inner
      )
      (fn main () ->Int ((outer)))
    "
  .to_string();
  let pkg =
    compile_executable_from_source(&source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();
  assert_eq!(exec.run_until_done().unwrap(), SLValue::Int(1));
}

#[test]
fn nested_closure_end_to_end() {
  let source = "
      (fn outer () ->(Fn () -> (Fn () -> Int))
        (let a 1)
        (fn middle () ->(Fn () -> Int)
          (fn inner () ->Int a)
          inner)
        middle)
      (fn main () ->Int (((outer))))
    ";
  assert_eq!(eval_main(source), SLValue::Int(1));
}

#[test]
fn closure_captures_outer_parameter() {
  let source = "
      (fn outer (a:Int) ->(Fn () -> Int)
        (fn inner () ->Int a)
        inner)
      (fn main () ->Int ((outer 7)))
    ";
  assert_eq!(eval_main(source), SLValue::Int(7));
}

#[test]
fn same_named_nested_closures_in_different_factories_do_not_collide() {
  let source = "
      (fn make-a () ->(Fn () -> Int)
        (let x 1)
        (fn get () ->Int x)
        get)
      (fn make-b () ->(Fn () -> Int)
        (let x 2)
        (fn get () ->Int (std::+ x 10))
        get)
      (fn main () ->Int
        (std::+ ((make-a)) ((make-b))))
    ";
  assert_eq!(eval_main(source), SLValue::Int(13));
}

#[test]
fn closure_capture_order() {
  let source = "
      (fn outer () ->(Fn () -> Int)
        (let a 1)
        (let b 2)
        (fn inner () ->Int
          (let ignore b)
          a)
        inner)
      (fn main () ->Int ((outer)))
    ";
  assert_eq!(eval_main(source), SLValue::Int(1));
}

#[test]
fn non_capturing_closure_end_to_end() {
  let source = "
      (fn outer () ->(Fn () -> Int)
        (fn inner () ->Int 5)
        inner)
      (fn main () ->Int ((outer)))
    ";
  assert_eq!(eval_main(source), SLValue::Int(5));
}

#[test]
fn local_nested_function_can_be_called() {
  let source = "
      (fn outer () ->Int
        (fn inner () ->Int 5)
        (inner))
      (fn main () ->Int (outer))
    ";
  assert_eq!(eval_main(source), SLValue::Int(5));
}

#[test]
fn captured_local_nested_function_can_be_called() {
  let source = "
      (fn outer () ->(Fn () -> Int)
        (fn inner () ->Int 5)
        (fn caller () ->Int (inner))
        caller)
      (fn main () ->Int ((outer)))
    ";
  assert_eq!(eval_main(source), SLValue::Int(5));
}

#[test]
fn captured_nested_function_can_call_itself() {
  let source = "
      (fn make-countdown (base:Int) ->(Fn (Int) -> Int)
        (fn countdown (n:Int) ->Int
          (if (std::== n 0)
              base
              (countdown (std::- n 1)))))
      (fn main () ->Int
        (let countdown (make-countdown 7))
        (countdown 4))
    ";
  assert_eq!(eval_main(source), SLValue::Int(7));
}

#[test]
fn captured_nested_functions_can_call_each_other() {
  let source = "
      (fn make-parity (even-result:Int odd-result:Int) ->(Fn (Int) -> Int)
        (fn even (n:Int) ->Int
          (if (std::== n 0)
              even-result
              (odd (std::- n 1))))
        (fn odd (n:Int) ->Int
          (if (std::== n 0)
              odd-result
              (even (std::- n 1))))
        even)
      (fn main () ->Int
        (let parity (make-parity 10 20))
        (parity 5))
    ";
  assert_eq!(eval_main(source), SLValue::Int(20));
}

#[test]
fn recursive_function_parameter_can_shadow_a_sibling_name() {
  let source = "
      (fn make () ->(Fn (Int) -> Int)
        (fn a (b:Int) ->Int b)
        (fn b (n:Int) ->Int n)
        a)
      (fn main () ->Int
        (let f (make))
        (f 7))
    ";
  assert_eq!(eval_main(source), SLValue::Int(7));
}

#[test]
fn transitive_capture_can_share_a_name_with_a_source_parameter() {
  let source = "
      (fn make (x:Int) ->(Fn (Int) -> Int)
        (fn a (x:Int) ->Int
          (if (std::== x 0)
              999
              (b (std::- x 1))))
        (fn b (n:Int) ->Int
          (if (std::== n 0)
              x
              (a (std::- n 1))))
        a)
      (fn main () ->Int
        (let f (make 42))
        (f 1))
    ";
  assert_eq!(eval_main(source), SLValue::Int(42));
}

#[test]
fn function_definition_expression_returns_function() {
  let source = "
      (fn outer () ->(Fn () -> Int)
        (fn inner () ->Int 5))
      (fn main () ->Int ((outer)))
    ";
  assert_eq!(eval_main(source), SLValue::Int(5));
}

#[test]
fn step_advances_ip_and_leaves_value_on_stack() {
  let code = compiler::Function {
    num_locals: 0,
    num_params: 0,
    instructions: vec![Instruction::PushInt(7), Instruction::Return],
  };
  let mut exec = exec_with_function(code, vec![]);
  // After pushing the initial frame, there's one frame at ip 0.
  assert_eq!(exec.stack_len(), 0);
  exec.step().unwrap();
  // PushInt executed: stack has the value, frame still on the stack.
  assert_eq!(exec.peek_value().unwrap(), SLValue::Int(7));
  exec.step().unwrap();
  // Return popped the frame; the value remains on the stack.
  assert!(exec.is_done());
  assert_eq!(exec.peek_value().unwrap(), SLValue::Int(7));
  let result = exec.run_until_done().unwrap();
  assert_eq!(result, SLValue::Int(7));
}

#[test]
fn scalar_bytecode_pushes_do_not_allocate_gc_boxes() {
  let cases = [
    (
      vec![Instruction::PushVoid, Instruction::Return],
      SLValue::Void,
    ),
    (
      vec![Instruction::PushBool(true), Instruction::Return],
      SLValue::Bool(true),
    ),
    (
      vec![Instruction::PushInt(7), Instruction::Return],
      SLValue::Int(7),
    ),
    (
      vec![Instruction::PushFloat(1.5), Instruction::Return],
      SLValue::Float(1.5),
    ),
    (
      vec![Instruction::MakeFunctionRef((0, 0)), Instruction::Return],
      SLValue::FunctionRef(0, 0),
    ),
  ];

  for (instructions, expected) in cases {
    let code = compiler::Function {
      num_locals: 0,
      num_params: 0,
      instructions,
    };
    let mut exec = exec_with_function(code, vec![]);
    assert_eq!(exec.gc_count(), 0);
    assert_eq!(exec.run_until_done().unwrap(), expected);
    assert_eq!(exec.gc_count(), 0);
  }
}

#[test]
fn importing_scalar_slvalues_does_not_allocate_gc_boxes() {
  let cases = [
    SLValue::Void,
    SLValue::Bool(true),
    SLValue::Int(7),
    SLValue::Float(1.5),
    SLValue::FunctionRef(0, 0),
  ];

  for value in cases {
    let code = compiler::Function {
      num_locals: 1,
      num_params: 0,
      instructions: vec![Instruction::LoadLocal(0), Instruction::Return],
    };
    let mut exec = exec_with_function(code, vec![value.clone()]);
    assert_eq!(exec.gc_count(), 0);
    assert_eq!(exec.run_until_done().unwrap(), value);
    assert_eq!(exec.gc_count(), 0);
  }
}

#[test]
fn run_until_done_pops_final_value() {
  let code = compiler::Function {
    num_locals: 0,
    num_params: 0,
    instructions: vec![Instruction::PushInt(99), Instruction::Return],
  };
  let mut exec = exec_with_function(code, vec![]);
  let result = exec.run_until_done().unwrap();
  assert_eq!(result, SLValue::Int(99));
  // The stack should be empty after run_until_done pops the final value.
  assert_eq!(exec.stack_len(), 0);
}

#[test]
fn running_past_end_without_return_errors() {
  let code = compiler::Function {
    num_locals: 0,
    num_params: 0,
    instructions: vec![Instruction::PushInt(1)],
  };
  let mut exec = exec_with_function(code, vec![]);
  exec.step().unwrap(); // PushInt
  let err = exec.step().unwrap_err();
  assert!(err.contains("ran past end"), "unexpected error: {}", err);
}

#[test]
fn step_with_no_frames_errors() {
  let pkg = Package::default();
  let mut exec = Execution::new(pkg, default_builtins());
  let err = exec.step().unwrap_err();
  assert!(err.contains("no frames"), "unexpected error: {}", err);
}

#[test]
fn call_dynamic_on_non_callable_errors() {
  let interp = Interpreter::new(Package::default());
  let err = match interp.call_value(SLValue::Int(3), vec![]) {
    Ok(_) => panic!("expected an error calling a non-callable"),
    Err(err) => err,
  };
  assert!(err.contains("non-callable"), "unexpected error: {}", err);
}

#[test]
fn call_to_missing_module_errors() {
  let code = compiler::Function {
    num_locals: 0,
    num_params: 0,
    instructions: vec![Instruction::Call((1, 0), 0)],
  };
  let mut exec = exec_with_function(code, vec![]);
  let err = exec.step().unwrap_err();
  assert!(
    err.contains("Module not found"),
    "unexpected error: {}",
    err
  );
}

/// A dummy variadic builtin used to exercise the variadic-call mechanism:
/// it returns the number of arguments it received as an `Int`.
fn varargs_builtins() -> Builtins {
  default_builtins().with_builtin(Builtin::variadic(
    "std",
    "varargs",
    sig(&[], vec![], Some(TypeConst::Int), TypeConst::Int),
    |args| Ok(Value::Int(args.len() as i64)),
  ))
}

#[test]
fn variadic_builtin_called_with_zero_args() {
  let builtins = varargs_builtins();
  assert_eq!(
    eval_main_with(
      "(fn main () ->Int (let result:Int (std::varargs)))",
      builtins
    ),
    SLValue::Int(0)
  );
}

#[test]
fn variadic_builtin_called_with_multiple_args() {
  let builtins = varargs_builtins();
  assert_eq!(
    eval_main_with("(fn main () ->Int (std::varargs 1 2 3))", builtins),
    SLValue::Int(3)
  );
}

#[test]
fn variadic_builtin_called_with_one_arg() {
  let builtins = varargs_builtins();
  assert_eq!(
    eval_main_with("(fn main () ->Int (std::varargs 7))", builtins),
    SLValue::Int(1)
  );
}

#[test]
fn fixed_arity_builtin_rejects_wrong_arg_count() {
  // `+` is a fixed binary builtin; calling it with one arg should now be
  // caught at the call site via the carried arity.
  let src = "(fn main () ->Int (std::+ 1))";
  let err = compile_executable_from_source(src, ("main", "main"), &default_builtins().specs(), &[])
    .unwrap_err();
  assert!(
    err.contains("expects 2 arguments, got 1"),
    "unexpected error: {}",
    err
  );
}

#[test]
fn partial_apply_then_call_dynamic_binds_pre_bound_args() {
  // inner takes one param and returns it. main builds a Partial with the
  // arg pre-bound, then CallDynamic's it with no extra args on the stack.
  let inner = compiler::Function {
    num_locals: 1,
    num_params: 1,
    instructions: vec![Instruction::LoadLocal(0), Instruction::Return],
  };
  let main = compiler::Function {
    num_locals: 0,
    num_params: 0,
    instructions: vec![
      Instruction::PushInt(42),
      Instruction::MakeFunctionRef((0, 0)),
      Instruction::PartialApply(1),
      Instruction::CallDynamic(0),
      Instruction::Return,
    ],
  };
  let pkg = Package {
    modules: vec![test_module(
      "main",
      vec![
        ("inner".to_string(), Callable::Function(inner)),
        ("main".to_string(), Callable::Function(main)),
      ],
    )],
    main: Some((0, 1)),
  };
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();
  assert_eq!(exec.run_until_done().unwrap(), SLValue::Int(42));
}

#[test]
fn function_call_leaves_no_stack_garbage() {
  // After main returns, the execution's value stack must contain exactly
  // one value: main's return value. Intermediate let-bindings and function
  // call results should not accumulate as garbage below it.
  let source = "
      (fn id (a:Int) ->Int a)
      (fn waste () ->Int
        (let a 1)
        (id 1)
        (let b 2)
        b)
      (fn main () ->Int (waste) (waste) (waste))
    ";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let mut exec = Interpreter::new(pkg).call_main().unwrap();
  let result = exec.run_until_done().unwrap();
  assert_eq!(result, SLValue::Int(2));
  // run_until_done pops the final value, so the stack should be empty.
  assert_eq!(exec.stack_len(), 0);
}

#[test]
fn if_does_not_evaluate_unselected_branch() {
  // If the else branch were evaluated, calling the unimplemented
  // `boom` builtin would error at runtime. Since the then branch is selected,
  // `boom` is never called and the program returns 42 cleanly. `boom` is
  // registered as a builtin (with an erroring handler) so the call links,
  // but its behavior is never invoked.
  let source = "(fn main () ->Int (if (std::== 1 1) 42 (boom 0)))";
  let builtins = default_builtins().with_builtin(Builtin::unary(
    "main",
    "boom",
    sig(&[], vec![TypeConst::Int], None, TypeConst::Int),
    |_| Err("boom".into()),
  ));
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &builtins.specs(), &[]).unwrap();
  let interp = Interpreter::with_builtins(pkg, builtins);
  let mut exec = interp.call_main().unwrap();
  assert_eq!(exec.run_until_done().unwrap(), SLValue::Int(42));
}

#[test]
fn if_rejects_non_bool_condition() {
  let source = "(fn main () ->Int (if 1 42 0))";
  let err =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap_err();
  assert!(err.contains("expected `Bool`, got `Int`"), "{err}");
}

#[test]
fn run_completes_in_one_call() {
  let source = "(fn main () ->Int 5)";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();
  let status = exec.run(1_000).unwrap();
  assert_eq!(status, Status::Done(SLValue::Int(5)));
}

#[test]
fn run_pauses_and_resumes() {
  let source = "
      (fn main () ->Int
        (let a 1) (let b 2) (let c 3) (let d 4) 5)";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();
  let status = exec.run(3).unwrap();
  assert_eq!(status, Status::Paused);
  assert_eq!(exec.executed, 3);
  assert!(!exec.is_done());
  let status = exec.run(1_000).unwrap();
  assert_eq!(status, Status::Done(SLValue::Int(5)));
}

#[test]
fn run_hits_limit_exactly() {
  // (std::+ 1 2) is 4 bytecodes: PushInt(1), PushInt(2), Call(std::+), Return.
  let source = "(fn main () ->Int (std::+ 1 2))";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();
  let status = exec.run(4).unwrap();
  assert_eq!(status, Status::Done(SLValue::Int(3)));
  assert_eq!(exec.executed, 4);
}

#[test]
fn run_limited_mid_program_pauses() {
  let source = "(fn main () ->Int (std::+ 1 2))";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();
  let status = exec.run(2).unwrap();
  assert_eq!(status, Status::Paused);
  let status = exec.run(1_000).unwrap();
  assert_eq!(status, Status::Done(SLValue::Int(3)));
  assert_eq!(exec.executed, 4);
}

#[test]
fn infinite_recursion_is_bounded_by_run_budget() {
  let source = "(fn loop (n:Int) ->Int (loop n)) (fn main () ->Int (loop 1))";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();
  let status = exec.run(10_000).unwrap();
  assert_eq!(status, Status::Paused);
  assert!(!exec.is_done());
}

#[test]
fn executed_count_is_cumulative() {
  let source = "(fn main () ->Int (let a 1) (let b 2) (let c 3) 4)";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();
  exec.run(2).unwrap();
  assert_eq!(exec.executed, 2);
  exec.run(2).unwrap();
  assert_eq!(exec.executed, 4);
  let s3 = exec.run(1_000).unwrap();
  assert!(matches!(s3, Status::Done(_)));
  assert_eq!(exec.executed, 14);
}

#[test]
fn two_executions_run_in_parallel() {
  // Two `Execution`s from one `Interpreter` should be independent: stepping
  // one does not affect the other's stack, frames, or counter. We interleave
  // `run` calls (cooperative scheduling) and confirm each produces its own
  // result. Each `main` returns its distinct constant so we can tell them
  // apart.
  let source = "(fn main () ->Int (let a 1) (let b 2) 3)";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let interp = Interpreter::new(pkg);
  let mut exec_a = interp.call_main().unwrap();
  let mut exec_b = interp.call_main().unwrap();

  // Run a couple of bytecodes on each, alternating, before either is done.
  assert_eq!(exec_a.run(1).unwrap(), Status::Paused);
  assert_eq!(exec_b.run(1).unwrap(), Status::Paused);
  assert_eq!(exec_a.executed, 1);
  assert_eq!(exec_b.executed, 1);

  // Run both to completion in the same interleaved fashion.
  assert_eq!(exec_a.run(1_000).unwrap(), Status::Done(SLValue::Int(3)));
  assert_eq!(exec_b.run(1_000).unwrap(), Status::Done(SLValue::Int(3)));
  assert!(exec_a.is_done());
  assert!(exec_b.is_done());
}

#[test]
fn run_for_duration_completes() {
  // A trivial program completes well within a generous duration.
  let source = "(fn main () ->Int 5)";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();
  let status = exec.run_for_duration(Duration::from_secs(1)).unwrap();
  assert_eq!(status, Status::Done(SLValue::Int(5)));
}

#[test]
fn run_for_duration_pauses_on_infinite_recursion() {
  // A non-terminating program is bounded by the time budget and pauses.
  let source = "(fn loop (n:Int) ->Int (loop n)) (fn main () ->Int (loop 1))";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();
  let status = exec.run_for_duration(Duration::from_millis(50)).unwrap();
  assert_eq!(status, Status::Paused);
  assert!(!exec.is_done());
  // It should have made progress.
  assert!(exec.executed > 0);
}

#[test]
fn collect_all_frees_unreachable_values() {
  // A program that creates garbage: `main` builds a Cell holding 42, then
  // discards it by returning `99`. After execution, the Cell should be
  // unreachable from the root. Forcing a full collection cycle should
  // reclaim those allocations.
  let source = "(fn main () ->Int (let garbage (std::cell 42)) 99)";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();

  // `call_main` enters the function. Void locals do not allocate.
  let baseline = exec.gc_count();
  assert_eq!(baseline, 0);

  // Step through the program manually so we can observe allocations before
  // the final value is popped (which would make it unreachable). Completion
  // leaves the int `99` on the stack and the Cell dead in a local.
  while !exec.is_done() {
    exec.step().unwrap();
  }
  assert!(exec.is_done());
  assert_eq!(exec.peek_value().unwrap(), SLValue::Int(99));

  // Forcing a full collection cycle must reclaim the unreachable Cell. The
  // int `99` on the stack is immediate and does not keep a GC box alive.
  exec.collect_all();
  let after_collect = exec.gc_count();
  assert_eq!(
    after_collect, 0,
    "expected no live Gc allocations after collect_all, got {}",
    after_collect,
  );
}

#[test]
fn gc_reclaims_intermediate_values_during_long_run() {
  // A program that produces a lot of garbage: it loops `n` times, each
  // iteration building a Cell and immediately discarding it. If the GC is
  // working, the live allocation count after the run should stay bounded
  // (proportional to the final stack, not to the number of iterations),
  // rather than growing linearly with `n`.
  let source = "
      (fn waste (n:Int) ->Int
        (if (std::== n 0)
          0
          (block
            (let garbage (std::cell n))
            (waste (std::- n 1)))))
      (fn main () ->Int (waste 1000))
    ";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();

  let result = exec.run_until_done().unwrap();
  assert_eq!(result, SLValue::Int(0));

  // After 1000 iterations each allocating a Cell, if GC never ran we'd
  // expect ~1000+ live allocations. Because `run_until_done` calls
  // `collect_debt` and the program allocates enough to trigger incremental
  // collection along the way, the count should be far smaller than 1000.
  // Forcing a final collection reclaims whatever transient garbage remains,
  // leaving only the final stack value.
  exec.collect_all();
  let live = exec.gc_count();
  assert!(
    live < 100,
    "expected bounded live Gc count after collecting garbage from 1000 iterations, got {}",
    live,
  );
}

#[test]
fn live_values_survive_collection() {
  // A program that returns a Cell: the Cell must survive a forced
  // collection, proving that reachable `Gc` pointers are not reclaimed.
  let source = "(fn main () ->(Cell Int) (std::cell 3))";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();

  // Step to completion. The Cell ends up on the stack as the result
  // (is_done() is true, but we haven't popped it).
  while !exec.is_done() {
    exec.step().unwrap();
  }
  assert!(exec.is_done());
  assert!(matches!(exec.peek_value().unwrap(), SLValue::Cell(_)));

  // The Cell on the stack is reachable and must survive a full collection.
  // Its inner Int is immediate, so only the cell box itself is live.
  exec.collect_all();
  let after = exec.gc_count();
  assert!(
    after >= 1,
    "expected the Cell to survive collection, got {} live allocations",
    after,
  );

  // And the value must still be readable after collection.
  assert!(matches!(exec.peek_value().unwrap(), SLValue::Cell(_)));
}

#[test]
fn cell_wrapping_immediate_allocates_one_gc_box() {
  let source = "(fn main () ->(Cell Int) (std::cell 3))";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();

  while !exec.is_done() {
    exec.step().unwrap();
  }
  exec.collect_all();
  assert_eq!(
    exec.gc_count(),
    1,
    "a Cell holding an immediate Int should only allocate the RefLock cell box"
  );
}

#[test]
fn memory_limit_defaults_to_none() {
  let pkg = Package::default();
  let mut exec = Execution::new(pkg, default_builtins());
  assert!(exec.memory_limit().is_none());
  assert_eq!(exec.memory_usage(), exec.gc_allocation_bytes());
  exec.set_memory_limit(Some(123));
  assert_eq!(exec.memory_limit(), Some(123));
  exec.set_memory_limit(None);
  assert!(exec.memory_limit().is_none());
}

#[test]
fn memory_limit_errors_when_exceeded() {
  let source = "(fn main () ->Int (let garbage (std::cell 42)) 99)";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();
  // 1 byte is far below any allocation; the first step after entering main
  // (which has allocated a Void local in the GC) must immediately trip the
  // limit.
  exec.set_memory_limit(Some(1));
  let err = exec.run_until_done().unwrap_err();
  assert!(
    err.contains("memory limit exceeded"),
    "unexpected error: {}",
    err
  );
}

#[test]
fn memory_limit_allows_small_program() {
  // A trivial program that only pushes an int and returns should fit well
  // under a generous limit and complete normally.
  let source = "(fn main () ->Int 5)";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();
  exec.set_memory_limit(Some(1024 * 1024));
  let result = exec.run_until_done().unwrap();
  assert_eq!(result, SLValue::Int(5));
}

#[test]
fn memory_limit_triggers_during_unbounded_allocation() {
  // A program that builds ever-growing lists: `loop(n)` returns a list of `n`
  // ints by repeatedly `push`ing onto an accumulator. With a tight memory
  // limit, the run must error instead of running to completion.
  let source = "
      (fn loop (n:Int acc:(List Int)) ->(List Int)
        (if (std::== n 0)
          acc
          (loop (std::- n 1) (std::push acc n))))
      (fn main () ->(List Int) (loop 100000 (std::list)))
    ";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();
  exec.set_memory_limit(Some(64 * 1024));
  let err = exec.run_until_done().unwrap_err();
  assert!(
    err.contains("memory limit exceeded"),
    "unexpected error: {}",
    err
  );
}

#[test]
fn memory_limit_catches_unbounded_stack_and_frames() {
  // A deliberately adversarial program: `spin` recurses forever, taking no
  // arguments and allocating nothing on the GC heap. Every recursive call
  // pushes a new `Frame` — and each `Frame` clones the function's
  // `instructions: Vec<…>` and allocates a fresh `locals: Vec<Gc<_>>` (even
  // if empty, the `Vec` header / capacity lives on `ExecRoot`'s heap) — and
  // grows the value stack by one slot for the pending call result. These are
  // *plain Rust `Vec` allocations owned by `ExecRoot`*, not `Gc` allocations,
  // so they don't show up in `Metrics::total_gc_allocation`.
  //
  // The live GC heap stays at essentially zero (no `Gc::new` runs inside
  // `spin`), so the current GC-only memory limit never fires — even though
  // 200 000 piled-up frames consume ~24 MB of real Rust heap. A *correct*
  // memory limit would trip. This test asserts that the run errors with
  // "memory limit exceeded"; it currently FAILS because the limit is never
  // reached. Once stack/frame overhead is tracked, the error will fire and
  // the test will pass.
  let source = "
      (fn spin () ->Int (spin))
      (fn main () ->Int (spin))
    ";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();
  // 64 KiB is far below what 200 000 cloned instruction vectors + frame
  // slots actually consume (~24 MB), yet comfortably above this program's
  // live GC heap at any moment.
  exec.set_memory_limit(Some(64 * 1024));
  // Drive execution in small batches so `collect_debt` reclaims the per-frame
  // `Gc<Void>` garbage and the live GC heap stays bounded — while the frame
  // objects (cloning `instructions: Vec<Instruction>`) pile up unboundedly
  // on the Rust heap. The loop expects one of the `run` calls to error with
  // "memory limit exceeded"; today none does, so the loop completes and the
  // final `unwrap_err` / unreachable panic fails the test.
  let mut hit = false;
  for _ in 0..2_000 {
    match exec.run(100) {
      Ok(_) if exec.is_done() => panic!("spin terminated unexpectedly"),
      Ok(_) => {}
      Err(e) => {
        assert!(
          e.contains("memory limit exceeded"),
          "unexpected error: {}",
          e,
        );
        hit = true;
        break;
      }
    }
  }
  assert!(
      hit,
      "expected the memory limit to fire, but the run completed with only {} bytes of live GC (limit {}); \
       the GC-only limit does not see the ~200 000 piled-up frames' Rust-heap overhead",
      exec.gc_allocation_bytes(),
      64 * 1024,
    );
}

#[test]
fn memory_limit_catches_large_string_contents() {
  // `SLVal::String(String)` stores the `String` inline inside the `Gc`-boxed
  // `Accounted`. gc-arena counts only the box layout (the `Accounted` struct
  // + `SLVal` enum + `String`'s 24-byte ptr/len/cap header) toward
  // `total_gc_allocation`. The actual string *bytes* live on the ordinary
  // Rust heap and are invisible to gc-arena's `Metrics` — but `Accounted`
  // charges them to the per-execution `MemoryTracker` via `external_bytes_of`
  // (which returns `String::capacity()`), so the limit sees them.
  //
  // This program doubles a string on each recursive call: after 25
  // iterations the string is 2^25 = 32 MB of actual bytes. A tight memory
  // limit catches it because the `Accounted` wrapper charges the string's
  // capacity to the tracker, and the limit check sums
  // `total_gc_allocation + tracker.external_bytes()`.
  let source = "
      (fn grow (s:String n:Int) ->String
        (if (std::== n 0)
          s
          (grow (std::concat s s) (std::- n 1))))
      (fn main () ->String (grow \"x\" 25))
    ";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();
  exec.set_memory_limit(Some(64 * 1024));
  let mut hit = false;
  for _ in 0..10_000 {
    match exec.run(50) {
      Ok(Status::Done(_)) => break,
      Ok(_) => {}
      Err(e) => {
        assert!(
          e.contains("memory limit exceeded"),
          "unexpected error: {}",
          e,
        );
        hit = true;
        break;
      }
    }
  }
  assert!(
      hit,
      "expected the memory limit to fire for a 32 MB string, but the run completed (gc_bytes={}, limit {})",
      exec.gc_allocation_bytes(),
      64 * 1024,
    );
}

#[test]
fn memory_limit_catches_large_string_in_closure_capture() {
  // The program builds a 2^20 = 1 MiB string, then a closure that captures it.
  // `main` returns the closure, so the captured string stays reachable. We
  // step `main` to completion *without* a limit (so the string builds
  // unhindered) — but use `step` rather than `run_until_done` so the closure
  // stays live on the arena's value stack rather than being popped out. Then
  // we force collection of transient garbage and check that the original
  // string object remains reachable and charged through the closure.
  let source = "
      (fn grow (s:String n:Int) ->String
        (if (std::== n 0)
          s
          (grow (std::concat s s) (std::- n 1))))
      (fn make-holder (big:String) ->(Fn () -> String)
        (fn holder () ->String big)
        holder)
      (fn main () ->(Fn () -> String)
        (let big (grow \"x\" 20))
        (make-holder big))
    ";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();
  // Step to completion with no limit: builds the 1 MiB string and closure.
  // Stepping leaves the final value (the closure) on the arena's value stack,
  // so the captured string stays reachable for the subsequent collection + check.
  while !exec.is_done() {
    exec.step().unwrap();
  }
  // The closure (Partial) is now on top of the arena's value stack, holding
  // the captured 1 MiB string.
  assert!(
    matches!(exec.peek_value().unwrap(), SLValue::Partial { .. }),
    "expected a closure (Partial) on the stack from main",
  );
  // Force collection to reclaim intermediate strings from `grow`. The
  // original `Accounted` for `big` must survive because the closure points to it.
  exec.collect_all();
  let usage_after = exec.memory_usage();
  // gc-arena's `total_gc_allocation` contributes only a few hundred bytes of
  // `Gc` box headers; the original string's `Accounted` charge supplies the
  // remaining external bytes.
  assert!(
    usage_after >= 1_000_000,
    "expected the captured 1 MiB string to be charged, but memory_usage was only {} bytes",
    usage_after,
  );
  // Sanity: the gc-arena-reported portion is tiny (box headers only).
  let gc_only = exec.gc_allocation_bytes();
  assert!(
    gc_only < 1_000,
    "expected gc-arena to report only box headers (< 1 KiB), got {} bytes",
    gc_only,
  );
  // The bulk of the charge remains the shared string's external buffer.
  let external = exec.memory_usage() - gc_only;
  assert!(
    external >= 1_000_000,
    "expected >= 1 MiB of captured external bytes, got {}",
    external,
  );
}

#[test]
fn cell_operations_do_not_clone_large_string_payloads() {
  let large = "x".repeat(64 * 1024);
  let source = format!("(fn main () ->String (let c (std::cell \"{large}\")) (std::get c))");
  let pkg =
    compile_executable_from_source(&source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();

  // `std::get` should return the existing string value from the cell. If it
  // cloned the 64 KiB payload, this tight-but-sufficient limit would be
  // exceeded by the duplicate external allocation.
  exec.set_memory_limit(Some(96 * 1024));
  assert_eq!(exec.run_until_done().unwrap(), SLValue::String(large));
}

#[test]
fn explicit_cell_is_shared_across_closures() {
  let source = "
      (fn counter () ->(Fn () -> Int)
        (let count (std::cell 0))
        (fn inc () ->Int
          (std::set! count (std::+ 1 (std::get count)))
          (std::get count))
        inc)
      (fn main () ->Int
        (let c (counter))
        (c)
        (c)
        (c))
    ";
  assert_eq!(eval_main(source), SLValue::Int(3));
}

#[test]
fn explicit_set_returns_void() {
  assert_eq!(
    eval_main("(fn main () ->Void (let x (std::cell 1)) (std::set! x 11))"),
    SLValue::Void
  );
}

#[test]
fn explicit_set_on_non_cell_errors() {
  let source = "(fn main () ->Int (std::set! 1 2))";
  let err =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap_err();
  assert!(err.contains("expected `(Cell"), "unexpected error: {}", err);
}

#[test]
fn cycle_is_collected() {
  // Build a cycle using only source-level constructs: an explicit Cell that
  // holds a closure which captures that same Cell. Once the result is
  // discarded, the cycle is unreachable and must be collected. This is the
  // key advantage of gc-arena over `Rc` (which would leak cycles).
  //
  // (fn make-cycle () ->(Fn () -> Int)
  //   (fn placeholder () ->Int 0)
  //   (let c (std::cell placeholder))
  //   (fn self () ->Int (let ignored:(Fn () -> Int) (std::get c)) 1)
  //   (std::set! c self)  ; Cell↔Partial cycle
  //   self)              ; return the cyclic closure
  // (fn main () ->Int (make-cycle) 99)  ; discard the cycle, return 99
  let source = "
      (fn make-cycle () ->(Fn () -> Int)
        (fn placeholder () ->Int 0)
        (let c (std::cell placeholder))
        (fn self () ->Int
          (let ignored:(Fn () -> Int) (std::get c))
          1)
        (std::set! c self)
        self)
      (fn main () ->Int (make-cycle) 99)
    ";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();

  // Run to completion: main calls make-cycle (which builds the cycle and
  // returns it on the stack), then main discards it by returning 99.
  let result = exec.run_until_done().unwrap();
  assert_eq!(result, SLValue::Int(99));

  // At this point the stack is empty (run_until_done popped 99), so the
  // cycle is unreachable. Forcing a full collection must reclaim it.
  exec.collect_all();
  let after = exec.gc_count();
  assert_eq!(
    after, 0,
    "expected 0 live Gc allocations after collecting the cycle, got {}",
    after,
  );
}

#[test]
fn list_self_reference_cycle_is_rejected_by_occurs_check() {
  // A list that contains a cell which in turn refers back to the same list
  // forms a cycle (List ↔ Cell). This is the list analogue of
  // `cycle_is_collected` (which builds a Partial↔Cell cycle via closures).
  let source = "
      (fn make-cycle ()
        (let l (std::cell (std::list)))
        (fn mkcycle ()
          (std::set! l (std::list l))
          (std::get l))
        mkcycle)
      (fn main () ->Int ((make-cycle)) 99)
    ";
  let error =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap_err();
  assert!(error.contains("infinite type"), "{error}");
}

#[test]
fn pacing_debt_is_positive_only() {
  // Direct unit test of the pacing-debt invariant: `charge` accumulates
  // positive debt; `release` does *not* subtract from it; `drain_pacing_debt`
  // returns the accumulated total and resets it to 0. This is the invariant
  // that prevents the stale-negative-debt bug (where a sweep-time
  // `release`, applied as a negative `Metrics::adjust_debt` after the GC
  // cycle reset, would suppress collection on the fresh cycle).
  let tracker = MemoryTracker::new();

  // charge accumulates positive debt.
  tracker.charge(100);
  assert_eq!(tracker.drain_pacing_debt(), 100);

  // after draining, the accumulator is 0.
  assert_eq!(tracker.drain_pacing_debt(), 0);

  // release does NOT contribute negative debt.
  tracker.charge(100);
  tracker.release(100);
  assert_eq!(tracker.drain_pacing_debt(), 100);

  // a subsequent charge still adds, unaffected by the earlier release.
  tracker.charge(50);
  assert_eq!(tracker.drain_pacing_debt(), 50);

  // external_bytes still tracks live bytes independently of pacing debt.
  // The sequence: charge(100)→100, charge(100)→200, release(100)→100,
  // charge(50)→150. Drains don't affect external_bytes.
  assert_eq!(tracker.external_bytes(), 150);
}

#[test]
fn temporary_reservation_is_scoped_to_one_execution_and_released() {
  let mut first = Execution::new(Package::default(), default_builtins());
  let second = Execution::new(Package::default(), default_builtins());
  let first_before = first.memory_usage();
  let second_before = second.memory_usage();

  first.arena.mutate_root(|mc, root| {
    let reservation = root.reserve_memory(mc, 128).unwrap();
    assert_eq!(reservation.bytes(), 128);
    assert_eq!(root.memory_usage(mc).unwrap(), first_before + 128);
    assert_eq!(root.tracker.drain_pacing_debt(), 0);
    drop(reservation);
    assert_eq!(root.memory_usage(mc).unwrap(), first_before);
  });

  assert_eq!(first.memory_usage(), first_before);
  assert_eq!(second.memory_usage(), second_before);
}

#[test]
fn temporary_reservation_rejects_limit_and_reconciles_capacity() {
  let mut exec = Execution::new(Package::default(), default_builtins());
  let baseline = exec.memory_usage();
  exec.set_memory_limit(Some(baseline + 32));

  exec.arena.mutate_root(|mc, root| {
    let mut reservation = root.reserve_memory(mc, 16).unwrap();
    root
      .reconcile_reservation(mc, &mut reservation, 32)
      .unwrap();
    assert_eq!(root.memory_usage(mc).unwrap(), baseline + 32);

    let err = root
      .reconcile_reservation(mc, &mut reservation, 33)
      .unwrap_err();
    assert!(err.contains("memory limit exceeded"), "unexpected: {err}");
    assert_eq!(reservation.bytes(), 32);

    root.reconcile_reservation(mc, &mut reservation, 8).unwrap();
    assert_eq!(root.memory_usage(mc).unwrap(), baseline + 8);
  });

  assert_eq!(exec.memory_usage(), baseline);
}

#[test]
fn reservation_cannot_be_reconciled_by_another_execution() {
  let mut first = Execution::new(Package::default(), default_builtins());
  let mut second = Execution::new(Package::default(), default_builtins());
  let first_before = first.memory_usage();

  let mut reservation = first
    .arena
    .mutate_root(|mc, root| root.reserve_memory(mc, 24).unwrap());
  let err = second.arena.mutate_root(|mc, root| {
    root
      .reconcile_reservation(mc, &mut reservation, 48)
      .unwrap_err()
  });
  assert!(
    err.contains("different execution"),
    "unexpected error: {err}"
  );
  drop(reservation);
  assert_eq!(first.memory_usage(), first_before);
}

#[test]
fn list_empty() {
  assert_eq!(
    eval_main("(fn main () ->(List Int) (let result:(List Int) (std::list)))"),
    SLValue::List(vec![])
  );
}

#[test]
fn list_with_ints() {
  assert_eq!(
    eval_main("(fn main () ->(List Int) (std::list 1 2 3))"),
    SLValue::List(vec![SLValue::Int(1), SLValue::Int(2), SLValue::Int(3)])
  );
}

#[test]
fn variadic_builtin_can_be_called_through_local_binding() {
  let source = "
      (fn main () ->(List Int)
        (let make std::list)
        (make 1 2 3))
    ";
  assert_eq!(
    eval_main(source),
    SLValue::List(vec![SLValue::Int(1), SLValue::Int(2), SLValue::Int(3)])
  );
}

#[test]
fn variadic_builtin_can_be_passed_to_annotated_parameter() {
  let source = "
      (fn use-list (make:(Fn (...Int) -> (List Int))) ->(List Int)
        (make 1 2 3))
      (fn main () ->(List Int)
        (use-list std::list))
    ";
  assert_eq!(
    eval_main(source),
    SLValue::List(vec![SLValue::Int(1), SLValue::Int(2), SLValue::Int(3)])
  );
}

#[test]
fn top_level_function_is_a_first_class_value() {
  let source = "
      (fn double (x:Int) ->Int (std::+ x x))
      (fn main () ->(List Int) (std::map (std::list 1 2 3) double))
    ";
  assert_eq!(
    eval_main(source),
    SLValue::List(vec![SLValue::Int(2), SLValue::Int(4), SLValue::Int(6)])
  );
}

#[test]
fn top_level_function_can_be_called_through_a_local() {
  let source = "
      (fn double (x:Int) ->Int (std::+ x x))
      (fn main () ->Int (let f double) (f 4))
    ";
  assert_eq!(eval_main(source), SLValue::Int(8));
}

#[test]
fn local_shadows_top_level_function_in_value_position() {
  let source = "
      (fn transform (x:Int) ->Int (std::+ x x))
      (fn identity (x:Int) ->Int x)
      (fn main () ->(List Int)
        (let transform identity)
        (std::map (std::list 1 2 3) transform))
    ";
  assert_eq!(
    eval_main(source),
    SLValue::List(vec![SLValue::Int(1), SLValue::Int(2), SLValue::Int(3)])
  );
}

#[test]
fn qualified_function_is_a_first_class_value() {
  let source = "
      (fn double (x:Int) ->Int (std::+ x x))
      (fn main () ->(List Int) (std::map (std::list 2 3) main::double))
    ";
  assert_eq!(
    eval_main(source),
    SLValue::List(vec![SLValue::Int(4), SLValue::Int(6)])
  );
}

#[test]
fn builtin_is_a_first_class_value() {
  let source = "
      (fn main () ->(List Int)
        (std::map (std::list (std::list 1) (std::list 1 2)) std::len))
    ";
  assert_eq!(
    eval_main(source),
    SLValue::List(vec![SLValue::Int(1), SLValue::Int(2)])
  );
}

#[test]
fn explicit_cell_get_set_and_nesting() {
  let source = "
      (fn main () ->Int
        (let inner (std::cell 1))
        (let outer (std::cell inner))
        (std::set! (std::get outer) 7)
        (std::get (std::get outer)))
    ";
  assert_eq!(eval_main(source), SLValue::Int(7));
}

#[test]
fn explicit_get_on_non_cell_errors() {
  let err = eval_main_err("(fn main () ->Int (std::get 1))");
  assert!(err.contains("expected `(Cell"), "unexpected error: {err}");
}

#[test]
fn bare_set_resolves_to_std_builtin_not_special_form() {
  let err = compile_executable_from_source(
    "(fn main () ->Int (let x 1) (set! x 2))",
    ("main", "main"),
    &default_builtins().specs(),
    &[("std", "set!")],
  )
  .unwrap_err();
  assert!(err.contains("expected `(Cell"), "unexpected: {err}");
}

#[test]
fn list_first_class_as_callable_value() {
  // `list` is a builtin (not a special form): its FunctionRef can be pushed
  // onto the stack and invoked via CallDynamic, exactly as a future `(let l
  // std::list)` binding would compile to once the compiler supports
  // referencing builtins as values.
  let std_mod = 1u32; // "std" is the second module after "main"
                      // `list` is registered after `+`, `-`, `==`, `concat`, so its function
                      // index in the `std` module is 4.
  let std_list = 4u32;
  let main = compiler::Function {
    num_locals: 0,
    num_params: 0,
    instructions: vec![
      Instruction::PushInt(1),
      Instruction::PushInt(2),
      Instruction::PushInt(3),
      Instruction::MakeFunctionRef((std_mod, std_list)),
      Instruction::CallDynamic(3),
      Instruction::Return,
    ],
  };
  let pkg = Package {
    modules: vec![
      test_module("main", vec![("main".to_string(), Callable::Function(main))]),
      test_module(
        "std",
        vec![
          ("+".to_string(), Callable::Builtin),
          ("-".to_string(), Callable::Builtin),
          ("==".to_string(), Callable::Builtin),
          ("concat".to_string(), Callable::Builtin),
          ("list".to_string(), Callable::Builtin),
        ],
      ),
    ],
    main: Some((0, 0)),
  };
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();
  assert_eq!(
    exec.run_until_done().unwrap(),
    SLValue::List(vec![SLValue::Int(1), SLValue::Int(2), SLValue::Int(3)])
  );
}

#[test]
fn concat_empty_lists() {
  assert_eq!(
    eval_main(
      "(fn main () ->(List Int)
           (let left:(List Int) (std::list))
           (let right:(List Int) (std::list))
           (std::concat left right))"
    ),
    SLValue::List(vec![])
  );
}

#[test]
fn concat_lists() {
  assert_eq!(
    eval_main("(fn main () ->(List Int) (std::concat (std::list 1 2) (std::list 3 4)))"),
    SLValue::List(vec![
      SLValue::Int(1),
      SLValue::Int(2),
      SLValue::Int(3),
      SLValue::Int(4)
    ])
  );
}

#[test]
fn concat_list_mismatch_errors() {
  let err = eval_main_err("(fn main () ->Int (std::concat (std::list 1) \"x\"))");
  assert!(
    err.contains("expected `(List Int)`"),
    "unexpected error: {}",
    err
  );
}

#[test]
fn list_equality() {
  assert_eq!(
    eval_main("(fn main () ->Bool (std::== (std::list 1 2 3) (std::list 1 2 3)))"),
    SLValue::Bool(true)
  );
  assert_eq!(
    eval_main("(fn main () ->Bool (std::== (std::list 1 2 3) (std::list 1 2 4)))"),
    SLValue::Bool(false)
  );
  assert_eq!(
    eval_main("(fn main () ->Bool (std::== (std::list 1) (std::list 1 2)))"),
    SLValue::Bool(false)
  );
}

#[test]
fn idx_list_positive() {
  assert_eq!(
    eval_main("(fn main () ->Int (std::idx (std::list 10 20 30) 0))"),
    SLValue::Int(10)
  );
  assert_eq!(
    eval_main("(fn main () ->Int (std::idx (std::list 10 20 30) 2))"),
    SLValue::Int(30)
  );
}

#[test]
fn idx_list_negative() {
  assert_eq!(
    eval_main("(fn main () ->Int (std::idx (std::list 10 20 30) -1))"),
    SLValue::Int(30)
  );
  assert_eq!(
    eval_main("(fn main () ->Int (std::idx (std::list 10 20 30) -3))"),
    SLValue::Int(10)
  );
}

#[test]
fn idx_list_out_of_range_errors() {
  let err = eval_main_err("(fn main () ->Int (std::idx (std::list 1 2) 5))");
  assert!(err.contains("out of range"), "unexpected error: {}", err);
  let err = eval_main_err("(fn main () ->Int (std::idx (std::list 1 2) -3))");
  assert!(err.contains("out of range"), "unexpected error: {}", err);
}

#[test]
fn slice_string_replaces_character_indexing() {
  assert_eq!(
    eval_main("(fn main () ->String (std::slice \"hello\" 0 1))"),
    SLValue::String("h".to_string())
  );
  assert_eq!(
    eval_main("(fn main () ->String (std::slice \"hello\" 4 5))"),
    SLValue::String("o".to_string())
  );
  assert_eq!(
    eval_main("(fn main () ->String (std::slice \"hello\" -1 5))"),
    SLValue::String("o".to_string())
  );
}

#[test]
fn idx_string_errors() {
  let err = eval_main_err("(fn main () ->Int (std::idx \"hi\" 0))");
  assert!(err.contains("expected `(List"), "unexpected error: {err}");
}

#[test]
fn push_returns_new_list() {
  assert_eq!(
    eval_main("(fn main () ->(List Int) (std::push (std::list 1 2) 3))"),
    SLValue::List(vec![SLValue::Int(1), SLValue::Int(2), SLValue::Int(3)])
  );
  // empty + one
  assert_eq!(
    eval_main("(fn main () ->(List Int) (std::push (std::list) 7))"),
    SLValue::List(vec![SLValue::Int(7)])
  );
}

#[test]
fn push_is_non_mutating() {
  // The original list is unaffected: indexing it afterwards still works.
  let source = "(fn main () ->Int (let l (std::list 1 2)) (std::push l 3) (std::idx l 1))";
  assert_eq!(eval_main(source), SLValue::Int(2));
}

#[test]
fn slice_list_basic() {
  assert_eq!(
    eval_main("(fn main () ->(List Int) (std::slice (std::list 1 2 3 4 5) 1 4))"),
    SLValue::List(vec![SLValue::Int(2), SLValue::Int(3), SLValue::Int(4)])
  );
}

#[test]
fn slice_list_negative_indices() {
  assert_eq!(
    eval_main("(fn main () ->(List Int) (std::slice (std::list 1 2 3 4 5) -3 -1))"),
    SLValue::List(vec![SLValue::Int(3), SLValue::Int(4)])
  );
}

#[test]
fn slice_list_clamped_bounds() {
  // stop past the end clamps to length.
  assert_eq!(
    eval_main("(fn main () ->(List Int) (std::slice (std::list 1 2 3) 1 100))"),
    SLValue::List(vec![SLValue::Int(2), SLValue::Int(3)])
  );
  // start before 0 clamps to 0.
  assert_eq!(
    eval_main("(fn main () ->(List Int) (std::slice (std::list 1 2 3) -100 2))"),
    SLValue::List(vec![SLValue::Int(1), SLValue::Int(2)])
  );
}

#[test]
fn slice_list_empty_result() {
  assert_eq!(
    eval_main("(fn main () ->(List Int) (std::slice (std::list 1 2 3) 2 2))"),
    SLValue::List(vec![])
  );
  // start > stop yields empty.
  assert_eq!(
    eval_main("(fn main () ->(List Int) (std::slice (std::list 1 2 3) 3 0))"),
    SLValue::List(vec![])
  );
}

#[test]
fn slice_string() {
  assert_eq!(
    eval_main("(fn main () ->String (std::slice \"hello world\" 0 5))"),
    SLValue::String("hello".to_string())
  );
  assert_eq!(
    eval_main("(fn main () ->String (std::slice \"hello\" -2 100))"),
    SLValue::String("lo".to_string())
  );
}

#[test]
fn recursive_sum_of_list() {
  // Recursion over a list using `len` for the base case, `idx` for the head,
  // and `slice` for the tail. `(std::slice l 1 (std::len l))` yields everything
  // from index 1 to the end.
  let source = "
      (fn sum (l:(List Int)) ->Int
        (if (std::== (std::len l) 0)
          0
          (std::+ (std::idx l 0) (sum (std::slice l 1 (std::len l))))))
      (fn main () ->Int (sum (std::list 1 2 3 4 5)))
    ";
  assert_eq!(eval_main(source), SLValue::Int(15));
}

/// Helper: evaluate `main` and return the error string (panics if no error).
fn eval_main_err(source: &str) -> String {
  let pkg = match compile_executable_from_source(
    source,
    ("main", "main"),
    &default_builtins().specs(),
    &[],
  ) {
    Ok(pkg) => pkg,
    Err(error) => return error,
  };
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();
  exec.run_until_done().unwrap_err()
}

#[test]
fn block_returns_value_of_last_expression() {
  assert_eq!(
    eval_main("(fn main () ->Int (block 1 2 3))"),
    SLValue::Int(3)
  );
}

#[test]
fn block_discards_non_final_values() {
  // The `let` inside the block is for a side effect; its value is discarded
  // and the block returns the trailing literal.
  assert_eq!(
    eval_main("(fn main () ->Int (block (let a 1) (let b 2) 3))"),
    SLValue::Int(3)
  );
}

#[test]
fn dynamic_calls_evaluate_arguments_before_the_callable() {
  let source = "
    (fn id (x:Int) ->Int x)
    (fn main () ->Int
      (let cell (std::cell 0))
      ((block (std::set! cell 1) id)
        (block (std::set! cell 2) 0))
      (std::get cell))
  ";
  assert_eq!(eval_main(source), SLValue::Int(1));
}

#[test]
fn block_in_if_else_branch() {
  // The else branch uses `block` to sequence two expressions; only the last
  // is returned as the if's (and main's) value.
  assert_eq!(
    eval_main("(fn main () ->Int (if false 0 (block (let a 1) 42)))"),
    SLValue::Int(42)
  );
}

#[test]
fn block_let_visible_after_in_same_function() {
  // A `let` introduced inside a block introduces a local that remains visible
  // to later expressions in the enclosing function body (the compiler's
  // `locals` map is shared across the block and its enclosing scope).
  assert_eq!(
    eval_main("(fn main () ->Int (block (let a 7) a) (std::+ a 1))"),
    SLValue::Int(8)
  );
}

#[test]
fn block_empty_is_a_parse_error() {
  // The error surfaces at compile time, so `eval_main_err` (which unwraps
  // compilation) would panic; instead, drive the parser directly.
  let err = crate::parser::read_multiple("(fn main () ->Int (block))").unwrap_err();
  assert!(
    err.contains("`block` must have at least one expression"),
    "got: {}",
    err
  );
}

// ---- Segmented stack tests ----

/// A value-returning callee transfers exactly its result to its caller and
/// leaves no stack garbage below it.
#[test]
fn value_returning_callee_transfers_exactly_its_result() {
  let source = "
      (fn id (a:Int) ->Int a)
      (fn main () ->Int
        (let a 1) (id 10) (let b 2) (id 20) (let c 3) (id 30))
    ";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let mut exec = Interpreter::new(pkg).call_main().unwrap();
  let result = exec.run_until_done().unwrap();
  assert_eq!(result, SLValue::Int(30));
  // run_until_done pops the final value; the stack should be empty.
  assert_eq!(exec.stack_len(), 0);
}

/// A Void-returning callee discards a non-Void body result and transfers the
/// shared Void value to its caller. The body here is a non-Void `Int`; the
/// caller observes `Void`, and the Int must not leak onto the caller's
/// stack.
#[test]
fn void_returning_callee_discards_body_value() {
  let source = "
      (fn side (x:Int) (std::+ x 1))
      (fn main () ->Void (side 5) (side 10))
    ";
  assert_eq!(eval_main(source), SLValue::Void);
}

/// Nested and recursive calls restore each caller's stack segment correctly:
/// after a deep recursion unwinds, only the final result remains.
#[test]
fn nested_recursive_calls_restore_stack_segments() {
  let source = "
      (fn sum (n:Int) ->Int
        (if (std::== n 0) 0 (std::+ n (sum (std::- n 1)))))
      (fn main () ->Int (sum 500))
    ";
  assert_eq!(eval_main(source), SLValue::Int(125_250));
}

/// Dynamic calls and partial applications obey the same frame boundary: a
/// partial application's bound args do not leak into the caller's segment.
/// Partial applications arise from the closure transform (a closure
/// captures its environment via `PartialApply`), so we exercise the boundary
/// through a capturing closure.
#[test]
fn dynamic_call_and_partial_obey_frame_boundary() {
  let source = "
      (fn add (a:Int b:Int) ->Int (std::+ a b))
      (fn make-adder (a:Int) ->(Fn (Int) -> Int)
        (fn adder (b:Int) ->Int (add a b)))
      (fn main () ->Int
        (let add5 (make-adder 5))
        (add5 10))
    ";
  assert_eq!(eval_main(source), SLValue::Int(15));
  // And a fully dynamic call via a local.
  let source2 = "
      (fn double (x:Int) ->Int (std::+ x x))
      (fn main () ->Int (let f double) (f 21))
    ";
  assert_eq!(eval_main(source2), SLValue::Int(42));
}

/// Malformed inline bytecode returning zero values reports a clear
/// stack-imbalance error rather than consuming caller values.
#[test]
fn return_with_empty_frame_segment_errors() {
  let code = compiler::Function {
    num_locals: 0,
    num_params: 0,
    instructions: vec![
      Instruction::PushInt(1),
      Instruction::Pop,
      Instruction::Return,
    ],
  };
  let mut exec = exec_with_function(code, vec![]);
  let err = exec.run_until_done().unwrap_err();
  assert!(
    err.contains("Return expects exactly one value"),
    "unexpected error: {}",
    err
  );
}

/// Malformed inline bytecode returning multiple values reports a clear
/// stack-imbalance error rather than leaking extras into the caller.
#[test]
fn return_with_multiple_frame_values_errors() {
  let code = compiler::Function {
    num_locals: 0,
    num_params: 0,
    instructions: vec![
      Instruction::PushInt(1),
      Instruction::PushInt(2),
      Instruction::Return,
    ],
  };
  let mut exec = exec_with_function(code, vec![]);
  let err = exec.run_until_done().unwrap_err();
  assert!(
    err.contains("Return expects exactly one value"),
    "unexpected error: {}",
    err
  );
}

/// A top-level Void function completes with `SLValue::Void` and leaves no
/// stack garbage.
#[test]
fn top_level_void_function_completes_with_void() {
  let source = "(fn main () (let a 1) (let b 2) (std::+ a b))";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();
  let result = exec.run_until_done().unwrap();
  assert_eq!(result, SLValue::Void);
  assert_eq!(exec.stack_len(), 0);
}

/// Two Void functions with different hidden body values still compare equal
/// as Void (the body values are discarded by `Pop`; each returns inline
/// `Value::Void`).
#[test]
fn two_void_functions_with_different_body_values_compare_equal_as_void() {
  let source = "
      (fn one () 1)
      (fn two () 2)
      (fn main () ->Bool (std::== (one) (two)))
    ";
  assert_eq!(eval_main(source), SLValue::Bool(true));
}

/// Cover both omitted return types and explicit `->Void`.
#[test]
fn explicit_void_return_type() {
  let source = "
      (fn f (x:Int) ->Void (std::+ x 1))
      (fn main () ->Void (f 5) (f 10))
    ";
  assert_eq!(eval_main(source), SLValue::Void);
}

/// `std::set!` still returns Void.
#[test]
fn std_set_still_returns_void() {
  assert_eq!(
    eval_main("(fn main () ->Void (let x (std::cell 1)) (std::set! x 11))"),
    SLValue::Void
  );
}

/// A resumable host builtin can schedule a value-returning callback and
/// consume its result when the host frame resumes.
#[test]
fn resumable_host_call_with_value_returning_callback() {
  let builtins = default_builtins().with_builtin(Builtin::resumable(
    "main",
    "applydouble",
    Some(1),
    sig(
      &[],
      vec![TypeConst::function(vec![TypeConst::Int], TypeConst::Int)],
      None,
      TypeConst::Int,
    ),
    |ctx, args| {
      ctx.push(args[0]);
      Ok(())
    },
    |ctx, pending_result| {
      if let Some(result) = pending_result {
        return match result {
          Value::Int(n) => Ok(HostPoll::Ready(Value::Int(n * 2))),
          other => Err(format!(
            "expected Int from callback, got {}",
            other.type_name()
          )),
        };
      }
      let f = ctx.pop()?;
      ctx.call(f, &[Value::Int(7)])?;
      Ok(HostPoll::Pending)
    },
  ));
  let source = "
      (fn dbl (x:Int) ->Int (std::+ x x))
      (fn main () ->Int (applydouble dbl))
    ";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &builtins.specs(), &[]).unwrap();
  let interp = Interpreter::with_builtins(pkg, builtins);
  let mut exec = interp.call_main().unwrap();
  assert_eq!(exec.run_until_done().unwrap(), SLValue::Int(28));
}

/// A Void-returning scheduled callback leaves Void on the stack for the host
/// continuation to consume.
#[test]
fn resumable_host_call_with_void_returning_callback() {
  let builtins = default_builtins().with_builtin(Builtin::resumable(
    "main",
    "callvoid",
    Some(1),
    sig(
      &[],
      vec![TypeConst::function(vec![TypeConst::Int], TypeConst::Void)],
      None,
      TypeConst::Bool,
    ),
    |ctx, args| {
      ctx.push(args[0]);
      Ok(())
    },
    |ctx, pending_result| {
      if let Some(result) = pending_result {
        return match result {
          Value::Void => Ok(HostPoll::Ready(Value::Bool(true))),
          other => Err(format!(
            "expected Void from callback, got {}",
            other.type_name()
          )),
        };
      }
      let f = ctx.pop()?;
      ctx.call(f, &[Value::Int(7)])?;
      Ok(HostPoll::Pending)
    },
  ));
  let source = "
      (fn voidy (x:Int) (std::+ x 1))
      (fn main () ->Bool (callvoid voidy))
    ";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &builtins.specs(), &[]).unwrap();
  let interp = Interpreter::with_builtins(pkg, builtins);
  let mut exec = interp.call_main().unwrap();
  assert_eq!(exec.run_until_done().unwrap(), SLValue::Bool(true));
}

#[test]
fn map_callback_bytecodes_count_toward_budget() {
  let source = "
      (fn spin (x:Int) ->Int
        (let a (std::+ x 1))
        (let b (std::+ a 1))
        (let c (std::+ b 1))
        c)
      (fn main () ->(List Int) (std::map (std::list 1) spin))
    ";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();

  assert_eq!(exec.run(10).unwrap(), Status::Paused);
  assert_eq!(exec.executed(), 10);
  assert_eq!(
    exec.run(1_000).unwrap(),
    Status::Done(SLValue::List(vec![SLValue::Int(4)]))
  );
}

#[test]
fn map_can_pause_inside_callback_and_resume() {
  let source = "
      (fn bump (x:Int) ->Int
        (let a (std::+ x 1))
        (let b (std::+ a 1))
        b)
      (fn main () ->(List Int) (std::map (std::list 1 2) bump))
    ";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();

  assert_eq!(exec.run(12).unwrap(), Status::Paused);
  assert_eq!(
    exec.run(1_000).unwrap(),
    Status::Done(SLValue::List(vec![SLValue::Int(3), SLValue::Int(4)]))
  );
}

#[test]
fn non_terminating_map_callback_pauses_under_instruction_limit() {
  let source = "
      (fn loop (x:Int) ->Int (loop x))
      (fn main () ->(List Int) (std::map (std::list 1) loop))
    ";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();

  assert_eq!(exec.run(100).unwrap(), Status::Paused);
  assert_eq!(exec.executed(), 100);
  assert!(!exec.is_done());
}

#[test]
fn memory_limit_catches_map_result_allocation_after_pause() {
  let source = "
      (fn id (x:String) ->String x)
      (fn main () ->(List String) (std::map (std::list \"a\" \"b\" \"c\" \"d\") id))
    ";
  let pkg =
    compile_executable_from_source(source, ("main", "main"), &default_builtins().specs(), &[])
      .unwrap();
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();

  assert_eq!(exec.run(10).unwrap(), Status::Paused);
  let limit = exec.memory_usage() + (3 * std::mem::size_of::<Value<'static>>());
  exec.set_memory_limit(Some(limit));
  let err = exec.run_until_done().unwrap_err();
  assert!(
    err.contains("memory limit exceeded"),
    "unexpected error: {err}"
  );
}
