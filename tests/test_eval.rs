//! Shared end-to-end tests for the SLC bytecode compiler + interpreter and,
//! when `wasm-tests` is enabled, the WASM backend + wasmtime.
//!
//! Only features supported by *both* backends are tested here (Int, Float,
//! Bool, `let`, `if`, same-module function calls, and the `std::+`/`std::-`/
//! `std::==` builtins). Backend-specific tests remain in their respective
//! modules under `src/`.

use rstest::rstest;
#[cfg(feature = "wasm-tests")]
use safelisp::wasm::{self, SLValue as WasmVal};
use safelisp::{
  compile_executable_from_source, sig, Builtin, CustomTypeSpec, Interpreter, Library, SLVal,
  SLValue, Signature, Value,
};
#[cfg(feature = "wasm-tests")]
use wasmtime::{Engine, Linker, Module, Store};

#[cfg(feature = "wasm-tests")]
const PRELUDE: &[(&str, &str)] = &[("std", "+"), ("std", "-"), ("std", "==")];

/// A value that both backends can produce, for cross-backend comparison.
#[derive(Debug, Clone, PartialEq)]
enum Val {
  Int(i64),
  Float(f64),
  Bool(bool),
  Void,
}

/// Run `source` through the SLC compiler + interpreter and return the result
/// as a `Val`. Panics on compile or runtime errors.
fn eval_interpreter(source: &str) -> Val {
  let pkg = compile_executable_from_source(source, ("main", "main"), &Library::default())
    .unwrap_or_else(|e| panic!("interpreter compile failed: {e}"));
  let interp = Interpreter::new(pkg);
  let mut exec = interp
    .call_main()
    .unwrap_or_else(|e| panic!("interpreter call_main failed: {e}"));
  let result = exec
    .run_until_done()
    .unwrap_or_else(|e| panic!("interpreter run failed: {e}"));
  match result {
    SLValue::Int(n) => Val::Int(n),
    SLValue::Float(f) => Val::Float(f),
    SLValue::Bool(b) => Val::Bool(b),
    SLValue::Void => Val::Void,
    other => panic!("expected scalar from interpreter, got {:?}", other),
  }
}

#[test]
fn custom_interpreter_builtins_are_public_api() {
  let builtins = Library::new().with_builtin(Builtin::unary(
    "main",
    "add2",
    sig(&[], vec![Signature::Int], None, Signature::Int),
    |value| match value {
      Value::Int(n) => Ok(Value::Int(n + 2)),
      other => Err(format!("expected Int, got {}", other.type_name())),
    },
  ));
  let package =
    compile_executable_from_source("(fn main () ->Int (add2 3))", ("main", "main"), &builtins)
      .unwrap_or_else(|e| panic!("compile failed: {e}"));
  let mut exec = Interpreter::with_library(package, builtins)
    .call_main()
    .unwrap_or_else(|e| panic!("call_main failed: {e}"));

  assert_eq!(exec.run_until_done().unwrap(), SLValue::Int(5));
}

#[test]
fn libraries_can_be_composed_with_custom_types() {
  let types = Library::new().with_type(CustomTypeSpec::struct_(
    "box",
    "Box",
    vec![("value", Signature::Int)],
  ));
  let funcs = Library::new()
    .with_builtin(Builtin::contextual_value(
      "box",
      "box",
      Some(1),
      sig(
        &[],
        vec![Signature::Int],
        None,
        Signature::named("box", "Box"),
      ),
      |ctx, args| ctx.alloc_struct("box", "Box", vec![args[0]]),
    ))
    .with_builtin(Builtin::unary(
      "box",
      "unbox",
      sig(
        &[],
        vec![Signature::named("box", "Box")],
        None,
        Signature::Int,
      ),
      |value| match value {
        Value::Heap(heap) => match &heap.value {
          SLVal::Struct(instance) => Ok(instance.fields[0]),
          other => Err(format!("expected Box, got {}", other.type_name())),
        },
        other => Err(format!("expected Box, got {}", other.type_name())),
      },
    ))
    .with_prelude("box", "box")
    .with_prelude("box", "unbox");
  let library = types.merge(funcs);
  let package = compile_executable_from_source(
    "(fn main () ->Int (unbox (box 40)))",
    ("main", "main"),
    &library,
  )
  .unwrap_or_else(|e| panic!("compile failed: {e}"));
  let mut exec = Interpreter::with_library(package, library)
    .call_main()
    .unwrap_or_else(|e| panic!("call_main failed: {e}"));

  assert_eq!(exec.run_until_done().unwrap(), SLValue::Int(40));
}

#[test]
fn custom_types_are_distinct_across_modules() {
  let library = Library::new()
    .with_type(CustomTypeSpec::struct_(
      "left",
      "Box",
      vec![("value", Signature::Int)],
    ))
    .with_type(CustomTypeSpec::struct_(
      "right",
      "Box",
      vec![("value", Signature::String)],
    ))
    .with_builtin(Builtin::contextual_value(
      "right",
      "box",
      Some(1),
      sig(
        &[],
        vec![Signature::String],
        None,
        Signature::named("right", "Box"),
      ),
      |ctx, args| ctx.alloc_struct("right", "Box", vec![args[0]]),
    ))
    .with_builtin(Builtin::unary(
      "left",
      "unbox",
      sig(
        &[],
        vec![Signature::named("left", "Box")],
        None,
        Signature::Int,
      ),
      |value| match value {
        Value::Heap(heap) => match &heap.value {
          SLVal::Struct(instance) => Ok(instance.fields[0]),
          other => Err(format!("expected Box, got {}", other.type_name())),
        },
        other => Err(format!("expected Box, got {}", other.type_name())),
      },
    ));

  let err = compile_executable_from_source(
    "(fn main () ->Int (left::unbox (right::box \"nope\")))",
    ("main", "main"),
    &library,
  )
  .unwrap_err();

  assert!(
    err.contains("expected `left::Box`, got `right::Box`"),
    "{err}"
  );
}

#[test]
fn bare_custom_type_annotations_must_be_unambiguous() {
  let types = Library::new()
    .with_type(CustomTypeSpec::struct_(
      "left",
      "Box",
      vec![("value", Signature::Int)],
    ))
    .with_type(CustomTypeSpec::struct_(
      "right",
      "Box",
      vec![("value", Signature::Int)],
    ));

  let err =
    compile_executable_from_source("(fn main () ->Box 1)", ("main", "main"), &types).unwrap_err();

  assert!(err.contains("ambiguous type `Box`"), "{err}");
  assert!(err.contains("left::Box"), "{err}");
  assert!(err.contains("right::Box"), "{err}");

  let library = types.with_builtin(Builtin::contextual_value(
    "left",
    "box",
    Some(1),
    sig(
      &[],
      vec![Signature::Int],
      None,
      Signature::named("left", "Box"),
    ),
    |ctx, args| ctx.alloc_struct("left", "Box", vec![args[0]]),
  ));
  compile_executable_from_source(
    "(fn main () -> left::Box (left::box 1))",
    ("main", "main"),
    &library,
  )
  .unwrap();
}

#[test]
fn call_main_with_args_is_public_api() {
  let package = compile_executable_from_source(
    "(fn main (a:Int b:Int) ->Int (+ a b))",
    ("main", "main"),
    &Library::default(),
  )
  .unwrap_or_else(|e| panic!("compile failed: {e}"));
  let mut exec = Interpreter::new(package)
    .call_main_with(vec![SLValue::Int(2), SLValue::Int(5)])
    .unwrap_or_else(|e| panic!("call_main_with failed: {e}"));

  assert_eq!(exec.run_until_done().unwrap(), SLValue::Int(7));
}

#[test]
fn call_main_with_checks_arity() {
  let package = compile_executable_from_source(
    "(fn main (a:Int) ->Int a)",
    ("main", "main"),
    &Library::default(),
  )
  .unwrap_or_else(|e| panic!("compile failed: {e}"));
  let err = match Interpreter::new(package).call_main_with(vec![]) {
    Ok(_) => panic!("expected call_main_with to reject missing args"),
    Err(err) => err,
  };

  assert!(
    err.contains("expects 1 arg(s) but was called with 0"),
    "unexpected error: {err}"
  );
}

#[test]
fn call_value_with_args_is_public_api() {
  let package = compile_executable_from_source(
    "
      (fn main () ->(Fn (Int) -> Int)
        (let base 10)
        (fn add-base (x:Int) ->Int (+ base x))
        add-base)
    ",
    ("main", "main"),
    &Library::default(),
  )
  .unwrap_or_else(|e| panic!("compile failed: {e}"));
  let interp = Interpreter::new(package);
  let mut exec = interp
    .call_main()
    .unwrap_or_else(|e| panic!("call_main failed: {e}"));
  let callable = exec
    .run_until_done()
    .unwrap_or_else(|e| panic!("run failed: {e}"));
  let mut exec = interp
    .call_value(callable, vec![SLValue::Int(7)])
    .unwrap_or_else(|e| panic!("call_value failed: {e}"));

  assert_eq!(exec.run_until_done().unwrap(), SLValue::Int(17));
}

/// Run `source` through the WASM backend + wasmtime and return the result as
/// a `Val`. Builtins are auto-registered from [`wasm::std_builtins`]. Panics
/// on compile or runtime errors.
#[cfg(feature = "wasm-tests")]
fn eval_wasm(source: &str) -> Val {
  let builtins = wasm::std_builtins();
  let wasm = wasm::compile(source, &builtins, PRELUDE)
    .unwrap_or_else(|e| panic!("wasm compile failed: {e}"));
  let engine = Engine::default();
  let module = Module::from_binary(&engine, &wasm).unwrap_or_else(|e| panic!("wasm validate: {e}"));
  let mut linker: Linker<()> = Linker::new(&engine);
  for b in builtins.iter() {
    register_one(&mut linker, b);
  }
  let mut store: Store<()> = Store::new(&engine, ());
  let instance = linker
    .instantiate(&mut store, &module)
    .unwrap_or_else(|e| panic!("wasm instantiate failed: {e}"));
  let main = instance
    .get_typed_func::<(), (i64, i32)>(&mut store, "main")
    .unwrap_or_else(|e| panic!("wasm get main failed: {e}"));
  let (payload, tag) = main
    .call(&mut store, ())
    .unwrap_or_else(|e| panic!("wasm call main failed: {e}"));
  match WasmVal::from_parts(tag, payload) {
    WasmVal::Int(n) => Val::Int(n),
    WasmVal::Float(f) => Val::Float(f),
    WasmVal::Bool(b) => Val::Bool(b),
    WasmVal::Void => Val::Void,
    WasmVal::FunctionRef(index) => panic!("expected scalar from WASM, got function {index}"),
  }
}

/// Register a single builtin with the linker, dispatching on arity.
#[cfg(feature = "wasm-tests")]
fn register_one(linker: &mut Linker<()>, b: &safelisp::wasm::Builtin) {
  let f = b.func.clone();
  match b.num_params {
    0 => {
      linker
        .func_wrap(&b.module, &b.name, move || {
          let r = f(&[]);
          (r.payload(), r.tag())
        })
        .unwrap();
    }
    1 => {
      linker
        .func_wrap(&b.module, &b.name, move |p0: i64, t0: i32| {
          let r = f(&[WasmVal::from_parts(t0, p0)]);
          (r.payload(), r.tag())
        })
        .unwrap();
    }
    2 => {
      linker
        .func_wrap(
          &b.module,
          &b.name,
          move |p0: i64, t0: i32, p1: i64, t1: i32| {
            let r = f(&[WasmVal::from_parts(t0, p0), WasmVal::from_parts(t1, p1)]);
            (r.payload(), r.tag())
          },
        )
        .unwrap();
    }
    3 => {
      linker
        .func_wrap(
          &b.module,
          &b.name,
          move |p0: i64, t0: i32, p1: i64, t1: i32, p2: i64, t2: i32| {
            let r = f(&[
              WasmVal::from_parts(t0, p0),
              WasmVal::from_parts(t1, p1),
              WasmVal::from_parts(t2, p2),
            ]);
            (r.payload(), r.tag())
          },
        )
        .unwrap();
    }
    n => panic!(
      "unsupported arity {} for builtin {}.{}",
      n, b.module, b.name
    ),
  }
}

#[rstest]
#[case::int_literal("(fn main () ->Int 42)", Val::Int(42))]
#[case::float_literal("(fn main () ->Float 1.5)", Val::Float(1.5))]
#[case::bool_true("(fn main () ->Bool true)", Val::Bool(true))]
#[case::bool_false("(fn main () ->Bool false)", Val::Bool(false))]
#[case::let_returns_bound_value("(fn main () ->Int (let a 1))", Val::Int(1))]
#[case::let_binds_float("(fn main () ->Float (let a 2.5))", Val::Float(2.5))]
#[case::later_let_is_returned("(fn main () ->Int (let a 1) (let b 2))", Val::Int(2))]
#[case::let_does_not_shadow_later_result("(fn main () ->Int (let a 1) a)", Val::Int(1))]
#[case::let_then_use_variable("(fn main () ->Int (let a 1) (let b 2) (std::+ a b))", Val::Int(3))]
#[case::let_shadows_earlier_binding("(fn main () ->Int (let a 1) (let a 2) a)", Val::Int(2))]
#[case::if_selects_then_branch("(fn main () ->Int (if true 42 0))", Val::Int(42))]
#[case::if_selects_else_branch("(fn main () ->Int (if false 42 0))", Val::Int(0))]
#[case::if_with_condition_from_call("(fn main () ->Int (if (std::== 1 1) 7 8))", Val::Int(7))]
#[case::layout_fn_and_if(
  "fn main () -> Int
     if (std::== 1 1)
       7
     else
       8",
  Val::Int(7)
)]
#[case::layout_match_arm_body(
  "enum MaybeInt
  (Some value:Int)
  (None)
fn main () -> Int
  match (new MaybeInt::Some value:4)
    (Some value) =>
      let new (+ value 1)
      new
    (None) => 0",
  Val::Int(5)
)]
#[case::if_branches_can_use_let_variables(
  "(fn main () ->Int (let a 10) (if true a 0))",
  Val::Int(10)
)]
#[case::binding_created_in_both_if_branches(
  "(fn main () ->Int (if true (let a 10) (let a 20)) a)",
  Val::Int(10)
)]
#[case::binding_created_in_both_if_branches_else_path(
  "(fn main () ->Int (if false (let a 10) (let a 20)) a)",
  Val::Int(20)
)]
#[case::if_join_selects_shadowing_binding(
  "(fn main () ->Int (let a 5) (if true (let a 10) a) a)",
  Val::Int(10)
)]
#[case::if_join_keeps_existing_binding(
  "(fn main () ->Int (let a 5) (if false (let a 10) a) a)",
  Val::Int(5)
)]
#[case::calls_same_module_function(
  "(fn id (a:Int) ->Int a) (fn main () ->Int (id 99))",
  Val::Int(99)
)]
#[case::calls_function_with_multiple_args(
  "(fn first (a:Int b:Int) ->Int a) (fn main () ->Int (first 5 6))",
  Val::Int(5)
)]
#[case::calls_function_defined_later(
  "(fn main () ->Int (later 7)) (fn later (x:Int) ->Int x)",
  Val::Int(7)
)]
#[case::std_add("(fn main () ->Int (std::+ 1 2))", Val::Int(3))]
#[case::prelude_std_add("(fn main () ->Int (+ 1 2))", Val::Int(3))]
#[case::prelude_function_ref_can_be_bound("(fn main () ->Int (let add +) (add 2 3))", Val::Int(5))]
#[case::same_module_function_shadows_prelude(
  "(fn + (a:Int b:Int) ->Int a) (fn main () ->Int (+ 5 6))",
  Val::Int(5)
)]
#[case::std_sub("(fn main () ->Int (std::- 1 2))", Val::Int(-1))]
#[case::std_add_floats("(fn main () ->Float (std::+ 1.5 2.5))", Val::Float(4.0))]
#[case::std_eq_int_true("(fn main () ->Bool (std::== 3 3))", Val::Bool(true))]
#[case::std_eq_int_false("(fn main () ->Bool (std::== 3 4))", Val::Bool(false))]
#[case::std_eq_float_true("(fn main () ->Bool (std::== 1.5 1.5))", Val::Bool(true))]
#[case::std_eq_float_false("(fn main () ->Bool (std::== 1.5 2.5))", Val::Bool(false))]
#[case::std_eq_bool_true("(fn main () ->Bool (std::== true true))", Val::Bool(true))]
#[case::std_eq_bool_false("(fn main () ->Bool (std::== true false))", Val::Bool(false))]
#[case::void_functions_discard_body_values(
  "(fn one () 1) (fn two () 2) (fn main () ->Bool (std::== (one) (two)))",
  Val::Bool(true)
)]
#[case::arithmetic_in_if(
  "(fn main () ->Int (if (std::== (std::+ 1 1) 2) 100 200))",
  Val::Int(100)
)]
#[case::multiple_lets_and_calls(
  "(fn main () ->Int (let a 1) (let b 2) (let c 3) (std::+ a (std::+ b c)))",
  Val::Int(6)
)]
#[case::calls_function_that_calls_another(
  "(fn inc (n:Int) ->Int (std::+ n 1)) (fn twice (n:Int) ->Int (std::+ (inc n) (inc n))) (fn main () ->Int (twice 10))",
  Val::Int(22)
)]
#[case::recursion_with_base_case(
  "(fn triangle (n:Int) ->Int (if (std::== n 0) 0 (std::+ n (triangle (std::- n 1))))) (fn main () ->Int (triangle 10))",
  Val::Int(55),
)]
#[case::deep_recursion(
  "(fn triangle (n:Int) ->Int (if (std::== n 0) 0 (std::+ n (triangle (std::- n 1))))) (fn main () ->Int (triangle 10000))",
  Val::Int(50_005_000),
)]
#[case::block_returns_last("(fn main () ->Int (block 1 2 3))", Val::Int(3))]
#[case::block_in_if_else("(fn main () ->Int (if false 0 (block (let a 1) 42)))", Val::Int(42))]
fn both_backends_match_expected(#[case] source: &str, #[case] expected: Val) {
  assert_eq!(eval_interpreter(source), expected, "interpreter: {source}");
  #[cfg(feature = "wasm-tests")]
  assert_eq!(eval_wasm(source), expected, "wasm: {source}");
}
