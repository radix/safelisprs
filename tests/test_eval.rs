//! Shared end-to-end tests that run the same SafeLisp source through both
//! the SLC bytecode compiler + interpreter and the WASM backend + wasmtime.
//!
//! Only features supported by *both* backends are tested here (Int, Float,
//! Bool, `let`, `if`, same-module function calls, and the `std.+`/`std.-`/
//! `std.==` builtins). Backend-specific tests remain in their respective
//! modules under `src/`.

use rstest::rstest;
use safelisp::builtins::default_builtins;
use safelisp::compiler::compile_executable_from_source;
use safelisp::interpreter::{Interpreter, SLValue};
use safelisp::wasm::{self, SLValue as WasmVal};
use wasmtime::{Engine, Linker, Module, Store};

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
  let pkg = compile_executable_from_source(source, ("main", "main"), &default_builtins().specs())
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

/// Run `source` through the WASM backend + wasmtime and return the result as
/// a `Val`. Builtins are auto-registered from [`wasm::std_builtins`]. Panics
/// on compile or runtime errors.
fn eval_wasm(source: &str) -> Val {
  let builtins = wasm::std_builtins();
  let wasm =
    wasm::compile(source, &builtins).unwrap_or_else(|e| panic!("wasm compile failed: {e}"));
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
#[case::int_literal("(fn main () 42)", Val::Int(42))]
#[case::float_literal("(fn main () 1.5)", Val::Float(1.5))]
#[case::bool_true("(fn main () true)", Val::Bool(true))]
#[case::bool_false("(fn main () false)", Val::Bool(false))]
#[case::let_returns_bound_value("(fn main () (let a 1))", Val::Int(1))]
#[case::let_binds_float("(fn main () (let a 2.5))", Val::Float(2.5))]
#[case::later_let_is_returned("(fn main () (let a 1) (let b 2))", Val::Int(2))]
#[case::let_does_not_shadow_later_result("(fn main () (let a 1) a)", Val::Int(1))]
#[case::let_then_use_variable("(fn main () (let a 1) (let b 2) (std.+ a b))", Val::Int(3))]
#[case::let_shadows_earlier_binding("(fn main () (let a 1) (let a 2) a)", Val::Int(2))]
#[case::if_selects_then_branch("(fn main () (if true 42 0))", Val::Int(42))]
#[case::if_selects_else_branch("(fn main () (if false 42 0))", Val::Int(0))]
#[case::if_with_condition_from_call("(fn main () (if (std.== 1 1) 7 8))", Val::Int(7))]
#[case::if_branches_can_use_let_variables("(fn main () (let a 10) (if true a 0))", Val::Int(10))]
#[case::calls_same_module_function("(fn id (a:Int) ->Int a) (fn main () (id 99))", Val::Int(99))]
#[case::calls_function_with_multiple_args(
  "(fn first (a:Int b:Int) ->Int a) (fn main () (first 5 6))",
  Val::Int(5)
)]
#[case::calls_function_defined_later(
  "(fn main () (later 7)) (fn later (x:Int) ->Int x)",
  Val::Int(7)
)]
#[case::std_add("(fn main () (std.+ 1 2))", Val::Int(3))]
#[case::std_sub("(fn main () (std.- 1 2))", Val::Int(-1))]
#[case::std_add_floats("(fn main () (std.+ 1.5 2.5))", Val::Float(4.0))]
#[case::std_eq_int_true("(fn main () (std.== 3 3))", Val::Bool(true))]
#[case::std_eq_int_false("(fn main () (std.== 3 4))", Val::Bool(false))]
#[case::std_eq_float_true("(fn main () (std.== 1.5 1.5))", Val::Bool(true))]
#[case::std_eq_float_false("(fn main () (std.== 1.5 2.5))", Val::Bool(false))]
#[case::std_eq_bool_true("(fn main () (std.== true true))", Val::Bool(true))]
#[case::std_eq_bool_false("(fn main () (std.== true false))", Val::Bool(false))]
#[case::arithmetic_in_if("(fn main () (if (std.== (std.+ 1 1) 2) 100 200))", Val::Int(100))]
#[case::multiple_lets_and_calls(
  "(fn main () (let a 1) (let b 2) (let c 3) (std.+ a (std.+ b c)))",
  Val::Int(6)
)]
#[case::calls_function_that_calls_another(
  "(fn inc (n:Int) ->Int (std.+ n 1)) (fn twice (n:Int) ->Int (std.+ (inc n) (inc n))) (fn main () (twice 10))",
  Val::Int(22)
)]
#[case::recursion_with_base_case(
  "(fn triangle (n:Int) ->Int (if (std.== n 0) 0 (std.+ n (triangle (std.- n 1))))) (fn main () (triangle 10))",
  Val::Int(55),
)]
#[case::deep_recursion(
  "(fn triangle (n:Int) ->Int (if (std.== n 0) 0 (std.+ n (triangle (std.- n 1))))) (fn main () (triangle 10000))",
  Val::Int(50_005_000),
)]
#[case::block_returns_last("(fn main () (block 1 2 3))", Val::Int(3))]
#[case::block_in_if_else("(fn main () (if false 0 (block (let a 1) 42)))", Val::Int(42))]
fn both_backends_match_expected(#[case] source: &str, #[case] expected: Val) {
  assert_eq!(eval_interpreter(source), expected, "interpreter: {source}");
  assert_eq!(eval_wasm(source), expected, "wasm: {source}");
}
