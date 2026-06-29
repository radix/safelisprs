//! Shared end-to-end tests that run the same SafeLisp source through both
//! the SLC bytecode compiler + interpreter and the WASM backend + wasmtime.
//!
//! Only features supported by *both* backends are tested here (Int, Bool,
//! `let`, `if`, same-module function calls, and the `std.+`/`std.-`/`std.==`
//! builtins). Backend-specific tests (closures, cells, GC, execution
//! control, unsupported-form errors, etc.) remain in their respective
//! modules under `src/`.
//!
//! Both backends normalize to `i64`: `Int` is the obvious mapping, and `Bool`
//! is `0` or `1`. The interpreter's `SLValue::Bool` is converted to `i64`
//! here so the same `expected: i64` works for both.

use rstest::rstest;
use safelisp::compiler::compile_executable_from_source;
use safelisp::interpreter::{Interpreter, SLValue};
use safelisp::wasm::{self};
use wasmtime::{Engine, Linker, Module, Store};

/// Run `source` through the SLC compiler + interpreter and return the result
/// as an `i64`. `Int(n)` → `n`; `Bool(b)` → `0`/`1`. Panics on compile or
/// runtime errors.
fn eval_interpreter(source: &str) -> i64 {
  let pkg = compile_executable_from_source(source, ("main", "main"))
    .unwrap_or_else(|e| panic!("interpreter compile failed: {e}"));
  let interp = Interpreter::new(pkg);
  let mut exec = interp
    .call_main()
    .unwrap_or_else(|e| panic!("interpreter call_main failed: {e}"));
  let result = exec
    .run_until_done()
    .unwrap_or_else(|e| panic!("interpreter run failed: {e}"));
  match result {
    SLValue::Int(n) => n,
    SLValue::Bool(b) => i64::from(b),
    other => panic!("expected Int or Bool from interpreter, got {:?}", other),
  }
}

/// Run `source` through the WASM backend + wasmtime and return the result as
/// an `i64`. Builtins are auto-registered from [`wasm::std_builtins`]. Panics
/// on compile or runtime errors.
fn eval_wasm(source: &str) -> i64 {
  let builtins = wasm::std_builtins();
  let wasm =
    wasm::compile(source, &builtins).unwrap_or_else(|e| panic!("wasm compile failed: {e}"));
  let engine = Engine::default();
  let module = Module::from_binary(&engine, &wasm).unwrap_or_else(|e| panic!("wasm validate: {e}"));
  let mut linker: Linker<()> = Linker::new(&engine);
  for b in builtins.iter() {
    let f = b.func.clone();
    linker
      .func_wrap(&b.module, &b.name, move |a: i64, b: i64| f(&[a, b]))
      .unwrap();
  }
  let mut store: Store<()> = Store::new(&engine, ());
  let instance = linker
    .instantiate(&mut store, &module)
    .unwrap_or_else(|e| panic!("wasm instantiate failed: {e}"));
  let main = instance
    .get_typed_func::<(), i64>(&mut store, "main")
    .unwrap_or_else(|e| panic!("wasm get main failed: {e}"));
  main
    .call(&mut store, ())
    .unwrap_or_else(|e| panic!("wasm call main failed: {e}"))
}

/// The set of shared test cases. Each case is `(source, expected_i64)`.
/// Both backends must produce `expected` for the source.
///
/// Sources that use `(use "src/std")` are fine: the interpreter resolves it
/// via file IO (the `src/std.sl` file), and the WASM backend ignores `use`
/// (it treats `std.*` calls as imports regardless).
#[rstest]
#[case::int_literal("(fn main () 42)", 42)]
#[case::bool_true("(fn main () true)", 1)]
#[case::bool_false("(fn main () false)", 0)]
#[case::let_returns_bound_value("(fn main () (let a 1))", 1)]
#[case::later_let_is_returned("(fn main () (let a 1) (let b 2))", 2)]
#[case::let_does_not_shadow_later_result("(fn main () (let a 1) a)", 1)]
#[case::let_then_use_variable("(use \"src/std\") (fn main () (let a 1) (let b 2) (std.+ a b))", 3)]
#[case::let_shadows_earlier_binding("(fn main () (let a 1) (let a 2) a)", 2)]
#[case::if_selects_then_branch("(fn main () (if true 42 0))", 42)]
#[case::if_selects_else_branch("(fn main () (if false 42 0))", 0)]
#[case::if_with_condition_from_call("(use \"src/std\") (fn main () (if (std.== 1 1) 7 8))", 7)]
#[case::calls_same_module_function("(fn id (a) a) (fn main () (id 99))", 99)]
#[case::calls_function_with_multiple_args("(fn first (a b) a) (fn main () (first 5 6))", 5)]
#[case::calls_function_defined_later("(fn main () (later 7)) (fn later (x) x)", 7)]
#[case::std_add("(use \"src/std\") (fn main () (std.+ 1 2))", 3)]
#[case::std_sub("(use \"src/std\") (fn main () (std.- 1 2))", -1)]
#[case::std_eq_true("(use \"src/std\") (fn main () (std.== 3 3))", 1)]
#[case::std_eq_false("(use \"src/std\") (fn main () (std.== 3 4))", 0)]
#[case::arithmetic_in_if(
  "(use \"src/std\") (fn main () (if (std.== (std.+ 1 1) 2) 100 200))",
  100
)]
#[case::multiple_lets_and_calls(
  "(use \"src/std\") (fn main () (let a 1) (let b 2) (let c 3) (std.+ a (std.+ b c)))",
  6
)]
#[case::calls_function_that_calls_another(
  "(use \"src/std\") (fn inc (n) (std.+ n 1)) (fn twice (n) (std.+ (inc n) (inc n))) (fn main () (twice 10))",
  22,
)]
#[case::recursion_with_base_case(
  "(use \"src/std\") (fn triangle (n) (if (std.== n 0) 0 (std.+ n (triangle (std.- n 1))))) (fn main () (triangle 10))",
  55,
)]
#[case::deep_recursion(
  "(use \"src/std\") (fn triangle (n) (if (std.== n 0) 0 (std.+ n (triangle (std.- n 1))))) (fn main () (triangle 10000))",
  50_005_000,
)]
fn both_backends_match_expected(#[case] source: &str, #[case(0)] expected: i64) {
  assert_eq!(eval_interpreter(source), expected, "interpreter: {source}");
  assert_eq!(eval_wasm(source), expected, "wasm: {source}");
}
