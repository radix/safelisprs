//! General evaluation tests.
//!

use rstest::rstest;
use safelisp::{
  compile_executable_from_source, compile_executable_from_source_with_options, CompileOptions,
  Interpreter, Library, SLValue,
};

/// Run `source` through the SLC compiler + interpreter and return the result
/// as an [`SLValue`]. Panics on compile or runtime errors.
fn eval_interpreter(source: &str) -> SLValue {
  let pkg = compile_executable_from_source(source, ("main", "main"), &Library::default())
    .unwrap_or_else(|e| panic!("interpreter compile failed: {e}"));
  let interp = Interpreter::new(pkg);
  let mut exec = interp
    .call_main()
    .unwrap_or_else(|e| panic!("interpreter call_main failed: {e}"));
  let result = exec
    .run_until_done()
    .unwrap_or_else(|e| panic!("interpreter run failed: {e}"));
  result
}

#[rstest]
#[case::int_literal("(fn main () ->Int 42)", SLValue::Int(42))]
#[case::float_literal("(fn main () ->Float 1.5)", SLValue::Float(1.5))]
#[case::bool_true("(fn main () ->Bool true)", SLValue::Bool(true))]
#[case::bool_false("(fn main () ->Bool false)", SLValue::Bool(false))]
#[case::let_returns_bound_value("(fn main () ->Int (let a 1))", SLValue::Int(1))]
#[case::let_binds_float("(fn main () ->Float (let a 2.5))", SLValue::Float(2.5))]
#[case::later_let_is_returned("(fn main () ->Int (let a 1) (let b 2))", SLValue::Int(2))]
#[case::let_does_not_shadow_later_result("(fn main () ->Int (let a 1) a)", SLValue::Int(1))]
#[case::let_then_use_variable(
  "(fn main () ->Int (let a 1) (let b 2) (std::+ a b))",
  SLValue::Int(3)
)]
#[case::shd_shadows_earlier_binding("(fn main () ->Int (let a 1) (shd a 2) a)", SLValue::Int(2))]
#[case::assign_reassigns_earlier_binding("(fn main () ->Int (let a 1) (= a 2) a)", SLValue::Int(2))]
#[case::if_selects_then_branch("(fn main () ->Int (if true 42 0))", SLValue::Int(42))]
#[case::if_selects_else_branch("(fn main () ->Int (if false 42 0))", SLValue::Int(0))]
#[case::if_with_condition_from_call("(fn main () ->Int (if (std::== 1 1) 7 8))", SLValue::Int(7))]
#[case::layout_fn_and_if(
  "fn main () -> Int
     if (std::== 1 1)
       7
     else
       8",
  SLValue::Int(7)
)]
#[case::layout_match_arm_body(
  "enum MaybeInt
  (Some value:Int)
  (None)
fn main () -> Int
  match (new MaybeInt::Some value:4)
    (Some value) =>
      let next (+ value 1)
      next
    (None) => 0",
  SLValue::Int(5)
)]
#[case::if_branches_can_use_let_variables(
  "(fn main () ->Int (let a 10) (if true a 0))",
  SLValue::Int(10)
)]
#[case::assign_in_both_if_branches_then_path(
  "(fn main () ->Int (let a 0) (if true (= a 10) (= a 20)) a)",
  SLValue::Int(10)
)]
#[case::assign_in_both_if_branches_else_path(
  "(fn main () ->Int (let a 0) (if false (= a 10) (= a 20)) a)",
  SLValue::Int(20)
)]
#[case::assign_in_if_then_branch_propagates(
  "(fn main () ->Int (let a 5) (if true (= a 10) a) a)",
  SLValue::Int(10)
)]
#[case::assign_in_if_else_branch_keeps_value(
  "(fn main () ->Int (let a 5) (if false (= a 10) a) a)",
  SLValue::Int(5)
)]
#[case::if_without_else_runs_then_side_effect(
  "(fn main () ->Int (let a 0) (if true (= a 7)) a)",
  SLValue::Int(7)
)]
#[case::if_without_else_skips_side_effect_when_false(
  "(fn main () ->Int (let a 0) (if false (shd a 7)) a)",
  SLValue::Int(0)
)]
#[case::layout_if_without_else_runs_then_side_effect(
  "fn main () -> Int
     let a 0
     if true
       = a 7
     a",
  SLValue::Int(7)
)]
#[case::calls_same_module_function(
  "(fn id (a:Int) ->Int a) (fn main () ->Int (id 99))",
  SLValue::Int(99)
)]
#[case::calls_function_with_multiple_args(
  "(fn first (a:Int b:Int) ->Int a) (fn main () ->Int (first 5 6))",
  SLValue::Int(5)
)]
#[case::calls_function_defined_later(
  "(fn main () ->Int (later 7)) (fn later (x:Int) ->Int x)",
  SLValue::Int(7)
)]
#[case::std_add("(fn main () ->Int (std::+ 1 2))", SLValue::Int(3))]
#[case::prelude_std_add("(fn main () ->Int (+ 1 2))", SLValue::Int(3))]
#[case::prelude_function_ref_can_be_bound(
  "(fn main () ->Int (let add +) (add 2 3))",
  SLValue::Int(5)
)]
#[case::same_module_function_shadows_prelude(
  "(fn + (a:Int b:Int) ->Int a) (fn main () ->Int (+ 5 6))",
  SLValue::Int(5)
)]
#[case::std_sub("(fn main () ->Int (std::- 1 2))", SLValue::Int(-1))]
#[case::std_add_floats("(fn main () ->Float (std::+ 1.5 2.5))", SLValue::Float(4.0))]
#[case::std_eq_int_true("(fn main () ->Bool (std::== 3 3))", SLValue::Bool(true))]
#[case::std_eq_int_false("(fn main () ->Bool (std::== 3 4))", SLValue::Bool(false))]
#[case::std_eq_float_true("(fn main () ->Bool (std::== 1.5 1.5))", SLValue::Bool(true))]
#[case::std_eq_float_false("(fn main () ->Bool (std::== 1.5 2.5))", SLValue::Bool(false))]
#[case::std_eq_bool_true("(fn main () ->Bool (std::== true true))", SLValue::Bool(true))]
#[case::std_eq_bool_false("(fn main () ->Bool (std::== true false))", SLValue::Bool(false))]
#[case::void_functions_discard_body_values(
  "(fn one () 1) (fn two () 2) (fn main () ->Bool (std::== (one) (two)))",
  SLValue::Bool(true)
)]
#[case::arithmetic_in_if(
  "(fn main () ->Int (if (std::== (std::+ 1 1) 2) 100 200))",
  SLValue::Int(100)
)]
#[case::multiple_lets_and_calls(
  "(fn main () ->Int (let a 1) (let b 2) (let c 3) (std::+ a (std::+ b c)))",
  SLValue::Int(6)
)]
#[case::calls_function_that_calls_another(
  "(fn inc (n:Int) ->Int (std::+ n 1)) (fn twice (n:Int) ->Int (std::+ (inc n) (inc n))) (fn main () ->Int (twice 10))",
  SLValue::Int(22)
)]
#[case::recursion_with_base_case(
  "(fn triangle (n:Int) ->Int (if (std::== n 0) 0 (std::+ n (triangle (std::- n 1))))) (fn main () ->Int (triangle 10))",
  SLValue::Int(55),
)]
#[case::deep_recursion(
  "(fn triangle (n:Int) ->Int (if (std::== n 0) 0 (std::+ n (triangle (std::- n 1))))) (fn main () ->Int (triangle 10000))",
  SLValue::Int(50_005_000),
)]
#[case::block_returns_last("(fn main () ->Int (block 1 2 3))", SLValue::Int(3))]
#[case::block_in_if_else(
  "(fn main () ->Int (if false 0 (block (let a 1) 42)))",
  SLValue::Int(42)
)]
#[case::return_unwinds_expression("(fn main () ->Int (std::+ 1 (return 42)))", SLValue::Int(42))]
#[case::and_short_circuits_return(
  "(fn main () ->Bool (and false (return true)))",
  SLValue::Bool(false)
)]
#[case::or_short_circuits_return(
  "(fn main () ->Bool (or true (return false)))",
  SLValue::Bool(true)
)]
#[case::shd_shadows_value("(fn main () ->Int (let a 1) (shd a 2) a)", SLValue::Int(2))]
#[case::shd_can_change_type(
  "(fn main () ->Int (let a 1) (shd a true) (if a 99 0))",
  SLValue::Int(99)
)]
#[case::assign_in_if_both_branches_same_type(
  "(fn main () ->Int (let x 1) (if (std::== x 1) (= x 99) (= x 0)) x)",
  SLValue::Int(99)
)]
#[case::assign_in_if_one_branch_same_type_propagates(
  "(fn main () ->Int (let x 1) (if true (= x 5) 0) x)",
  SLValue::Int(5)
)]
#[case::assign_in_for_accumulates_outer_binding(
  "(fn main () ->Int (let s 0) (for n in (std::list 1 2 3) (= s (std::+ s n))) s)",
  SLValue::Int(6)
)]
#[case::let_in_if_branch_does_not_escape(
  "(fn main () ->Int (let a 0) (if true (block (let b 10) (= a b)) (= a 20)) a)",
  SLValue::Int(10)
)]
fn interpreter_matches_expected(#[case] source: &str, #[case] expected: SLValue) {
  assert_eq!(eval_interpreter(source), expected, "interpreter: {source}");
}

/// Regression reproducer: compiling deeply nested expressions must not abort
/// the process with a stack overflow. The parser caps nesting at
/// [`CompileOptions::DEFAULT_MAX_PARSE_DEPTH`] by default and returns a clean
/// error past that. Raising the budget (on a thread with a large stack) lets
/// the same deeply nested program compile, proving the default rejection is a
/// depth-budget guard rather than a logic bug.
#[test]
fn compile_deeply_nested_addition_does_not_overflow_on_large_stack() {
  // A depth far above the default budget, but well within reach of a large
  // stack once the budget is raised.
  let depth = 2_000;
  let mut inner = "0".to_string();
  for _ in 0..depth {
    inner = format!("(+ 1 {inner})");
  }
  let source = format!("fn main () -> Int\n  {inner}\n");

  // Default budget rejects depth 2,000 with a clean error (no abort).
  let default_err =
    compile_executable_from_source(&source, ("main", "main"), &Library::default()).unwrap_err();
  assert!(
    default_err.contains("nesting too deep"),
    "expected a depth error, got: {default_err}"
  );

  // Raising the budget and giving the compile a large stack lets it succeed.
  let options = CompileOptions::default().max_parse_depth(depth + 32);
  let handle = std::thread::Builder::new()
    .stack_size(512 * 1024 * 1024)
    .spawn(move || {
      compile_executable_from_source_with_options(
        &source,
        ("main", "main"),
        &Library::default(),
        &options,
      )
    })
    .expect("spawn");
  let result = handle.join().expect("compile thread panicked");
  assert!(
    result.is_ok(),
    "deeply nested program should compile: {result:?}"
  );
}

/// The default budget must reject deeply nested source with a clean error
/// instead of aborting the process with a stack overflow. This runs on the
/// default (2 MiB) test-thread stack in both debug and release.
#[test]
fn default_budget_rejects_deeply_nested_source_with_a_clean_error() {
  let depth = 5_000;
  let mut inner = "0".to_string();
  for _ in 0..depth {
    inner = format!("(+ 1 {inner})");
  }
  let source = format!("fn main () -> Int\n  {inner}\n");
  let error =
    compile_executable_from_source(&source, ("main", "main"), &Library::default()).unwrap_err();
  assert!(
    error.contains("nesting too deep"),
    "expected a depth error, got: {error}"
  );
  assert!(
    error.contains("maximum parse depth of"),
    "expected the limit in the message, got: {error}"
  );
}

/// Programs with ordinary nesting still compile under the default budget.
#[test]
fn default_budget_compiles_moderately_nested_programs() {
  let mut inner = "0".to_string();
  for _ in 0..16 {
    inner = format!("(+ 1 {inner})");
  }
  let source = format!("fn main () -> Int\n  {inner}\n");
  let package = compile_executable_from_source(&source, ("main", "main"), &Library::default())
    .expect("moderately nested program should compile under the default budget");
  let result = Interpreter::new(package)
    .call_main()
    .unwrap()
    .run_until_done()
    .unwrap();
  assert_eq!(result, SLValue::Int(16));
}
