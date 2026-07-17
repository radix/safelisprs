use super::*;
use wasmtime::{Engine, Linker, Module, Store};

/// Register every builtin with the given wasmtime `Linker`. Each builtin
/// has a signature of `n` SafeLisp values (each a tag+payload pair on the
/// WASM side). We dispatch on `num_params` to call `func_wrap` with the
/// right arity, converting between the WASM `(i64, i32)` pairs and
/// `SlValue`.
fn register_all(linker: &mut Linker<()>, builtins: &Builtins) -> Result<(), String> {
  for b in builtins.iter() {
    let f = b.func.clone();
    let result = match b.num_params {
      0 => linker.func_wrap(&b.module, &b.name, move || {
        let r = f(&[]);
        (r.payload(), r.tag())
      }),
      1 => linker.func_wrap(&b.module, &b.name, move |p0: i64, t0: i32| {
        let r = f(&[SLValue::from_parts(t0, p0)]);
        (r.payload(), r.tag())
      }),
      2 => linker.func_wrap(
        &b.module,
        &b.name,
        move |p0: i64, t0: i32, p1: i64, t1: i32| {
          let r = f(&[SLValue::from_parts(t0, p0), SLValue::from_parts(t1, p1)]);
          (r.payload(), r.tag())
        },
      ),
      3 => linker.func_wrap(
        &b.module,
        &b.name,
        move |p0: i64, t0: i32, p1: i64, t1: i32, p2: i64, t2: i32| {
          let r = f(&[
            SLValue::from_parts(t0, p0),
            SLValue::from_parts(t1, p1),
            SLValue::from_parts(t2, p2),
          ]);
          (r.payload(), r.tag())
        },
      ),
      n => {
        return Err(format!(
          "unsupported arity {} for builtin {}.{}",
          n, b.module, b.name
        ))
      }
    };
    result.map_err(|e| format!("link {}::{}: {e}", b.module, b.name))?;
  }
  Ok(())
}

/// Compile with [`std_builtins`], instantiate, run `main`, and return the
/// `SlValue` result.
fn run_main(source: &str) -> Result<SLValue, String> {
  run_main_with(source, &std_builtins())
}

/// Compile with the given `builtins` registry, auto-register every builtin
/// with the linker, then instantiate and run `main`.
fn run_main_with(source: &str, builtins: &Builtins) -> Result<SLValue, String> {
  let wasm = compile(source, builtins, &[])?;
  let engine = Engine::default();
  let module = Module::from_binary(&engine, &wasm).map_err(|e| format!("wasm validate: {e}"))?;
  let mut linker: Linker<()> = Linker::new(&engine);
  register_all(&mut linker, builtins)?;
  let mut store: Store<()> = Store::new(&engine, ());
  let instance = linker
    .instantiate(&mut store, &module)
    .map_err(|e| format!("instantiate: {e}"))?;
  let main = instance
    .get_typed_func::<(), (i64, i32)>(&mut store, "main")
    .map_err(|e| format!("get main: {e}"))?;
  let (payload, tag) = main
    .call(&mut store, ())
    .map_err(|e| format!("call main: {e}"))?;
  Ok(SLValue::from_parts(tag, payload))
}

fn assert_main_eq(source: &str, expected: SLValue) {
  let result = run_main(source).unwrap_or_else(|e| panic!("run_main failed: {e}"));
  assert_eq!(result, expected, "source: {:?}", source);
}

#[test]
fn returns_int_literal() {
  assert_main_eq("(fn main () ->Int 42)", SLValue::Int(42));
}

#[test]
fn returns_bool_literal() {
  assert_main_eq("(fn main () ->Bool true)", SLValue::Bool(true));
  assert_main_eq("(fn main () ->Bool false)", SLValue::Bool(false));
}

#[test]
fn returns_float_literal() {
  assert_main_eq("(fn main () ->Float 1.5)", SLValue::Float(1.5));
}

#[test]
fn let_returns_bound_value() {
  assert_main_eq("(fn main () ->Int (let a 1))", SLValue::Int(1));
}

#[test]
fn let_binds_float() {
  assert_main_eq("(fn main () ->Float (let a 2.5))", SLValue::Float(2.5));
}

#[test]
fn let_then_use_variable() {
  assert_main_eq(
    "(fn main () ->Int (let a 1) (let b 2) (std::+ a b))",
    SLValue::Int(3),
  );
}

#[test]
fn top_level_function_can_be_called_through_local() {
  assert_main_eq(
    "(fn double (x:Int) ->Int (std::+ x x)) (fn main () ->Int (let f double) (f 4))",
    SLValue::Int(8),
  );
}

#[test]
fn qualified_function_can_be_called_dynamically() {
  assert_main_eq(
    "(fn double (x:Int) ->Int (std::+ x x)) (fn main () ->Int (let f main::double) (f 4))",
    SLValue::Int(8),
  );
}

#[test]
fn builtin_function_can_be_called_through_local() {
  assert_main_eq(
    "(fn main () ->Int (let add std::+) (add 2 3))",
    SLValue::Int(5),
  );
}

#[test]
fn local_shadows_top_level_function_as_value() {
  assert_main_eq(
    "(fn transform (x:Int) ->Int (std::+ x x))
       (fn identity (x:Int) ->Int x)
       (fn main () ->Int (let transform identity) (transform 7))",
    SLValue::Int(7),
  );
}

#[test]
fn let_shadows_earlier_binding() {
  assert_main_eq("(fn main () ->Int (let a 1) (let a 2) a)", SLValue::Int(2));
}

#[test]
fn if_selects_then_branch() {
  assert_main_eq("(fn main () ->Int (if true 42 0))", SLValue::Int(42));
}

#[test]
fn if_selects_else_branch() {
  assert_main_eq("(fn main () ->Int (if false 42 0))", SLValue::Int(0));
}

#[test]
fn if_with_condition_from_call() {
  assert_main_eq("(fn main () ->Int (if (std::== 1 1) 7 8))", SLValue::Int(7));
}

#[test]
fn if_branches_can_use_let_variables() {
  assert_main_eq(
    "(fn main () ->Int (let a 10) (if true a 0))",
    SLValue::Int(10),
  );
}

#[test]
fn calls_same_module_function() {
  assert_main_eq(
    "(fn id (a:Int) ->Int a) (fn main () ->Int (id 99))",
    SLValue::Int(99),
  );
}

#[test]
fn calls_function_with_multiple_args() {
  assert_main_eq(
    "(fn first (a:Int b:Int) ->Int a) (fn main () ->Int (first 5 6))",
    SLValue::Int(5),
  );
}

#[test]
fn calls_function_defined_later() {
  assert_main_eq(
    "(fn main () ->Int (later 7)) (fn later (x:Int) ->Int x)",
    SLValue::Int(7),
  );
}

#[test]
fn std_add() {
  assert_main_eq("(fn main () ->Int (std::+ 1 2))", SLValue::Int(3));
}

#[test]
fn std_sub() {
  assert_main_eq("(fn main () ->Int (std::- 1 2))", SLValue::Int(-1));
}

#[test]
fn std_eq_true() {
  assert_main_eq("(fn main () ->Bool (std::== 3 3))", SLValue::Bool(true));
}

#[test]
fn std_eq_false() {
  assert_main_eq("(fn main () ->Bool (std::== 3 4))", SLValue::Bool(false));
}

#[test]
fn std_add_floats() {
  assert_main_eq("(fn main () ->Float (std::+ 1.5 2.5))", SLValue::Float(4.0));
}

#[test]
fn arithmetic_in_if() {
  assert_main_eq(
    "(fn main () ->Int (if (std::== (std::+ 1 1) 2) 100 200))",
    SLValue::Int(100),
  );
}

#[test]
fn multiple_lets_and_calls() {
  assert_main_eq(
    "(fn main () ->Int (let a 1) (let b 2) (let c 3) (std::+ a (std::+ b c)))",
    SLValue::Int(6),
  );
}

#[test]
fn calls_function_that_calls_another() {
  assert_main_eq(
      "(fn inc (n:Int) ->Int (std::+ n 1)) (fn twice (n:Int) ->Int (std::+ (inc n) (inc n))) (fn main () ->Int (twice 10))",
      SLValue::Int(22),
    );
}

#[test]
fn recursion_with_base_case() {
  assert_main_eq(
      "(fn triangle (n:Int) ->Int (if (std::== n 0) 0 (std::+ n (triangle (std::- n 1))))) (fn main () ->Int (triangle 10))",
      SLValue::Int(55),
    );
}

#[test]
fn deep_recursion() {
  assert_main_eq(
      "(fn triangle (n:Int) ->Int (if (std::== n 0) 0 (std::+ n (triangle (std::- n 1))))) (fn main () ->Int (triangle 10000))",
      SLValue::Int(50_005_000),
    );
}

#[test]
fn empty_function_body_is_rejected() {
  let err = run_main("(fn main () ->Int )").unwrap_err();
  assert!(
    err.contains("`fn` must have at least one body expression"),
    "got: {err}"
  );
}

#[test]
fn unsupported_string_errors_clearly() {
  let err = run_main("(fn main () ->String \"hi\")").unwrap_err();
  assert!(err.contains("does not yet support"), "got: {err}");
}

#[test]
fn unsupported_dynamic_call_errors_clearly() {
  let err = run_main("(fn main () ->Int (undefined 1))").unwrap_err();
  assert!(err.contains("unknown function"), "got: {err}");
}

#[test]
fn compiled_module_validates() {
  let wasm = compile(
    "(fn id (a:Int) ->Int a) (fn main () ->Int (id 7))",
    &std_builtins(),
    &[],
  )
  .unwrap();
  let engine = Engine::default();
  Module::from_binary(&engine, &wasm).expect("emitted wasm should validate");
}

#[test]
fn compile_rejects_type_errors_before_wasm_codegen() {
  let source = "(fn main () ->Int\n  (std::+ 1\n    true))";
  let error = compile(source, &std_builtins(), &[]).unwrap_err();
  assert!(error.starts_with("line 3, column 5: TypeError:"), "{error}");
  assert!(error.contains("expected `Int`, got `Bool`"), "{error}");
}

#[test]
fn custom_builtin_with_different_module_name() {
  let builtins = Builtins::new().with_builtin(Builtin::unary(
    "math",
    "double",
    sig(&[], vec![TypeConst::Int], None, TypeConst::Int),
    |v| match v {
      SLValue::Int(n) => SLValue::Int(n * 2),
      _ => SLValue::Void,
    },
  ));
  let result = run_main_with("(fn main () ->Int (math::double 21))", &builtins).unwrap();
  assert_eq!(result, SLValue::Int(42));
}

#[test]
fn bare_builtin_name_requires_qualification() {
  let builtins = Builtins::new().with_builtin(Builtin::unary(
    "host",
    "double",
    sig(&[], vec![TypeConst::Int], None, TypeConst::Int),
    |v| match v {
      SLValue::Int(n) => SLValue::Int(n * 2),
      _ => SLValue::Void,
    },
  ));
  let error = run_main_with("(fn main () ->Int (double 21))", &builtins).unwrap_err();
  assert!(error.contains("unknown function `double`"), "{error}");
}

#[test]
fn unused_builtins_are_not_emitted() {
  let wasm = compile("(fn main () ->Int (std::+ 1 2))", &std_builtins(), &[]).unwrap();
  let engine = Engine::default();
  let module = Module::from_binary(&engine, &wasm).unwrap();
  let num_imports = module.imports().filter(|i| i.module() == "std").count();
  assert_eq!(num_imports, 1);
}
