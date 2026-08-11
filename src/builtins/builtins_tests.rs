use super::rand::{rand_rng, rand_roll};
use super::*;
use crate::compiler::compile_executable_from_source;
use crate::interpreter::{Execution, Interpreter, SLValue};
use rstest::rstest;

#[test]
fn signatures_own_their_type_declarations() {
  let variable_name = String::from("Element");
  let signature = sig(
    &[(variable_name.as_str(), &[Trait::Eq])],
    vec![Signature::var(variable_name.clone())],
    None,
    Signature::list(Signature::var(variable_name.clone())),
  );
  drop(variable_name);

  assert_eq!(signature.type_vars[0].0, "Element");
  assert_eq!(signature.params[0], Signature::var("Element"));
}

#[test]
fn default_library_merges_std_and_rand_libraries() {
  let std_library = super::std::library();
  let rand_library = super::rand::library();

  assert!(std_library
    .builtins()
    .all(|builtin| builtin.spec().module == "std"));
  assert_eq!(std_library.types().count(), 0);
  assert!(std_library.prelude().is_empty());

  assert!(rand_library
    .builtins()
    .all(|builtin| builtin.spec().module == "rand"));
  assert_eq!(
    rand_library
      .types()
      .map(|type_| (type_.module, type_.name))
      .collect::<Vec<_>>(),
    vec![("rand", "Rng")]
  );
  assert!(rand_library.prelude().is_empty());

  let expected_builtins = std_library
    .builtins()
    .chain(rand_library.builtins())
    .map(|builtin| (builtin.spec().module, builtin.spec().name))
    .collect::<Vec<_>>();
  let default_library = Library::default();
  assert_eq!(
    default_library
      .builtins()
      .map(|builtin| (builtin.spec().module, builtin.spec().name))
      .collect::<Vec<_>>(),
    expected_builtins
  );
  assert_eq!(
    default_library.prelude().len(),
    std_library.builtins().count()
  );
}

/// Helper: evaluate `main` with [`Library::default`] and return the result.
fn eval_builtin_main(source: &str) -> Result<SLValue, String> {
  let pkg = compile_executable_from_source(source, ("main", "main"), &Library::default())?;
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();
  exec.run_until_done()
}

fn maybe_int_library() -> Library {
  Library::new()
    .with_type(CustomTypeSpec::enum_(
      "host",
      "MaybeInt",
      vec![("Some", vec![("value", Signature::Int)]), ("None", vec![])],
    ))
    .with_builtin(Builtin::contextual_value(
      "host",
      "some",
      Some(1),
      sig(
        &[],
        vec![Signature::Int],
        None,
        Signature::named("host", "MaybeInt"),
      ),
      |ctx, args| ctx.alloc_enum("host", "MaybeInt", "Some", vec![args[0]]),
    ))
    .with_builtin(Builtin::contextual_value(
      "host",
      "none",
      Some(0),
      sig(&[], vec![], None, Signature::named("host", "MaybeInt")),
      |ctx, _args| ctx.alloc_enum("host", "MaybeInt", "None", vec![]),
    ))
    .with_builtin(Builtin::contextual_value(
      "host",
      "get-or-zero",
      Some(1),
      sig(
        &[],
        vec![Signature::named("host", "MaybeInt")],
        None,
        Signature::Int,
      ),
      |ctx, args| {
        let instance = ctx.enum_instance(&args[0], "host", "MaybeInt")?;
        match (instance.variant, instance.fields.as_slice()) {
          (0, [Value::Int(value)]) => Ok(Value::Int(*value)),
          (1, []) => Ok(Value::Int(0)),
          _ => Err("invalid host::MaybeInt value".to_string()),
        }
      },
    ))
}

#[test]
fn host_defined_enums_can_be_constructed_by_builtins_and_matched() {
  let library = maybe_int_library();
  let package = compile_executable_from_source(
    "(fn main (use_some:Bool) ->Int
       (let maybe
         (if use_some (host::some 42) (host::none)))
       (match maybe
         (Some value) => value
         (None) => 0))",
    ("main", "main"),
    &library,
  )
  .unwrap_or_else(|e| panic!("compile failed: {e}"));
  let mut some = Interpreter::with_library(package.clone(), library.clone())
    .call_main_with(vec![SLValue::Bool(true)])
    .unwrap_or_else(|e| panic!("call_main_with failed: {e}"));
  let mut none = Interpreter::with_library(package, library)
    .call_main_with(vec![SLValue::Bool(false)])
    .unwrap_or_else(|e| panic!("call_main_with failed: {e}"));

  assert_eq!(some.run_until_done().unwrap(), SLValue::Int(42));
  assert_eq!(none.run_until_done().unwrap(), SLValue::Int(0));
}

/// Host-defined enums can be constructed directly from source with the
/// `new module::Type::Variant` form, not only through builtin allocators.
#[test]
fn host_defined_enums_can_be_constructed_from_source() {
  let library = maybe_int_library();
  let package = compile_executable_from_source(
    "(fn main (use_some:Bool) ->Int
       (let maybe
         (if use_some
           (new host::MaybeInt::Some value:42)
           (new host::MaybeInt::None)))
       (match maybe
         (Some value) => value
         (None) => 0))",
    ("main", "main"),
    &library,
  )
  .unwrap_or_else(|e| panic!("compile failed: {e}"));
  let mut some = Interpreter::with_library(package.clone(), library.clone())
    .call_main_with(vec![SLValue::Bool(true)])
    .unwrap_or_else(|e| panic!("call_main_with failed: {e}"));
  let mut none = Interpreter::with_library(package, library)
    .call_main_with(vec![SLValue::Bool(false)])
    .unwrap_or_else(|e| panic!("call_main_with failed: {e}"));

  assert_eq!(some.run_until_done().unwrap(), SLValue::Int(42));
  assert_eq!(none.run_until_done().unwrap(), SLValue::Int(0));
}

/// A value built with `new module::Type::Variant` is indistinguishable from one
/// built by a builtin allocator: the host can consume it with `enum_instance`.
#[test]
fn source_constructed_host_enums_can_be_consumed_by_builtins() {
  let library = maybe_int_library();
  let package = compile_executable_from_source(
    "(fn main (use_some:Bool) ->Int
       (host::get-or-zero
         (if use_some
           (new host::MaybeInt::Some value:42)
           (new host::MaybeInt::None))))",
    ("main", "main"),
    &library,
  )
  .unwrap_or_else(|e| panic!("compile failed: {e}"));
  let mut some = Interpreter::with_library(package.clone(), library.clone())
    .call_main_with(vec![SLValue::Bool(true)])
    .unwrap_or_else(|e| panic!("call_main_with failed: {e}"));
  let mut none = Interpreter::with_library(package, library)
    .call_main_with(vec![SLValue::Bool(false)])
    .unwrap_or_else(|e| panic!("call_main_with failed: {e}"));

  assert_eq!(some.run_until_done().unwrap(), SLValue::Int(42));
  assert_eq!(none.run_until_done().unwrap(), SLValue::Int(0));
}

/// Constructing a host enum variant with the wrong field type is a
/// compile-time type error.
#[test]
fn source_constructed_host_enum_rejects_wrong_field_type() {
  let library = maybe_int_library();
  let error = compile_executable_from_source(
    "(fn main () ->host::MaybeInt
       (new host::MaybeInt::Some value:\"nope\"))",
    ("main", "main"),
    &library,
  )
  .unwrap_err();
  assert!(error.contains("expected `Int`"), "{error}");
  assert!(error.contains("got `String`"), "{error}");
}

#[test]
fn builtins_can_consume_host_defined_enums() {
  let library = maybe_int_library();
  let package = compile_executable_from_source(
    "(fn main (use_some:Bool) ->Int
       (host::get-or-zero
         (if use_some (host::some 42) (host::none))))",
    ("main", "main"),
    &library,
  )
  .unwrap_or_else(|e| panic!("compile failed: {e}"));
  let mut some = Interpreter::with_library(package.clone(), library.clone())
    .call_main_with(vec![SLValue::Bool(true)])
    .unwrap_or_else(|e| panic!("call_main_with failed: {e}"));
  let mut none = Interpreter::with_library(package, library)
    .call_main_with(vec![SLValue::Bool(false)])
    .unwrap_or_else(|e| panic!("call_main_with failed: {e}"));

  assert_eq!(some.run_until_done().unwrap(), SLValue::Int(42));
  assert_eq!(none.run_until_done().unwrap(), SLValue::Int(0));
}

#[test]
fn host_allocators_reject_the_wrong_custom_type_kind() {
  let library = Library::new()
    .with_type(CustomTypeSpec::enum_(
      "host",
      "MaybeInt",
      vec![("Some", vec![("value", Signature::Int)])],
    ))
    .with_builtin(Builtin::contextual_value(
      "host",
      "bad",
      Some(1),
      sig(
        &[],
        vec![Signature::Int],
        None,
        Signature::named("host", "MaybeInt"),
      ),
      |ctx, args| ctx.alloc_struct("host", "MaybeInt", vec![args[0]]),
    ))
    .with_type(CustomTypeSpec::struct_(
      "host",
      "Box",
      vec![("value", Signature::Int)],
    ))
    .with_builtin(Builtin::contextual_value(
      "host",
      "bad-enum",
      Some(1),
      sig(
        &[],
        vec![Signature::Int],
        None,
        Signature::named("host", "Box"),
      ),
      |ctx, args| ctx.alloc_enum("host", "Box", "Box", vec![args[0]]),
    ));
  let package = compile_executable_from_source(
    "(fn main () ->host::MaybeInt (host::bad 42))",
    ("main", "main"),
    &library,
  )
  .unwrap_or_else(|e| panic!("compile failed: {e}"));
  let mut exec = Interpreter::with_library(package, library.clone())
    .call_main()
    .unwrap_or_else(|e| panic!("call_main failed: {e}"));

  let error = exec.run_until_done().unwrap_err();
  assert!(
    error.contains("Struct not found: host::MaybeInt"),
    "{error}"
  );

  let package = compile_executable_from_source(
    "(fn main () ->host::Box (host::bad-enum 42))",
    ("main", "main"),
    &library,
  )
  .unwrap_or_else(|e| panic!("compile failed: {e}"));
  let mut exec = Interpreter::with_library(package, library)
    .call_main()
    .unwrap_or_else(|e| panic!("call_main failed: {e}"));

  let error = exec.run_until_done().unwrap_err();
  assert!(error.contains("Enum not found: host::Box"), "{error}");
}

#[test]
fn custom_interpreter_builtins_are_public_api() {
  let builtins = Library::new().with_builtin(Builtin::unary(
    "main",
    "add2",
    sig(&[], vec![Signature::Int], None, Signature::Int),
    |value| {
      let n = value
        .as_int()
        .map_err(|_| format!("expected Int, got {}", value.type_name()))?;
      Ok(Value::Int(n + 2))
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
    .with_builtin(Builtin::contextual_value(
      "box",
      "unbox",
      Some(1),
      sig(
        &[],
        vec![Signature::named("box", "Box")],
        None,
        Signature::Int,
      ),
      |ctx, args| {
        let struct_ = ctx.struct_type("box", "Box")?;
        let instance = ctx.args("box::unbox", args).struct_instance(0, struct_)?;
        Ok(instance.fields[0])
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
    .with_builtin(Builtin::contextual_value(
      "left",
      "unbox",
      Some(1),
      sig(
        &[],
        vec![Signature::named("left", "Box")],
        None,
        Signature::Int,
      ),
      |ctx, args| {
        let struct_ = ctx.struct_type("left", "Box")?;
        let instance = ctx.args("left::unbox", args).struct_instance(0, struct_)?;
        Ok(instance.fields[0])
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

/// Compile `source`, execute every instruction before `main`'s final
/// expression call, and leave that call ready to step. This lets tests build
/// large operands without a limit, then install a limit immediately before
/// the builtin's temporary allocation.
fn before_final_call(source: &str) -> Execution {
  let pkg = compile_executable_from_source(source, ("main", "main"), &Library::default()).unwrap();
  let (module, function) = pkg.main.unwrap();
  let instruction_count = match pkg.get_function(module, function).unwrap() {
    crate::compiler::Callable::Function(function) => function.instructions.len(),
    crate::compiler::Callable::Builtin => panic!("main must not be a builtin"),
  };
  assert!(
    instruction_count >= 2,
    "main needs a final call followed by Return"
  );
  let interp = Interpreter::new(pkg);
  let mut exec = interp.call_main().unwrap();
  for _ in 0..instruction_count - 2 {
    exec.step().unwrap();
  }
  exec
}

fn assert_final_builtin_memory_preflight_fails(
  source: &str,
  arity: usize,
  result_external_bytes: usize,
) {
  assert!(result_external_bytes > 0);
  let mut exec = before_final_call(source);
  let scratch_bytes = arity * ::std::mem::size_of::<Value<'static>>();
  let limit = exec
    .memory_usage()
    .checked_add(scratch_bytes)
    .and_then(|usage| usage.checked_add(result_external_bytes - 1))
    .unwrap();
  exec.set_memory_limit(Some(limit));
  let gc_before = exec.gc_count();

  let err = exec.step().unwrap_err();
  assert!(
    err.contains("memory limit exceeded"),
    "unexpected error for {source}: {err}"
  );
  assert_eq!(
    exec.gc_count(),
    gc_before,
    "builtin allocated GC results before rejecting its memory preflight: {source}"
  );
}

#[test]
fn allocating_builtins_check_memory_before_building_results() {
  let large = "x".repeat(512);
  let ptr = ::std::mem::size_of::<Value<'static>>();
  let cases = [
    (
      format!("(fn main () ->String (std::concat \"{large}\" \"{large}\"))"),
      2,
      1024,
    ),
    (
      "(fn main () ->(List Int) (std::list 1 2 3 4))".to_string(),
      4,
      4 * ptr,
    ),
    (
      "(fn main () ->(List Int) (std::push (std::list 1 2 3) 4))".to_string(),
      2,
      4 * ptr,
    ),
    (
      "(fn main () ->(List Int) (std::slice (std::list 1 2 3 4) 1 3))".to_string(),
      3,
      2 * ptr,
    ),
    (
      format!("(fn main () ->String (std::slice \"{large}\" 0 512))"),
      3,
      large.len(),
    ),
  ];

  for (source, arity, bytes) in cases {
    assert_final_builtin_memory_preflight_fails(&source, arity, bytes);
  }
}

#[test]
fn list_idx_returns_the_existing_value_without_cloning() {
  let large = "x".repeat(64 * 1024);
  let source = format!("(fn main () ->String (std::idx (std::list \"{large}\") 0))");
  let mut exec = before_final_call(&source);
  let scratch_bytes = 2 * ::std::mem::size_of::<Value<'static>>();
  exec.set_memory_limit(Some(exec.memory_usage() + scratch_bytes));
  let gc_before = exec.gc_count();

  exec.step().unwrap();
  assert!(
    exec.gc_count() <= gc_before,
    "idx allocated a duplicate value object"
  );
  assert_eq!(exec.peek_value().unwrap(), SLValue::String(large));
}

#[test]
fn builtin_argument_buffer_is_reserved_before_allocation() {
  let source = format!(
    "(fn main () ->(List Int) (std::list {}))",
    (0..128)
      .map(|i| i.to_string())
      .collect::<Vec<_>>()
      .join(" ")
  );
  let mut exec = before_final_call(&source);
  let scratch_bytes = 128 * ::std::mem::size_of::<Value<'static>>();
  exec.set_memory_limit(Some(exec.memory_usage() + scratch_bytes - 1));
  let gc_before = exec.gc_count();

  let err = exec.step().unwrap_err();
  assert!(err.contains("memory limit exceeded"), "unexpected: {err}");
  assert_eq!(exec.gc_count(), gc_before);
}

#[test]
fn range_native_builder_enforces_memory_limit() {
  let mut exec = before_final_call("(fn main () ->(List Int) (std::range 0 10000))");
  let value_bytes = ::std::mem::size_of::<Value<'static>>();
  let scratch_bytes = 2 * value_bytes;
  let result_value_bytes = 10_000 * value_bytes;
  exec.set_memory_limit(Some(
    exec.memory_usage() + scratch_bytes + result_value_bytes - 1,
  ));
  let err = exec.step().unwrap_err();
  assert!(err.contains("memory limit exceeded"), "unexpected: {err}");
}

#[test]
fn string_slicing_uses_character_indices() {
  assert_eq!(
    eval_builtin_main("(fn main () ->String (std::slice \"aé🦀z\" 1 3))").unwrap(),
    SLValue::String("é🦀".to_string())
  );
  assert_eq!(
    eval_builtin_main("(fn main () ->String (std::slice \"aé🦀z\" -3 -1))").unwrap(),
    SLValue::String("é🦀".to_string())
  );
  assert_eq!(
    eval_builtin_main("(fn main () ->String (std::slice \"aé🦀z\" -2 -1))").unwrap(),
    SLValue::String("🦀".to_string())
  );
}

/// End-to-end: the surface `(rand::rng seed "name")` returns an `Rng`
/// wrapping a `Cell(Int)` whose contents match the seed derivation directly.
#[rstest]
#[case::alpha(0, "alpha", -1438303955140652998)]
#[case::beta(1, "beta", 6165243067257761546)]
#[case::battle(42, "battle", -6532365554512174988)]
#[case::neg(-1, "neg", -2221088163922545247)]
#[case::weather(100, "weather", 6058102796144909055)]
#[case::loop_(7, "loop", 3200058603457882367)]
#[case::doors(256, "doors", -7515552181829398974)]
#[case::shadow(-99, "shadow", 6601820722361913051)]
#[case::big(123_456_789, "big", -7499502896394584729)]
#[case::huge(-8_589_934_592, "huge", 5640261956235639084)]
fn rand_rng_surface(#[case] seed: i64, #[case] name: &str, #[case] expected: i64) {
  let source = format!("(fn main () ->Rng (rand::rng {} \"{}\"))", seed, name);
  let result = eval_builtin_main(&source).unwrap();
  match result {
    SLValue::Struct { fields, .. } => match fields.as_slice() {
      [SLValue::Cell(inner)] => {
        assert_eq!(
          **inner,
          SLValue::Int(expected),
          "rand::rng {} {:?}",
          seed,
          name
        )
      }
      other => panic!("expected Rng state cell, got {:?}", other),
    },
    other => panic!("expected Rng from rand::rng, got {:?}", other),
  }
}

#[test]
fn host_builtins_can_construct_rng_values() {
  let library = Library::default().with_builtin(Builtin::contextual_value(
    "host",
    "rng",
    Some(1),
    sig(
      &[],
      vec![Signature::Int],
      None,
      Signature::named("rand", "Rng"),
    ),
    |ctx, args| {
      let seed = ctx.args("host::rng", args).int(0)?;
      super::rand::alloc_rng(ctx, seed)
    },
  ));
  let package = compile_executable_from_source(
    "(fn main () ->Int (rand::roll! (host::rng 42) 20))",
    ("main", "main"),
    &library,
  )
  .unwrap_or_else(|e| panic!("compile failed: {e}"));
  let mut execution = Interpreter::with_library(package, library)
    .call_main()
    .unwrap_or_else(|e| panic!("call_main failed: {e}"));

  assert_eq!(
    execution.run_until_done().unwrap(),
    SLValue::Int(rand_roll(42, 20).0)
  );
}

/// End-to-end: `(rand::roll! rng sides)` mutates the Cell<Int> `rng` in place
/// and returns the roll as an Int. Calling it 10 times against the same cell
/// reproduces the expected 10-roll chain.
#[rstest]
#[case::alpha(0, "alpha", [11, 6, 14, 18, 3, 4, 2, 7, 12, 1])]
#[case::beta(1, "beta", [18, 13, 1, 5, 18, 12, 9, 19, 4, 5])]
#[case::battle(42, "battle", [18, 17, 3, 10, 19, 15, 8, 5, 10, 12])]
#[case::neg(-1, "neg", [19, 18, 10, 12, 16, 20, 2, 1, 14, 2])]
#[case::weather(100, "weather", [13, 9, 6, 11, 15, 9, 4, 2, 15, 17])]
#[case::loop_(7, "loop", [12, 14, 15, 3, 19, 8, 1, 12, 7, 1])]
#[case::doors(256, "doors", [16, 5, 3, 10, 10, 8, 14, 12, 20, 9])]
#[case::shadow(-99, "shadow", [12, 4, 14, 9, 12, 7, 6, 15, 3, 18])]
#[case::big(123_456_789, "big", [17, 8, 18, 16, 14, 11, 11, 4, 10, 9])]
#[case::huge(-8_589_934_592, "huge", [4, 18, 7, 17, 11, 14, 15, 18, 1, 9])]
fn rand_roll_surface_chain(#[case] seed: i64, #[case] name: &str, #[case] expected: [i64; 10]) {
  // (std::map (std::range 0 10) (fn roll (_idx:Int) ->Int (rand::roll! rng 20)))
  //
  // `std::map` applies `roll` to each element of `(std::range 0 10)` and
  // collects the rolls. `roll` ignores its argument (the index) and captures
  // the explicit `rng` cell, which `rand::roll!` mutates.
  let src = format!(
    "(fn main () ->(List Int)\n\
        (let rng (rand::rng {seed} \"{name}\"))\n\
        (fn roll (_idx:Int) ->Int (rand::roll! rng 20))\n\
        (std::map (std::range 0 10) roll))"
  );
  let result = eval_builtin_main(&src).unwrap();
  let got: Vec<i64> = match result {
    SLValue::List(items) => items
      .into_iter()
      .map(|v| match v {
        SLValue::Int(i) => i,
        other => panic!("expected Int in roll list, got {:?}", other),
      })
      .collect(),
    other => panic!("expected List from std::map, got {:?}", other),
  };
  assert_eq!(got, expected.to_vec(), "seed={} name={:?}", seed, name);
}

/// `std::range` produces a half-open list of ints, like Python's `range`.
#[test]
fn range_basic() {
  assert_eq!(
    eval_builtin_main("(fn main () ->(List Int) (std::range 0 5))").unwrap(),
    SLValue::List(vec![
      SLValue::Int(0),
      SLValue::Int(1),
      SLValue::Int(2),
      SLValue::Int(3),
      SLValue::Int(4)
    ])
  );
}

#[test]
fn range_empty() {
  assert_eq!(
    eval_builtin_main("(fn main () ->(List Int) (std::range 3 3))").unwrap(),
    SLValue::List(vec![])
  );
  assert_eq!(
    eval_builtin_main("(fn main () ->(List Int) (std::range 5 2))").unwrap(),
    SLValue::List(vec![])
  );
}

#[test]
fn range_negative_start() {
  assert_eq!(
    eval_builtin_main("(fn main () ->(List Int) (std::range -2 2))").unwrap(),
    SLValue::List(vec![
      SLValue::Int(-2),
      SLValue::Int(-1),
      SLValue::Int(0),
      SLValue::Int(1)
    ])
  );
}

/// `std::map` applies a function to each element of a list.
#[test]
fn map_doubles() {
  assert_eq!(
    eval_builtin_main(
      "(fn main () ->(List Int)
           (fn dbl (x:Int) ->Int (std::+ x x))
           (std::map (std::list 1 2 3) dbl))"
    )
    .unwrap(),
    SLValue::List(vec![SLValue::Int(2), SLValue::Int(4), SLValue::Int(6)])
  );
}

#[test]
fn map_empty_list() {
  assert_eq!(
    eval_builtin_main(
      "(fn main () ->(List Int)
           (fn id (x:Int) ->Int x)
           (std::map (std::list) id))"
    )
    .unwrap(),
    SLValue::List(vec![])
  );
}

#[test]
fn map_with_local_closure() {
  assert_eq!(
    eval_builtin_main(
      "(fn main () ->(List Int)
           (fn inc (x:Int) ->Int (std::+ x 1))
           (std::map (std::range 0 5) inc))"
    )
    .unwrap(),
    SLValue::List(vec![
      SLValue::Int(1),
      SLValue::Int(2),
      SLValue::Int(3),
      SLValue::Int(4),
      SLValue::Int(5)
    ])
  );
}

#[test]
fn map_builds_large_persistent_list_in_order() {
  let result = eval_builtin_main(
    "(fn main () ->(List Int)
       (fn id (x:Int) ->Int x)
       (std::map (std::range 0 1000) id))",
  )
  .unwrap();
  let expected = (0..1_000).map(SLValue::Int).collect();
  assert_eq!(result, SLValue::List(expected));
}

#[test]
fn map_non_list_errors() {
  let err = eval_builtin_main(
    "(fn main () ->Int
         (fn id (x:Int) ->Int x)
         (std::map 5 id))",
  )
  .unwrap_err();
  assert!(err.contains("expected `(List"), "got: {}", err);
}

/// `rand::roll!` rejects non-positive sides with a runtime error.
#[test]
fn rand_roll_rejects_non_positive_sides() {
  let err =
    eval_builtin_main("(fn main () ->Int (rand::roll! (rand::rng 0 \"x\") 0))").unwrap_err();
  assert!(err.contains("sides must be positive"), "got: {}", err);
}

/// `rand::roll!` rejects a non-Rng value.
#[test]
fn rand_roll_rejects_non_rng() {
  let err = eval_builtin_main("(fn main () ->Int (rand::roll! \"not-a-cell\" 6))").unwrap_err();
  assert!(err.contains("expected `rand::Rng`"), "got: {}", err);
}

/// `rand::rng` rejects non-Int seeds.
#[test]
fn rand_rng_rejects_non_int_seed() {
  let err = eval_builtin_main("(fn main () ->Int (rand::rng \"x\" \"name\"))").unwrap_err();
  assert!(err.contains("expected `Int`"), "got: {}", err);
}

#[test]
fn rand_choice_surface_chain_advances_rng_state() {
  let seed = 42;
  let name = "choice";
  let mut state = rand_rng(seed, name);
  let items = [10, 20, 30];
  let expected = (0..8)
    .map(|_| {
      let (roll, next) = rand_roll(state, items.len() as i64);
      state = next;
      SLValue::Int(items[(roll - 1) as usize])
    })
    .collect::<Vec<_>>();
  let source = format!(
    "(fn main () ->(List Int)
       (let rng (rand::rng {seed} \"{name}\"))
       (let items (std::list 10 20 30))
       (std::list
         (rand::choice! rng items) (rand::choice! rng items)
         (rand::choice! rng items) (rand::choice! rng items)
         (rand::choice! rng items) (rand::choice! rng items)
         (rand::choice! rng items) (rand::choice! rng items)))"
  );

  assert_eq!(eval_builtin_main(&source).unwrap(), SLValue::List(expected));
}

#[test]
fn rand_choice_of_singleton_still_advances_rng_state() {
  let seed = 7;
  let name = "singleton";
  let initial = rand_rng(seed, name);
  let (_, next) = rand_roll(initial, 1);
  let expected_roll = rand_roll(next, 20).0;
  let source = format!(
    "(fn main () ->(List Int)
       (let rng (rand::rng {seed} \"{name}\"))
       (std::list
         (rand::choice! rng (std::list 99))
         (rand::roll! rng 20)))"
  );

  assert_eq!(
    eval_builtin_main(&source).unwrap(),
    SLValue::List(vec![SLValue::Int(99), SLValue::Int(expected_roll)])
  );
}

#[test]
fn rand_choice_rejects_an_empty_list() {
  let err =
    eval_builtin_main("(fn main () ->Int (rand::choice! (rand::rng 0 \"empty\") (std::list)))")
      .unwrap_err();
  assert!(err.contains("empty list"), "got: {err}");
}

#[test]
fn host_builtins_can_receive_and_return_tuples() {
  let library = Library::new()
    .with_builtin(Builtin::contextual_value(
      "host",
      "swap",
      Some(1),
      sig(
        &[],
        vec![Signature::tuple(vec![Signature::Int, Signature::String])],
        None,
        Signature::tuple(vec![Signature::String, Signature::Int]),
      ),
      |ctx, args| {
        let elements = ctx.args("host::swap", args).tuple(0)?;
        let n = elements[0]
          .as_int()
          .map_err(|_| "expected Int as first tuple element".to_string())?;
        // Reorder the elements: String first, Int second.
        ctx.alloc_tuple(vec![elements[1], Value::Int(n)])
      },
    ))
    .with_builtin(Builtin::contextual_value(
      "host",
      "first",
      Some(1),
      sig(
        &[],
        vec![Signature::tuple(vec![Signature::Int, Signature::Int])],
        None,
        Signature::Int,
      ),
      |ctx, args| {
        let elements = ctx.args("host::first", args).tuple(0)?;
        Ok(Value::Int(elements[0].as_int().map_err(|_| {
          "expected Int as first tuple element".to_string()
        })?))
      },
    ));

  let package = compile_executable_from_source(
    "(fn main () -> (Tuple String Int)
       (host::swap (Tuple 7 \"hi\")))",
    ("main", "main"),
    &library,
  )
  .unwrap_or_else(|e| panic!("compile failed: {e}"));
  let mut exec = Interpreter::with_library(package, library.clone())
    .call_main()
    .unwrap_or_else(|e| panic!("call_main failed: {e}"));
  assert_eq!(
    exec.run_until_done().unwrap(),
    SLValue::Tuple(vec![SLValue::String("hi".to_string()), SLValue::Int(7)]),
  );

  let package = compile_executable_from_source(
    "(fn main () -> Int
       (host::first (Tuple 9 2)))",
    ("main", "main"),
    &library,
  )
  .unwrap_or_else(|e| panic!("compile failed: {e}"));
  let mut exec = Interpreter::with_library(package, library)
    .call_main()
    .unwrap_or_else(|e| panic!("call_main failed: {e}"));
  assert_eq!(exec.run_until_done().unwrap(), SLValue::Int(9));
}

/// `std::remove-idx` returns the element at `index` paired with the list with
/// that element removed, as a `(Tuple A (List A))`. Negative indices count from
/// the end, mirroring `std::idx`.
#[test]
fn remove_idx_returns_first_element_and_remaining_list() {
  assert_eq!(
    eval_builtin_main(
      "(fn main () -> (Tuple Int (List Int)) (std::remove-idx (std::list 1 2 3) 0))"
    )
    .unwrap(),
    SLValue::Tuple(vec![
      SLValue::Int(1),
      SLValue::List(vec![SLValue::Int(2), SLValue::Int(3)]),
    ])
  );
}

#[test]
fn remove_idx_removes_from_the_middle() {
  assert_eq!(
    eval_builtin_main(
      "(fn main () -> (Tuple Int (List Int)) (std::remove-idx (std::list 1 2 3) 1))"
    )
    .unwrap(),
    SLValue::Tuple(vec![
      SLValue::Int(2),
      SLValue::List(vec![SLValue::Int(1), SLValue::Int(3)]),
    ])
  );
}

#[test]
fn remove_idx_negative_index_counts_from_end() {
  assert_eq!(
    eval_builtin_main(
      "(fn main () -> (Tuple Int (List Int)) (std::remove-idx (std::list 1 2 3) -1))"
    )
    .unwrap(),
    SLValue::Tuple(vec![
      SLValue::Int(3),
      SLValue::List(vec![SLValue::Int(1), SLValue::Int(2)]),
    ])
  );
}

#[test]
fn remove_idx_negative_index_minus_two() {
  assert_eq!(
    eval_builtin_main(
      "(fn main () -> (Tuple Int (List Int)) (std::remove-idx (std::list 1 2 3) -2))"
    )
    .unwrap(),
    SLValue::Tuple(vec![
      SLValue::Int(2),
      SLValue::List(vec![SLValue::Int(1), SLValue::Int(3)]),
    ])
  );
}

#[test]
fn remove_idx_single_element_list_yields_empty_list() {
  assert_eq!(
    eval_builtin_main("(fn main () -> (Tuple Int (List Int)) (std::remove-idx (std::list 7) 0))")
      .unwrap(),
    SLValue::Tuple(vec![SLValue::Int(7), SLValue::List(vec![])])
  );
}

#[test]
fn remove_idx_leaves_original_list_unchanged() {
  let source = "(fn main () -> (List Int)
    (let xs (std::list 1 2 3))
    (std::remove-idx xs 1)
    xs)";
  assert_eq!(
    eval_builtin_main(source).unwrap(),
    SLValue::List(vec![SLValue::Int(1), SLValue::Int(2), SLValue::Int(3)])
  );
}

#[test]
fn remove_idx_out_of_range_errors() {
  let err = eval_builtin_main(
    "(fn main () -> (Tuple Int (List Int)) (std::remove-idx (std::list 1 2 3) 3))",
  )
  .unwrap_err();
  assert!(err.contains("remove-idx"), "got: {}", err);
  assert!(err.contains("out of range"), "got: {}", err);
}

#[test]
fn remove_idx_negative_out_of_range_errors() {
  let err = eval_builtin_main(
    "(fn main () -> (Tuple Int (List Int)) (std::remove-idx (std::list 1 2 3) -4))",
  )
  .unwrap_err();
  assert!(err.contains("remove-idx"), "got: {}", err);
  assert!(err.contains("out of range"), "got: {}", err);
}

#[test]
fn remove_idx_on_empty_list_errors() {
  let err =
    eval_builtin_main("(fn main () -> (Tuple Int (List Int)) (std::remove-idx (std::list) 0))")
      .unwrap_err();
  assert!(err.contains("remove-idx"), "got: {}", err);
  assert!(err.contains("out of range"), "got: {}", err);
}

#[test]
fn remove_idx_non_list_errors() {
  let err =
    eval_builtin_main("(fn main () -> (Tuple Int (List Int)) (std::remove-idx 5 0))").unwrap_err();
  assert!(err.contains("expected"), "got: {}", err);
  assert!(err.contains("List"), "got: {}", err);
}

#[test]
fn remove_idx_non_int_index_errors() {
  let err = eval_builtin_main(
    "(fn main () -> (Tuple Int (List Int)) (std::remove-idx (std::list 1 2 3) \"x\"))",
  )
  .unwrap_err();
  assert!(err.contains("expected"), "got: {}", err);
  assert!(err.contains("Int"), "got: {}", err);
}
