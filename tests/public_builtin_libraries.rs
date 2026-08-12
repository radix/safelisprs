use safelisp::builtins::{rand, std};
use safelisp::{
  compile_executable_from_source, sig, Builtin, HostCtx, Interpreter, SLValue, Signature, Value,
};

fn seeded_rng<'gc, 'call>(
  ctx: &mut HostCtx<'gc, 'call>,
  args: &[Value<'gc>],
) -> Result<Value<'gc>, String> {
  let seed = ctx.args("host::rng", args).int(0)?;
  rand::alloc_rng(ctx, seed)
}

#[test]
fn builtin_libraries_and_rng_allocator_are_public() {
  let library = std::library()
    .merge(rand::library())
    .with_builtin(Builtin::contextual_value(
      "host",
      "rng",
      Some(1),
      sig(
        &[],
        vec![Signature::Int],
        None,
        Signature::named("rand", "Rng"),
      ),
      seeded_rng,
    ));
  let package = compile_executable_from_source(
    "[fn main [] ->Int [rand::roll! [host::rng 42] 20]]",
    ("main", "main"),
    &library,
  )
  .unwrap_or_else(|error| panic!("compile failed: {error}"));
  let mut execution = Interpreter::with_library(package, library)
    .call_main()
    .unwrap_or_else(|error| panic!("call_main failed: {error}"));

  let result = execution.run_until_done().unwrap();
  assert!(matches!(result, SLValue::Int(1..=20)), "{result:?}");
}
