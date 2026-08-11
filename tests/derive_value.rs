//! End-to-end tests for the `SafelispValue` derive macro
use safelisp::{
  compile_executable_from_source, sig, Builtin, Interpreter, Library, SLValue, SafelispType,
  SafelispValue, Signature, Value,
};

/// A struct with named fields.
#[derive(Debug, PartialEq, SafelispValue)]
#[safelisp(module = "arp")]
struct Point {
  x: i64,
  y: i64,
}

/// A tuple struct. Positional fields require explicit SafeLisp names.
#[derive(Debug, PartialEq, SafelispValue)]
#[safelisp(module = "arp")]
struct Vec2(#[safelisp(field = "x")] f64, #[safelisp(field = "y")] f64);

/// The motivating example: a recursive enum with named, tuple, and unit
/// variants.
#[derive(Debug, PartialEq, SafelispValue)]
#[safelisp(module = "arp")]
enum Dice {
  Expr {
    num: u8,
    size: u8,
  },
  Plus(
    #[safelisp(field = "left")] Box<Dice>,
    #[safelisp(field = "right")] Box<Dice>,
  ),
  Flat {
    value: i8,
  },
  BestOf(
    #[safelisp(field = "count")] u8,
    #[safelisp(field = "dice")] Box<Dice>,
  ),
  Crit,
}

fn library() -> Library {
  Library::new()
    .with_type(Point::type_spec())
    .with_type(Vec2::type_spec())
    .with_type(Dice::type_spec())
    .with_builtin(Builtin::contextual_value(
      "host",
      "point-roundtrip",
      Some(0),
      sig(&[], vec![], None, Signature::Int),
      |_ctx, _args| {
        let point = Point { x: 3, y: 4 };
        let value = point.to_value(_ctx)?;
        let back = Point::from_value(_ctx, value)?;
        Ok(Value::Int(if back == point { 1 } else { 0 }))
      },
    ))
    .with_builtin(Builtin::contextual_value(
      "host",
      "vec2-roundtrip",
      Some(0),
      sig(&[], vec![], None, Signature::Int),
      |ctx, _args| {
        let v = Vec2(1.5, -2.25);
        let value = v.to_value(ctx)?;
        let back = Vec2::from_value(ctx, value)?;
        Ok(Value::Int(if back == v { 1 } else { 0 }))
      },
    ))
    .with_builtin(Builtin::contextual_value(
      "host",
      "dice-roundtrip",
      Some(0),
      sig(&[], vec![], None, Signature::Int),
      |ctx, _args| {
        let dice = Dice::BestOf(
          2,
          Box::new(Dice::Plus(
            Box::new(Dice::Expr { num: 2, size: 6 }),
            Box::new(Dice::Flat { value: -1 }),
          )),
        );
        let value = dice.to_value(ctx)?;
        let back = Dice::from_value(ctx, value)?;
        Ok(Value::Int(if back == dice { 1 } else { 0 }))
      },
    ))
    .with_builtin(Builtin::contextual_value(
      "host",
      "dice-num",
      Some(1),
      sig(
        &[],
        vec![Signature::named("arp", "Dice")],
        None,
        Signature::Int,
      ),
      |ctx, args| {
        let dice = Dice::from_value(ctx, args[0])?;
        let num = match dice {
          Dice::Expr { num, .. } => i64::from(num),
          _ => -100,
        };
        Ok(Value::Int(num))
      },
    ))
    .with_builtin(Builtin::contextual_value(
      "host",
      "make-flat",
      Some(0),
      sig(&[], vec![], None, Signature::named("arp", "Dice")),
      |ctx, _args| Dice::Flat { value: -3 }.to_value(ctx),
    ))
    .with_builtin(Builtin::contextual_value(
      "host",
      "dice-best-of-count",
      Some(1),
      sig(
        &[],
        vec![Signature::named("arp", "Dice")],
        None,
        Signature::Int,
      ),
      |ctx, args| {
        let dice = Dice::from_value(ctx, args[0])?;
        let count = match dice {
          Dice::BestOf(count, _) => i64::from(count),
          _ => -100,
        };
        Ok(Value::Int(count))
      },
    ))
}

fn run(source: &str) -> Result<SLValue, String> {
  let pkg = compile_executable_from_source(source, ("main", "main"), &library())?;
  let interp = Interpreter::with_library(pkg, library());
  let mut exec = interp.call_main()?;
  exec.run_until_done()
}

#[test]
fn point_roundtrips_through_arena() {
  assert_eq!(
    run("(fn main () ->Int (host::point-roundtrip))"),
    Ok(SLValue::Int(1))
  );
}

#[test]
fn tuple_struct_roundtrips_through_arena() {
  assert_eq!(
    run("(fn main () ->Int (host::vec2-roundtrip))"),
    Ok(SLValue::Int(1))
  );
}

#[test]
fn recursive_enum_roundtrips_through_arena() {
  assert_eq!(
    run("(fn main () ->Int (host::dice-roundtrip))"),
    Ok(SLValue::Int(1))
  );
}

#[test]
fn from_value_reads_source_constructed_variant() {
  let source = "(fn main () ->Int (host::dice-num (new arp::Dice::Expr num:2 size:6)))";
  assert_eq!(run(source), Ok(SLValue::Int(2)));
}

#[test]
fn to_value_result_is_matchable_in_source() {
  // With `#[safelisp(field = "...")]` naming the positional fields, every
  // variant — including the tuple variants — is matchable by name in source,
  // so the match is fully exhaustive without a default arm.
  let source = "(fn main () ->Int
  (match (host::make-flat)
    (Expr num size) => 0
    (Plus left right) => 1
    (Flat value) => value
    (BestOf count dice) => 2
    (Crit) => 3))";
  assert_eq!(run(source), Ok(SLValue::Int(-3)));
}

#[test]
fn tuple_variant_is_constructible_in_source() {
  // A tuple variant with named positional fields can be built with `new` and
  // read back by the host via `from_value`.
  let source = "(fn main () ->Int (host::dice-best-of-count (new arp::Dice::BestOf count:2 dice:(new arp::Dice::Crit))))";
  assert_eq!(run(source), Ok(SLValue::Int(2)));
}

#[test]
fn readme_match_example_compiles() {
  // Mirrors the match example in README.md's "Deriving Conversions" section:
  // constructs a tuple variant by name and matches it (with a default arm).
  let source = "(fn main () ->Int
  (match (new arp::Dice::BestOf count:2 dice:(new arp::Dice::Flat value:-1))
    (BestOf count dice) => count
    _ => 0))";
  assert_eq!(run(source), Ok(SLValue::Int(2)));
}
