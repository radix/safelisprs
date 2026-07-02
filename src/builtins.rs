use std::sync::Arc;

use blake3::Hasher;
use gc_arena::{Gc, Mutation, RefLock};
use rand_chacha::rand_core::{RngCore, SeedableRng};
use rand_chacha::ChaCha8Rng;

use crate::interpreter::SLVal;

/// A compile-time description of a builtin: which module/name it lives in and
/// how many arguments it takes. `num_params` is `None` for variadic builtins.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct BuiltinSpec {
  pub module: &'static str,
  pub name: &'static str,
  pub num_params: Option<u16>,
}

/// A builtin's runtime handler. Takes the GC `Mutation` context (so builtins
/// may allocate fresh `Gc` values) and the evaluated arguments (as `Gc`
/// pointers into the current execution's arena), and returns either a result
/// value or a runtime error string.
///
/// The `for<'gc>` higher-ranked bound lets one `'static` handler serve any
/// execution's arena.
pub type HostFn =
  Arc<dyn for<'gc> Fn(&Mutation<'gc>, &[Gc<'gc, SLVal<'gc>>]) -> Result<SLVal<'gc>, String>>;

/// A builtin: metadata ([`BuiltinSpec`]) plus its handler ([`HostFn`]).
#[derive(Clone)]
pub struct Builtin {
  spec: BuiltinSpec,
  func: HostFn,
}

impl Builtin {
  pub fn spec(&self) -> BuiltinSpec {
    self.spec
  }

  /// Invoke this builtin's handler with the given arguments.
  pub fn call<'gc>(
    &self,
    mc: &'gc Mutation<'gc>,
    args: &[Gc<'gc, SLVal<'gc>>],
  ) -> Result<SLVal<'gc>, String> {
    (self.func)(mc, args)
  }

  /// A unary (one-arg) builtin.
  pub fn unary(
    module: &'static str,
    name: &'static str,
    func: impl for<'gc> Fn(Gc<'gc, SLVal<'gc>>) -> Result<SLVal<'gc>, String> + 'static,
  ) -> Self {
    Builtin {
      spec: BuiltinSpec {
        module,
        name,
        num_params: Some(1),
      },
      func: Arc::new(move |_mc, args| func(args[0])),
    }
  }

  /// A binary (two-arg) builtin. `func` receives `(left, right)`.
  pub fn binary(
    module: &'static str,
    name: &'static str,
    func: impl for<'gc> Fn(Gc<'gc, SLVal<'gc>>, Gc<'gc, SLVal<'gc>>) -> Result<SLVal<'gc>, String>
      + 'static,
  ) -> Self {
    Builtin {
      spec: BuiltinSpec {
        module,
        name,
        num_params: Some(2),
      },
      func: Arc::new(move |_mc, args| func(args[0], args[1])),
    }
  }

  /// A binary builtin that needs the GC `Mutation` context (e.g. to allocate a
  /// fresh `List`). `func` receives `(mc, left, right)`. The `mc` reference's
  /// lifetime is left elided (independent of `'gc`) so that the coercion into
  /// `HostFn` — whose `&Mutation<'gc>` and `&[Gc<'gc, …>]` references also have
  /// independent lifetimes — succeeds despite `SLVal<'gc>` being invariant.
  pub fn binary_alloc(
    module: &'static str,
    name: &'static str,
    func: impl for<'gc> Fn(
        &Mutation<'gc>,
        Gc<'gc, SLVal<'gc>>,
        Gc<'gc, SLVal<'gc>>,
      ) -> Result<SLVal<'gc>, String>
      + 'static,
  ) -> Self {
    Builtin {
      spec: BuiltinSpec {
        module,
        name,
        num_params: Some(2),
      },
      func: Arc::new(move |mc, args| func(mc, args[0], args[1])),
    }
  }

  /// A ternary (three-arg) builtin.
  pub fn ternary(
    module: &'static str,
    name: &'static str,
    func: impl for<'gc> Fn(
        Gc<'gc, SLVal<'gc>>,
        Gc<'gc, SLVal<'gc>>,
        Gc<'gc, SLVal<'gc>>,
      ) -> Result<SLVal<'gc>, String>
      + 'static,
  ) -> Self {
    Builtin {
      spec: BuiltinSpec {
        module,
        name,
        num_params: Some(3),
      },
      func: Arc::new(move |_mc, args| func(args[0], args[1], args[2])),
    }
  }

  /// A variadic builtin: it receives the whole argument slice and may be called
  /// with any number of args (including zero). `num_params` is `None`, so the
  /// interpreter uses the call-site arity (carried on `Instruction::Call` /
  /// `Instruction::CallDynamic`) to know how many args to pop.
  pub fn variadic(
    module: &'static str,
    name: &'static str,
    func: impl for<'gc> Fn(&[Gc<'gc, SLVal<'gc>>]) -> Result<SLVal<'gc>, String> + 'static,
  ) -> Self {
    Builtin {
      spec: BuiltinSpec {
        module,
        name,
        num_params: None,
      },
      func: Arc::new(move |_mc, args| func(args)),
    }
  }
}

/// A registry of builtins available to a program. The compiler reads the
/// [`BuiltinSpec`]s (via [`Builtins::specs`]) to register `Callable::Builtin`
/// slots; the interpreter looks up the handler (via [`Builtins::lookup`]) at
/// runtime. Mirrors the WASM backend's `wasm::Builtins`.
#[derive(Clone, Default)]
pub struct Builtins {
  entries: Vec<Builtin>,
}

impl Builtins {
  pub fn new() -> Self {
    Self::default()
  }

  /// Register a builtin (builder style).
  pub fn with_builtin(mut self, builtin: Builtin) -> Self {
    self.entries.push(builtin);
    self
  }

  pub fn iter(&self) -> impl Iterator<Item = &Builtin> {
    self.entries.iter()
  }

  /// The specs of all registered builtins, for the compiler to inject as
  /// `Callable::Builtin` slots.
  pub fn specs(&self) -> Vec<BuiltinSpec> {
    self.entries.iter().map(|b| b.spec).collect()
  }

  /// Look up a builtin by `(module, name)`.
  pub fn lookup(&self, module: &str, name: &str) -> Option<&Builtin> {
    self
      .entries
      .iter()
      .find(|b| b.spec.module == module && b.spec.name == name)
  }
}

/// The default builtin registry.
pub fn default_builtins() -> Builtins {
  Builtins::new()
    .with_builtin(Builtin::binary("std", "+", |a, b| match (&*a, &*b) {
      (SLVal::Int(x), SLVal::Int(y)) => Ok(SLVal::Int(x + y)),
      (SLVal::Float(x), SLVal::Float(y)) => Ok(SLVal::Float(x + y)),
      _ => Err(format!("Couldn't add {:?} and {:?}", a, b)),
    }))
    .with_builtin(Builtin::binary("std", "-", |a, b| match (&*a, &*b) {
      // `a` is the left operand, `b` is the right operand: left - right.
      (SLVal::Int(x), SLVal::Int(y)) => Ok(SLVal::Int(x - y)),
      (SLVal::Float(x), SLVal::Float(y)) => Ok(SLVal::Float(x - y)),
      _ => Err(format!("Couldn't sub {:?} and {:?}", a, b)),
    }))
    .with_builtin(Builtin::binary("std", "==", |a, b| Ok(SLVal::Bool(a == b))))
    .with_builtin(Builtin::binary("std", "concat", |a, b| match (&*a, &*b) {
      (SLVal::String(x), SLVal::String(y)) => Ok(SLVal::String(format!("{x}{y}"))),
      (SLVal::List(x), SLVal::List(y)) => {
        let mut combined = x.clone();
        combined.extend(y.iter().cloned());
        Ok(SLVal::List(combined))
      }
      _ => Err(format!("Couldn't concat {:?} and {:?}", a, b)),
    }))
    .with_builtin(Builtin::variadic("std", "list", |args| {
      Ok(SLVal::List(args.to_vec()))
    }))
    .with_builtin(Builtin::unary("std", "len", |a| match &*a {
      SLVal::List(items) => Ok(SLVal::Int(items.len() as i64)),
      _ => Err(format!("len: expected a List, got {:?}", a)),
    }))
    .with_builtin(Builtin::binary("std", "idx", |a, b| match (&*a, &*b) {
      (SLVal::List(items), SLVal::Int(i)) => {
        let len = items.len() as i64;
        let idx = if *i < 0 { *i + len } else { *i };
        if idx < 0 || idx >= len {
          Err(format!(
            "idx: index {} out of range for list of length {}",
            i, len
          ))
        } else {
          Ok((**items.get(idx as usize).unwrap()).clone())
        }
      }
      (SLVal::String(s), SLVal::Int(i)) => {
        let len = s.chars().count() as i64;
        let idx = if *i < 0 { *i + len } else { *i };
        if idx < 0 || idx >= len {
          Err(format!(
            "idx: index {} out of range for string of length {}",
            i, len
          ))
        } else {
          Ok(SLVal::String(
            s.chars().nth(idx as usize).unwrap().to_string(),
          ))
        }
      }
      _ => Err(format!(
        "idx: expected (List, Int) or (String, Int), got ({:?}, {:?})",
        a, b
      )),
    }))
    .with_builtin(Builtin::binary("std", "push", |a, b| match &*a {
      SLVal::List(items) => {
        let mut new = items.clone();
        new.push(b);
        Ok(SLVal::List(new))
      }
      _ => Err(format!("push: expected a List, got {:?}", a)),
    }))
    .with_builtin(Builtin::ternary("std", "slice", |a, b, c| {
      match (&*a, &*b, &*c) {
        (SLVal::List(items), SLVal::Int(start), SLVal::Int(stop)) => {
          let len = items.len() as i64;
          let s = norm_index(*start, len);
          let e = norm_index(*stop, len);
          let s = s.clamp(0, len);
          let e = e.clamp(0, len);
          if s >= e {
            Ok(SLVal::List(vec![]))
          } else {
            Ok(SLVal::List(items[s as usize..e as usize].to_vec()))
          }
        }
        (SLVal::String(s), SLVal::Int(start), SLVal::Int(stop)) => {
          let chars: Vec<char> = s.chars().collect();
          let len = chars.len() as i64;
          let st = norm_index(*start, len).clamp(0, len) as usize;
          let en = norm_index(*stop, len).clamp(0, len) as usize;
          if st >= en {
            Ok(SLVal::String(String::new()))
          } else {
            Ok(SLVal::String(chars[st..en].iter().collect()))
          }
        }
        _ => Err(format!(
          "slice: expected (List, Int, Int) or (String, Int, Int), got ({:?}, {:?}, {:?})",
          a, b, c
        )),
      }
    }))
    // ── rand module ────────────────────────────────────────────────────────
    // (rand.rng seed "name") -> Cell(Int)
    //   Deterministically derives a new 64-bit seed from `seed` (Int) and
    //   `name` (String) using BLAKE3, and wraps it in a Cell so that
    //   `rand.roll!` can mutate it in place. Same inputs always produce the
    //   same Cell contents; differing `name` or `seed` produces differing
    //   output.
    .with_builtin(Builtin::binary_alloc("rand", "rng", |mc, seed, name| {
      let parent = match &*seed {
        SLVal::Int(i) => *i,
        other => return Err(format!("rand.rng: expected Int seed, got {:?}", other)),
      };
      let ns = match &*name {
        SLVal::String(s) => s.as_str(),
        other => return Err(format!("rand.rng: expected String name, got {:?}", other)),
      };
      Ok(SLVal::Cell(Gc::new(
        mc,
        RefLock::new(SLVal::Int(rand_rng(parent, ns))),
      )))
    }))
    // (rand.roll! rng sides) -> Int
    //   Mutates the `rng` in place, advancing it to the next seed,
    //   and returns the roll (in `1..=sides`). The Cell is both the RNG state
    //   and (after the call) the advanced state, so callers don't need to
    //   thread a new seed through.
    .with_builtin(Builtin::binary_alloc("rand", "roll!", |mc, rng, sides| {
      let cell = match &*rng {
        SLVal::Cell(c) => *c,
        other => return Err(format!("rand.roll!: expected Cell rng, got {:?}", other)),
      };
      let n = match &*sides {
        SLVal::Int(i) => *i,
        other => return Err(format!("rand.roll!: expected Int sides, got {:?}", other)),
      };
      if n <= 0 {
        return Err(format!("rand.roll!: sides must be positive, got {}", n));
      }
      let s = match &*cell.borrow() {
        SLVal::Int(i) => *i,
        ref other => {
          return Err(format!(
            "rand.roll!: expected Cell to hold an Int, got {:?}",
            other
          ))
        }
      };
      let (roll, next) = rand_roll(s, n);
      *Gc::write(mc, cell).unlock().borrow_mut() = SLVal::Int(next);
      Ok(SLVal::Int(roll))
    }))
}

/// Derive a deterministic 64-bit seed from a parent seed and a name, using
/// BLAKE3. The 64-bit result is the first 8 bytes of the BLAKE3 XOF output.
pub fn rand_rng(parent_seed: i64, name: &str) -> i64 {
  let mut h = Hasher::new();
  h.update(&parent_seed.to_le_bytes());
  h.update(name.as_bytes());
  let mut out = [0u8; 32];
  h.finalize_xof().fill(&mut out);
  i64::from_le_bytes(out[..8].try_into().unwrap())
}

/// Roll a die with `sides` faces from `seed`. Returns `(roll, new_seed)` where
/// `roll` is in `1..=sides` and `new_seed` is the advanced state, so callers
/// thread it into the next `rand_roll` (or `rand.rng`) call. Pure and
/// deterministic: same inputs always yield the same outputs.
pub fn rand_roll(seed: i64, sides: i64) -> (i64, i64) {
  let mut h = Hasher::new();
  h.update(&seed.to_le_bytes());
  h.update(b"rand.roll");
  let mut seed32 = [0u8; 32];
  h.finalize_xof().fill(&mut seed32);
  let mut rng = ChaCha8Rng::from_seed(seed32);
  let roll = 1 + (rng.next_u64() % sides as u64) as i64;
  let mut next_bytes = [0u8; 8];
  rng.fill_bytes(&mut next_bytes);
  let next_seed = i64::from_le_bytes(next_bytes);
  (roll, next_seed)
}

/// Normalize a possibly-negative index into a non-negative clamped index, using
/// Python's `l[start:stop]` semantics: a negative index counts from the end.
/// The result is *not* clamped to `[0, len]`; the caller does that after both
/// bounds are normalized (so `l[2:100]` works on a length-3 list).
fn norm_index(i: i64, len: i64) -> i64 {
  if i < 0 {
    i + len
  } else {
    i
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::compiler::compile_executable_from_source;
  use crate::interpreter::{Interpreter, SLValue};
  use rstest::rstest;

  /// End-to-end: the surface `(rand.rng seed "name")` returns a `Cell(Int)`
  /// whose contents match [`rand_rng`] directly.
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
    let source = format!("(fn main () (rand.rng {} \"{}\"))", seed, name);
    let pkg =
      compile_executable_from_source(&source, ("main", "main"), &default_builtins().specs())
        .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let result = exec.run_until_done().unwrap();
    match result {
      SLValue::Cell(inner) => {
        assert_eq!(
          *inner,
          SLValue::Int(expected),
          "rand.rng {} {:?}",
          seed,
          name
        )
      }
      other => panic!("expected Cell from rand.rng, got {:?}", other),
    }
  }

  /// End-to-end: `(rand.roll! rng sides)` mutates the Cell<Int> `rng` in place
  /// and returns the roll as an Int. Calling it 10 times against the same cell
  /// reproduces the expected 10-roll chain.
  #[rstest]
  #[case::alpha(0, "alpha", [1, 16, 13, 17, 12, 15, 1, 18, 14, 11])]
  #[case::beta(1, "beta", [17, 12, 8, 7, 12, 6, 18, 7, 8, 3])]
  #[case::battle(42, "battle", [15, 7, 6, 1, 16, 16, 12, 14, 17, 19])]
  #[case::neg(-1, "neg", [17, 20, 15, 19, 10, 11, 1, 5, 11, 17])]
  #[case::weather(100, "weather", [3, 6, 9, 15, 9, 12, 15, 20, 10, 1])]
  #[case::loop_(7, "loop", [3, 3, 1, 4, 4, 12, 19, 19, 10, 17])]
  #[case::doors(256, "doors", [11, 17, 19, 17, 10, 3, 4, 18, 2, 5])]
  #[case::shadow(-99, "shadow", [5, 20, 4, 15, 10, 3, 13, 4, 4, 9])]
  #[case::big(123_456_789, "big", [14, 12, 19, 11, 7, 3, 3, 13, 9, 11])]
  #[case::huge(-8_589_934_592, "huge", [2, 15, 4, 6, 19, 1, 2, 9, 3, 18])]
  fn rand_roll_surface_chain(#[case] seed: i64, #[case] name: &str, #[case] expected: [i64; 10]) {
    let src = format!(
      "(fn rolln (rng n acc)
         (if (std.== n 0)
           acc
           (block
             (let r (rand.roll! rng 20))
             (rolln rng (std.- n 1) (std.push acc r)))))

       (fn main () (rolln (rand.rng {seed} \"{name}\") 10 (std.list)))
       "
    );
    let pkg =
      compile_executable_from_source(&src, ("main", "main"), &default_builtins().specs()).unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let result = exec.run_until_done().unwrap();
    let got: Vec<i64> = match result {
      SLValue::List(items) => items
        .into_iter()
        .map(|v| match v {
          SLValue::Int(i) => i,
          other => panic!("expected Int in roll list, got {:?}", other),
        })
        .collect(),
      other => panic!("expected List from rolln, got {:?}", other),
    };
    assert_eq!(got, expected.to_vec(), "seed={} name={:?}", seed, name);
  }

  /// `rand.roll!` rejects non-positive sides with a runtime error.
  #[test]
  fn rand_roll_rejects_non_positive_sides() {
    let source = "(fn main () (rand.roll! (rand.rng 0 \"x\") 0))";
    let pkg = compile_executable_from_source(source, ("main", "main"), &default_builtins().specs())
      .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let err = exec.run_until_done().unwrap_err();
    assert!(err.contains("sides must be positive"), "got: {}", err);
  }

  /// `rand.roll!` rejects a non-Cell rng.
  #[test]
  fn rand_roll_rejects_non_cell_rng() {
    let source = "(fn main () (rand.roll! \"not-a-cell\" 6))";
    let pkg = compile_executable_from_source(source, ("main", "main"), &default_builtins().specs())
      .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let err = exec.run_until_done().unwrap_err();
    assert!(err.contains("expected Cell rng"), "got: {}", err);
  }

  /// `rand.rng` rejects non-Int seeds.
  #[test]
  fn rand_rng_rejects_non_int_seed() {
    let source = "(fn main () (rand.rng \"x\" \"name\"))";
    let pkg = compile_executable_from_source(source, ("main", "main"), &default_builtins().specs())
      .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let err = exec.run_until_done().unwrap_err();
    assert!(err.contains("expected Int seed"), "got: {}", err);
  }
}
