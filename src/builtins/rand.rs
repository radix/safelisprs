//! Deterministic random-number builtins and `rand::Rng` value construction.

use blake3::Hasher;
use gc_arena::{Gc, RefLock};
use rand_chacha::ChaCha8Rng;
use rand_core::{RngCore, SeedableRng};

use crate::interpreter::{CellContents, HostCtx, Value};
use crate::types::Signature;

use super::{sig, Builtin, CustomTypeSpec, Library};

/// Construct the random-number builtin library.
pub fn library() -> Library {
  Library::new()
    .with_type(CustomTypeSpec::struct_(
      "rand",
      "Rng",
      vec![("state", Signature::cell(Signature::Int))],
    ))
    // (rand::rng seed "name") -> Rng
    //   Deterministically derives a new 64-bit seed from `seed` (Int) and
    //   `name` (String) using BLAKE3, and wraps it in a Cell so that
    //   `rand::roll!` can mutate it in place. Same inputs always produce the
    //   same Cell contents; differing `name` or `seed` produces differing
    //   output.
    .with_builtin(Builtin::contextual_value(
      "rand",
      "rng",
      Some(2),
      sig(
        &[],
        vec![Signature::Int, Signature::String],
        None,
        Signature::named("rand", "Rng"),
      ),
      rng,
    ))
    // (rand::roll! rng sides) -> Int
    //   Mutates the `rng` in place, advancing it to the next seed,
    //   and returns the roll (in `1..=sides`). The Cell is both the RNG state
    //   and (after the call) the advanced state, so callers don't need to
    //   thread a new seed through.
    .with_builtin(Builtin::contextual_value(
      "rand",
      "roll!",
      Some(2),
      sig(
        &[],
        vec![Signature::named("rand", "Rng"), Signature::Int],
        None,
        Signature::Int,
      ),
      roll,
    ))
    // (rand::choice! rng list) -> element
    //   Advances `rng` and returns an item selected from `list`.
    //   Choosing from an empty list is an error and does not advance the RNG.
    .with_builtin(Builtin::contextual_value(
      "rand",
      "choice!",
      Some(2),
      sig(
        &[("A", &[])],
        vec![
          Signature::named("rand", "Rng"),
          Signature::list(Signature::var("A")),
        ],
        None,
        Signature::var("A"),
      ),
      choice,
    ))
}

fn choice<'gc, 'call>(
  ctx: &mut HostCtx<'gc, 'call>,
  args: &[Value<'gc>],
) -> Result<Value<'gc>, String> {
  let a = ctx.args("rand::choice!", args);
  let rng = a.value(0)?;
  let items = a.list(1)?;
  let cell = rng_state_cell(ctx, rng, "rand::choice!")?;
  if items.is_empty() {
    return Err("rand::choice!: cannot choose from an empty list".to_string());
  }
  let sides = i64::try_from(items.len())
    .map_err(|_| "rand::choice!: list is too long to choose from".to_string())?;
  let seed = rng_seed(cell, "rand::choice!")?;
  let (roll, next) = rand_roll(seed, sides);
  Gc::write(ctx.mc(), cell)
    .unlock()
    .borrow_mut()
    .set(Value::Int(next));
  items
    .get((roll - 1) as usize)
    .ok_or_else(|| "rand::choice!: generated index is out of range".to_string())
}

/// Construct a `rand::Rng` value initialized directly with `seed`.
///
/// This allows host builtins to supply RNGs without depending on the internal
/// `rand::Rng` representation. The context must belong to an execution whose
/// library includes [`library`].
pub fn alloc_rng<'gc, 'call>(
  ctx: &mut HostCtx<'gc, 'call>,
  seed: i64,
) -> Result<Value<'gc>, String> {
  let field_bytes = ::std::mem::size_of::<Value<'gc>>();
  let mut reservation = ctx.reserve_memory(field_bytes)?;
  let mut fields = Vec::new();
  fields
    .try_reserve_exact(1)
    .map_err(|_| "rand::Rng: failed to allocate state field".to_string())?;
  let actual_bytes = fields
    .capacity()
    .checked_mul(field_bytes)
    .ok_or_else(|| "rand::Rng: allocation capacity overflow".to_string())?;
  ctx.reconcile_reservation(&mut reservation, actual_bytes)?;

  let contents = CellContents::new(Value::Int(seed));
  let cell = Value::Cell(Gc::new(ctx.mc(), RefLock::new(contents)));
  fields.push(cell);
  ctx.alloc_struct("rand", "Rng", fields)
}

fn rng<'gc, 'call>(
  ctx: &mut HostCtx<'gc, 'call>,
  args: &[Value<'gc>],
) -> Result<Value<'gc>, String> {
  let a = ctx.args("rand::rng", args);
  let parent = a.int(0)?;
  let namespace = a.string(1)?;
  alloc_rng(ctx, rand_rng(parent, namespace))
}

fn roll<'gc, 'call>(
  ctx: &mut HostCtx<'gc, 'call>,
  args: &[Value<'gc>],
) -> Result<Value<'gc>, String> {
  let a = ctx.args("rand::roll!", args);
  let rng = a.value(0)?;
  let sides = a.int(1)?;
  let cell = rng_state_cell(ctx, rng, "rand::roll!")?;
  if sides <= 0 {
    return Err(format!(
      "rand::roll!: sides must be positive, got {}",
      sides
    ));
  }
  let seed = rng_seed(cell, "rand::roll!")?;
  let (roll, next) = rand_roll(seed, sides);
  Gc::write(ctx.mc(), cell)
    .unlock()
    .borrow_mut()
    .set(Value::Int(next));
  Ok(Value::Int(roll))
}

fn rng_state_cell<'gc, 'call>(
  ctx: &HostCtx<'gc, 'call>,
  rng: Value<'gc>,
  operation: &str,
) -> Result<Gc<'gc, RefLock<CellContents<'gc>>>, String> {
  let instance = ctx
    .struct_instance(&rng, "rand", "Rng")
    .map_err(|error| format!("{operation}: {error}"))?;
  match instance.fields.as_slice() {
    [state] => state.as_cell().map_err(|_| {
      format!(
        "{operation}: expected Rng state to be a Cell, got {}",
        state.type_name()
      )
    }),
    _ => Err(format!(
      "{operation}: expected Rng to have 1 field, got {}",
      instance.fields.len()
    )),
  }
}

fn rng_seed<'gc>(
  cell: Gc<'gc, RefLock<CellContents<'gc>>>,
  operation: &str,
) -> Result<i64, String> {
  let value = cell.borrow().value;
  value.as_int().map_err(|_| {
    format!(
      "{operation}: expected Cell to hold an Int, got {}",
      value.type_name()
    )
  })
}

/// Derive a deterministic 64-bit seed from a parent seed and a name, using
/// BLAKE3. The 64-bit result is the first 8 bytes of the BLAKE3 XOF output.
pub(super) fn rand_rng(parent_seed: i64, name: &str) -> i64 {
  // NEVER CHANGE THIS CODE
  let mut h = Hasher::new();
  h.update(&parent_seed.to_le_bytes());
  h.update(name.as_bytes());
  let mut out = [0u8; 8];
  out.copy_from_slice(&h.finalize().as_bytes()[..8]);
  i64::from_le_bytes(out)
}

/// Roll a die with `sides` faces from `seed`. Returns `(roll, new_seed)` where
/// `roll` is in `1..=sides` and `new_seed` is the advanced state, so callers
/// thread it into the next `rand_roll` (or `rand::rng`) call. Pure and
/// deterministic: same inputs always yield the same outputs.
pub(super) fn rand_roll(seed: i64, sides: i64) -> (i64, i64) {
  // NEVER CHANGE THIS CODE
  let mut chachaseed = [0u8; 32];
  chachaseed[..8].copy_from_slice(&seed.to_le_bytes());
  let mut rng = ChaCha8Rng::from_seed(chachaseed);
  // I don't give a DANG about no modulo bias with a u64.
  let roll = 1 + (rng.next_u64() % sides as u64) as i64;
  let mut next_bytes = [0u8; 8];
  rng.fill_bytes(&mut next_bytes);
  let next_seed = i64::from_le_bytes(next_bytes);
  (roll, next_seed)
}
