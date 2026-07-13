use std::sync::Arc;

use blake3::Hasher;
use gc_arena::{Gc, Mutation, RefLock};
use rand_chacha::rand_core::{RngCore, SeedableRng};
use rand_chacha::ChaCha8Rng;

use crate::interpreter::{Accounted, CellContents, HostCtx, MemoryReservation, SLVal, Value};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Trait {
  Add,
  Sub,
  Eq,
  Concat,
  Slice,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeConst {
  Int,
  Float,
  String,
  Bool,
  Void,
  Cell(Box<TypeConst>),
  List(Box<TypeConst>),
  Fn {
    params: Vec<TypeConst>,
    ret: Box<TypeConst>,
  },
  Var(String),
}

impl TypeConst {
  pub fn var(name: impl Into<String>) -> Self {
    Self::Var(name.into())
  }

  pub fn cell(item: TypeConst) -> Self {
    Self::Cell(Box::new(item))
  }

  pub fn list(item: TypeConst) -> Self {
    Self::List(Box::new(item))
  }

  pub fn function(params: Vec<TypeConst>, ret: TypeConst) -> Self {
    Self::Fn {
      params,
      ret: Box::new(ret),
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BuiltinSignature {
  pub type_vars: Vec<(String, Vec<Trait>)>,
  pub params: Vec<TypeConst>,
  pub rest: Option<TypeConst>,
  pub ret: TypeConst,
}

/// A compile-time description of a builtin: which module/name it lives in and
/// how many arguments it takes. `num_params` is `None` for variadic builtins.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BuiltinSpec {
  pub module: &'static str,
  pub name: &'static str,
  pub num_params: Option<u16>,
  pub signature: BuiltinSignature,
}

pub fn sig(
  type_vars: &[(&str, &[Trait])],
  params: Vec<TypeConst>,
  rest: Option<TypeConst>,
  ret: TypeConst,
) -> BuiltinSignature {
  BuiltinSignature {
    type_vars: type_vars
      .iter()
      .map(|(name, bounds)| ((*name).to_string(), bounds.to_vec()))
      .collect(),
    params,
    rest,
    ret,
  }
}

/// A builtin's runtime handler. Takes a [`HostCtx`] (which bundles the
/// `&mut ExecRoot`, the GC `Mutation` context, and the `Package`/`Builtins`
/// registries) and the evaluated arguments.
///
/// The `for<'gc, 'call>` higher-ranked bound lets one `'static` handler serve
/// any execution's arena. `'gc` is the arena brand (invariant, used in `Gc`
/// and `ExecRoot`); `'call` is the short mutable borrow of the root while
/// invoking one builtin (distinct from `'gc` so the borrow checker can
/// reborrow `&'gc mut ExecRoot<'gc>` as `&'call mut ExecRoot<'gc>` without
/// affecting the inner arena brand).
pub(crate) type HostFn = Arc<
  dyn for<'gc, 'call> Fn(
    &mut HostCtx<'gc, 'call>,
    &[Gc<'gc, Accounted<'gc>>],
  ) -> Result<Value<'gc>, String>,
>;

/// A builtin: metadata ([`BuiltinSpec`]) plus its handler ([`HostFn`]).
#[derive(Clone)]
pub struct Builtin {
  spec: BuiltinSpec,
  func: HostFn,
}

impl Builtin {
  pub fn spec(&self) -> &BuiltinSpec {
    &self.spec
  }

  /// Invoke this builtin's handler and push the returned shared value handle.
  pub(crate) fn call<'gc, 'call>(
    &self,
    ctx: &mut HostCtx<'gc, 'call>,
    args: &[Gc<'gc, Accounted<'gc>>],
  ) -> Result<(), String> {
    let result = (self.func)(ctx, args)?;
    ctx.push_gc(result);
    Ok(())
  }

  /// Construct a builtin that receives the full execution context and raw
  /// argument slice. `num_params` is `None` for a variadic builtin.
  ///
  /// Contextual host functions are trusted runtime extensions: SafeLisp
  /// cannot prevent them from allocating untracked memory, blocking, or
  /// otherwise affecting the process. Implementations must reserve
  /// guest-sized Rust-heap allocations through [`HostCtx::reserve_memory`].
  pub fn contextual(
    module: &'static str,
    name: &'static str,
    num_params: Option<u16>,
    signature: BuiltinSignature,
    func: impl for<'gc, 'call> Fn(
        &mut HostCtx<'gc, 'call>,
        &[Gc<'gc, Accounted<'gc>>],
      ) -> Result<SLVal<'gc>, String>
      + 'static,
  ) -> Self {
    Builtin {
      spec: BuiltinSpec {
        module,
        name,
        num_params,
        signature,
      },
      func: Arc::new(move |ctx, args| {
        let value = func(ctx, args)?;
        Ok(ctx.alloc_value(value))
      }),
    }
  }

  /// Construct a contextual builtin that returns an existing or explicitly
  /// allocated in-arena value handle. Use this when an operation should
  /// preserve object identity, such as list indexing.
  pub fn contextual_value(
    module: &'static str,
    name: &'static str,
    num_params: Option<u16>,
    signature: BuiltinSignature,
    func: impl for<'gc, 'call> Fn(
        &mut HostCtx<'gc, 'call>,
        &[Gc<'gc, Accounted<'gc>>],
      ) -> Result<Value<'gc>, String>
      + 'static,
  ) -> Self {
    Builtin {
      spec: BuiltinSpec {
        module,
        name,
        num_params,
        signature,
      },
      func: Arc::new(func),
    }
  }

  /// A unary (one-arg) builtin.
  pub fn unary(
    module: &'static str,
    name: &'static str,
    signature: BuiltinSignature,
    func: impl for<'gc> Fn(Gc<'gc, Accounted<'gc>>) -> Result<SLVal<'gc>, String> + 'static,
  ) -> Self {
    Builtin {
      spec: BuiltinSpec {
        module,
        name,
        num_params: Some(1),
        signature,
      },
      func: Arc::new(move |ctx, args| {
        let value = func(args[0])?;
        Ok(ctx.alloc_value(value))
      }),
    }
  }

  /// A binary (two-arg) builtin. `func` receives `(left, right)`.
  pub fn binary(
    module: &'static str,
    name: &'static str,
    signature: BuiltinSignature,
    func: impl for<'gc> Fn(Gc<'gc, Accounted<'gc>>, Gc<'gc, Accounted<'gc>>) -> Result<SLVal<'gc>, String>
      + 'static,
  ) -> Self {
    Builtin {
      spec: BuiltinSpec {
        module,
        name,
        num_params: Some(2),
        signature,
      },
      func: Arc::new(move |ctx, args| {
        let value = func(args[0], args[1])?;
        Ok(ctx.alloc_value(value))
      }),
    }
  }

  /// A binary builtin that needs the GC `Mutation` context (e.g. to allocate a
  /// fresh `List`). `func` receives `(mc, left, right)`.
  pub fn binary_alloc(
    module: &'static str,
    name: &'static str,
    signature: BuiltinSignature,
    func: impl for<'gc> Fn(
        &Mutation<'gc>,
        Gc<'gc, Accounted<'gc>>,
        Gc<'gc, Accounted<'gc>>,
      ) -> Result<SLVal<'gc>, String>
      + 'static,
  ) -> Self {
    Builtin {
      spec: BuiltinSpec {
        module,
        name,
        num_params: Some(2),
        signature,
      },
      func: Arc::new(move |ctx, args| {
        let value = func(ctx.mc(), args[0], args[1])?;
        Ok(ctx.alloc_value(value))
      }),
    }
  }

  /// A ternary (three-arg) builtin.
  pub fn ternary(
    module: &'static str,
    name: &'static str,
    signature: BuiltinSignature,
    func: impl for<'gc> Fn(
        Gc<'gc, Accounted<'gc>>,
        Gc<'gc, Accounted<'gc>>,
        Gc<'gc, Accounted<'gc>>,
      ) -> Result<SLVal<'gc>, String>
      + 'static,
  ) -> Self {
    Builtin {
      spec: BuiltinSpec {
        module,
        name,
        num_params: Some(3),
        signature,
      },
      func: Arc::new(move |ctx, args| {
        let value = func(args[0], args[1], args[2])?;
        Ok(ctx.alloc_value(value))
      }),
    }
  }

  /// A variadic builtin: it receives the whole argument slice and may be called
  /// with any number of args (including zero). `num_params` is `None`, so the
  /// interpreter uses the call-site arity (carried on `Instruction::Call` /
  /// `Instruction::CallDynamic`) to know how many args to pop.
  pub fn variadic(
    module: &'static str,
    name: &'static str,
    signature: BuiltinSignature,
    func: impl for<'gc> Fn(&[Gc<'gc, Accounted<'gc>>]) -> Result<SLVal<'gc>, String> + 'static,
  ) -> Self {
    Builtin {
      spec: BuiltinSpec {
        module,
        name,
        num_params: None,
        signature,
      },
      func: Arc::new(move |ctx, args| {
        let value = func(args)?;
        Ok(ctx.alloc_value(value))
      }),
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
    self.entries.iter().map(|b| b.spec.clone()).collect()
  }

  /// Look up a builtin by `(module, name)`.
  pub fn lookup(&self, module: &str, name: &str) -> Option<&Builtin> {
    self
      .entries
      .iter()
      .find(|b| b.spec.module == module && b.spec.name == name)
  }
}

fn reserved_vec<T>(
  ctx: &HostCtx<'_, '_>,
  len: usize,
  operation: &str,
) -> Result<(Vec<T>, MemoryReservation), String> {
  let requested_bytes = len
    .checked_mul(std::mem::size_of::<T>())
    .ok_or_else(|| format!("{operation}: allocation size overflow"))?;
  let mut reservation = ctx.reserve_memory(requested_bytes)?;
  let mut values = Vec::new();
  values
    .try_reserve_exact(len)
    .map_err(|_| format!("{operation}: failed to allocate space for {len} values"))?;
  let actual_bytes = values
    .capacity()
    .checked_mul(std::mem::size_of::<T>())
    .ok_or_else(|| format!("{operation}: allocation capacity overflow"))?;
  ctx.reconcile_reservation(&mut reservation, actual_bytes)?;
  Ok((values, reservation))
}

fn reserved_string(
  ctx: &HostCtx<'_, '_>,
  len: usize,
  operation: &str,
) -> Result<(String, MemoryReservation), String> {
  let mut reservation = ctx.reserve_memory(len)?;
  let mut value = String::new();
  value
    .try_reserve_exact(len)
    .map_err(|_| format!("{operation}: failed to allocate {len} string bytes"))?;
  ctx.reconcile_reservation(&mut reservation, value.capacity())?;
  Ok((value, reservation))
}

/// Return the UTF-8 byte offset for a character boundary. `char_index` may
/// equal the character count, in which case this returns `value.len()`.
fn char_boundary(value: &str, char_index: usize) -> usize {
  value
    .char_indices()
    .nth(char_index)
    .map_or(value.len(), |(byte_index, _)| byte_index)
}

/// The default builtin registry.
pub fn default_builtins() -> Builtins {
  Builtins::new()
    .with_builtin(Builtin::binary(
      "std",
      "+",
      sig(
        &[("A", &[Trait::Add])],
        vec![TypeConst::var("A"), TypeConst::var("A")],
        None,
        TypeConst::var("A"),
      ),
      |a, b| match (&a.value, &b.value) {
        (SLVal::Int(x), SLVal::Int(y)) => Ok(SLVal::Int(x + y)),
        (SLVal::Float(x), SLVal::Float(y)) => Ok(SLVal::Float(x + y)),
        _ => Err(format!(
          "Couldn't add {} and {}",
          a.value.type_name(),
          b.value.type_name()
        )),
      },
    ))
    .with_builtin(Builtin::binary(
      "std",
      "-",
      sig(
        &[("A", &[Trait::Sub])],
        vec![TypeConst::var("A"), TypeConst::var("A")],
        None,
        TypeConst::var("A"),
      ),
      |a, b| {
        match (&a.value, &b.value) {
          // `a` is the left operand, `b` is the right operand: left - right.
          (SLVal::Int(x), SLVal::Int(y)) => Ok(SLVal::Int(x - y)),
          (SLVal::Float(x), SLVal::Float(y)) => Ok(SLVal::Float(x - y)),
          _ => Err(format!(
            "Couldn't sub {} and {}",
            a.value.type_name(),
            b.value.type_name()
          )),
        }
      },
    ))
    .with_builtin(Builtin::binary(
      "std",
      "==",
      sig(
        &[("A", &[Trait::Eq])],
        vec![TypeConst::var("A"), TypeConst::var("A")],
        None,
        TypeConst::Bool,
      ),
      |a, b| Ok(SLVal::Bool(a == b)),
    ))
    .with_builtin(Builtin::contextual(
      "std",
      "concat",
      Some(2),
      sig(
        &[("A", &[Trait::Concat])],
        vec![TypeConst::var("A"), TypeConst::var("A")],
        None,
        TypeConst::var("A"),
      ),
      |ctx, args| {
        let (a, b) = (args[0], args[1]);
        match (&a.value, &b.value) {
          (SLVal::String(x), SLVal::String(y)) => {
            let len = x
              .len()
              .checked_add(y.len())
              .ok_or_else(|| "concat: string length overflow".to_string())?;
            let (mut combined, _reservation) = reserved_string(ctx, len, "concat")?;
            combined.push_str(x);
            combined.push_str(y);
            Ok(SLVal::String(combined))
          }
          (SLVal::List(x), SLVal::List(y)) => {
            let len = x
              .len()
              .checked_add(y.len())
              .ok_or_else(|| "concat: list length overflow".to_string())?;
            let (mut combined, _reservation) = reserved_vec(ctx, len, "concat")?;
            combined.extend(x.iter().copied());
            combined.extend(y.iter().copied());
            Ok(SLVal::List(combined))
          }
          _ => Err(format!(
            "Couldn't concat {} and {}",
            a.value.type_name(),
            b.value.type_name()
          )),
        }
      },
    ))
    .with_builtin(Builtin::contextual(
      "std",
      "list",
      None,
      sig(
        &[("A", &[])],
        vec![],
        Some(TypeConst::var("A")),
        TypeConst::list(TypeConst::var("A")),
      ),
      |ctx, args| {
        let (mut items, _reservation) = reserved_vec(ctx, args.len(), "list")?;
        items.extend_from_slice(args);
        Ok(SLVal::List(items))
      },
    ))
    .with_builtin(Builtin::contextual(
      "std",
      "cell",
      Some(1),
      sig(
        &[("A", &[])],
        vec![TypeConst::var("A")],
        None,
        TypeConst::cell(TypeConst::var("A")),
      ),
      |ctx, args| {
        let contents = CellContents::new(args[0]);
        Ok(SLVal::Cell(Gc::new(ctx.mc(), RefLock::new(contents))))
      },
    ))
    .with_builtin(Builtin::contextual_value(
      "std",
      "get",
      Some(1),
      sig(
        &[("A", &[])],
        vec![TypeConst::cell(TypeConst::var("A"))],
        None,
        TypeConst::var("A"),
      ),
      |_ctx, args| match &args[0].value {
        SLVal::Cell(cell) => Ok(cell.borrow().value),
        other => Err(format!(
          "std.get: expected a Cell, got {}",
          other.type_name()
        )),
      },
    ))
    .with_builtin(Builtin::contextual(
      "std",
      "set!",
      Some(2),
      sig(
        &[("A", &[])],
        vec![TypeConst::cell(TypeConst::var("A")), TypeConst::var("A")],
        None,
        TypeConst::Void,
      ),
      |ctx, args| match &args[0].value {
        SLVal::Cell(cell) => {
          Gc::write(ctx.mc(), *cell)
            .unlock()
            .borrow_mut()
            .set(args[1]);
          Ok(SLVal::Void)
        }
        other => Err(format!(
          "std.set!: expected a Cell, got {}",
          other.type_name()
        )),
      },
    ))
    .with_builtin(Builtin::unary(
      "std",
      "len",
      sig(
        &[("A", &[])],
        vec![TypeConst::list(TypeConst::var("A"))],
        None,
        TypeConst::Int,
      ),
      |a| match &a.value {
        SLVal::List(items) => Ok(SLVal::Int(items.len() as i64)),
        _ => Err(format!("len: expected a List, got {}", a.value.type_name())),
      },
    ))
    .with_builtin(Builtin::contextual_value(
      "std",
      "idx",
      Some(2),
      sig(
        &[("A", &[])],
        vec![TypeConst::list(TypeConst::var("A")), TypeConst::Int],
        None,
        TypeConst::var("A"),
      ),
      |_ctx, args| {
        let (a, b) = (args[0], args[1]);
        match (&a.value, &b.value) {
          (SLVal::List(items), SLVal::Int(i)) => {
            let len = items.len() as i64;
            let idx = if *i < 0 { *i + len } else { *i };
            if idx < 0 || idx >= len {
              Err(format!(
                "idx: index {} out of range for list of length {}",
                i, len
              ))
            } else {
              Ok(items[idx as usize])
            }
          }
          _ => Err(format!(
            "idx: expected (List, Int), got ({}, {})",
            a.value.type_name(),
            b.value.type_name()
          )),
        }
      },
    ))
    .with_builtin(Builtin::contextual(
      "std",
      "push",
      Some(2),
      sig(
        &[("A", &[])],
        vec![TypeConst::list(TypeConst::var("A")), TypeConst::var("A")],
        None,
        TypeConst::list(TypeConst::var("A")),
      ),
      |ctx, args| {
        let (a, b) = (args[0], args[1]);
        match &a.value {
          SLVal::List(items) => {
            let len = items
              .len()
              .checked_add(1)
              .ok_or_else(|| "push: list length overflow".to_string())?;
            let (mut new, _reservation) = reserved_vec(ctx, len, "push")?;
            new.extend(items.iter().copied());
            new.push(b);
            Ok(SLVal::List(new))
          }
          _ => Err(format!(
            "push: expected a List, got {}",
            a.value.type_name()
          )),
        }
      },
    ))
    // (std.range start stop) -> List<Int>
    //   Like Python's `list(range(start, stop))`: half-open, `[start, stop)`.
    //   `start >= stop` yields the empty list.
    .with_builtin(Builtin::contextual(
      "std",
      "range",
      Some(2),
      sig(
        &[],
        vec![TypeConst::Int, TypeConst::Int],
        None,
        TypeConst::list(TypeConst::Int),
      ),
      |ctx, args| {
        let (a, b) = (args[0], args[1]);
        match (&a.value, &b.value) {
          (SLVal::Int(start), SLVal::Int(stop)) => {
            if stop <= start {
              Ok(SLVal::List(vec![]))
            } else {
              let len = usize::try_from(i128::from(*stop) - i128::from(*start))
                .map_err(|_| "range: result is too large for this platform".to_string())?;
              let (mut values, _reservation) = reserved_vec(ctx, len, "range")?;
              for i in *start..*stop {
                values.push(ctx.alloc_value(SLVal::Int(i)));
                // A range creates all of its GC boxes inside a single bytecode
                // instruction, so enforce the limit incrementally.
                ctx.check_memory_limit()?;
              }
              Ok(SLVal::List(values))
            }
          }
          _ => Err(format!(
            "range: expected (Int, Int), got ({}, {})",
            a.value.type_name(),
            b.value.type_name()
          )),
        }
      },
    ))
    // (std.map list fn) -> List
    //   Applies `fn` (a callable value: FunctionRef or Partial) to each element
    //   of `list` and collects the results into a new list. Implemented as a
    //   builtin with [`HostCtx`] access so it can synchronously invoke the
    //   callable for each element.
    .with_builtin(Builtin::contextual(
      "std",
      "map",
      Some(2),
      sig(
        &[("A", &[]), ("B", &[])],
        vec![
          TypeConst::list(TypeConst::var("A")),
          TypeConst::function(vec![TypeConst::var("A")], TypeConst::var("B")),
        ],
        None,
        TypeConst::list(TypeConst::var("B")),
      ),
      |ctx, args| {
        let (list, func) = (args[0], args[1]);
        let items = match &list.value {
          SLVal::List(items) => items,
          other => {
            return Err(format!(
              "std.map: expected a List, got {}",
              other.type_name()
            ))
          }
        };
        let (mut results, _reservation) = reserved_vec(ctx, items.len(), "map")?;
        for item in items.iter().copied() {
          let result = ctx.call(func, &[item])?;
          results.push(result);
        }
        Ok(SLVal::List(results))
      },
    ))
    .with_builtin(Builtin::contextual(
      "std",
      "slice",
      Some(3),
      sig(
        &[("A", &[Trait::Slice])],
        vec![TypeConst::var("A"), TypeConst::Int, TypeConst::Int],
        None,
        TypeConst::var("A"),
      ),
      |ctx, args| {
        let (a, b, c) = (args[0], args[1], args[2]);
        match (&a.value, &b.value, &c.value) {
          (SLVal::List(items), SLVal::Int(start), SLVal::Int(stop)) => {
            let len = items.len() as i64;
            let s = norm_index(*start, len);
            let e = norm_index(*stop, len);
            let s = s.clamp(0, len);
            let e = e.clamp(0, len);
            if s >= e {
              Ok(SLVal::List(vec![]))
            } else {
              let slice = &items[s as usize..e as usize];
              let (mut result, _reservation) = reserved_vec(ctx, slice.len(), "slice")?;
              result.extend(slice.iter().copied());
              Ok(SLVal::List(result))
            }
          }
          (SLVal::String(s), SLVal::Int(start), SLVal::Int(stop)) => {
            let len = s.chars().count() as i64;
            let st = norm_index(*start, len).clamp(0, len) as usize;
            let en = norm_index(*stop, len).clamp(0, len) as usize;
            if st >= en {
              Ok(SLVal::String(String::new()))
            } else {
              let start_byte = char_boundary(s, st);
              let end_byte = char_boundary(s, en);
              let source = &s[start_byte..end_byte];
              let (mut result, _reservation) = reserved_string(ctx, source.len(), "slice")?;
              result.push_str(source);
              Ok(SLVal::String(result))
            }
          }
          _ => Err(format!(
            "slice: expected (List, Int, Int) or (String, Int, Int), got ({}, {}, {})",
            a.value.type_name(),
            b.value.type_name(),
            c.value.type_name()
          )),
        }
      },
    ))
    // ── rand module ────────────────────────────────────────────────────────
    // (rand.rng seed "name") -> Cell(Int)
    //   Deterministically derives a new 64-bit seed from `seed` (Int) and
    //   `name` (String) using BLAKE3, and wraps it in a Cell so that
    //   `rand.roll!` can mutate it in place. Same inputs always produce the
    //   same Cell contents; differing `name` or `seed` produces differing
    //   output.
    .with_builtin(Builtin::contextual(
      "rand",
      "rng",
      Some(2),
      sig(
        &[],
        vec![TypeConst::Int, TypeConst::String],
        None,
        TypeConst::cell(TypeConst::Int),
      ),
      |ctx, args| {
        let (seed, name) = (args[0], args[1]);
        let parent = match &seed.value {
          SLVal::Int(i) => *i,
          other => {
            return Err(format!(
              "rand.rng: expected Int seed, got {}",
              other.type_name()
            ))
          }
        };
        let ns = match &name.value {
          SLVal::String(s) => s.as_str(),
          other => {
            return Err(format!(
              "rand.rng: expected String name, got {}",
              other.type_name()
            ))
          }
        };
        let state = ctx.alloc_value(SLVal::Int(rand_rng(parent, ns)));
        let contents = CellContents::new(state);
        Ok(SLVal::Cell(Gc::new(ctx.mc(), RefLock::new(contents))))
      },
    ))
    // (rand.roll! rng sides) -> Int
    //   Mutates the `rng` in place, advancing it to the next seed,
    //   and returns the roll (in `1..=sides`). The Cell is both the RNG state
    //   and (after the call) the advanced state, so callers don't need to
    //   thread a new seed through.
    .with_builtin(Builtin::contextual(
      "rand",
      "roll!",
      Some(2),
      sig(
        &[],
        vec![TypeConst::cell(TypeConst::Int), TypeConst::Int],
        None,
        TypeConst::Int,
      ),
      |ctx, args| {
        let (rng, sides) = (args[0], args[1]);
        let cell = match &rng.value {
          SLVal::Cell(c) => *c,
          other => {
            return Err(format!(
              "rand.roll!: expected Cell rng, got {}",
              other.type_name()
            ))
          }
        };
        let n = match &sides.value {
          SLVal::Int(i) => *i,
          other => {
            return Err(format!(
              "rand.roll!: expected Int sides, got {}",
              other.type_name()
            ))
          }
        };
        if n <= 0 {
          return Err(format!("rand.roll!: sides must be positive, got {}", n));
        }
        let s = match &cell.borrow().value.value {
          SLVal::Int(i) => *i,
          other => {
            return Err(format!(
              "rand.roll!: expected Cell to hold an Int, got {}",
              other.type_name()
            ))
          }
        };
        let (roll, next) = rand_roll(s, n);
        let next = ctx.alloc_value(SLVal::Int(next));
        Gc::write(ctx.mc(), cell).unlock().borrow_mut().set(next);
        Ok(SLVal::Int(roll))
      },
    ))
}

/// Derive a deterministic 64-bit seed from a parent seed and a name, using
/// BLAKE3. The 64-bit result is the first 8 bytes of the BLAKE3 XOF output.
pub fn rand_rng(parent_seed: i64, name: &str) -> i64 {
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
/// thread it into the next `rand_roll` (or `rand.rng`) call. Pure and
/// deterministic: same inputs always yield the same outputs.
pub fn rand_roll(seed: i64, sides: i64) -> (i64, i64) {
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
  use crate::interpreter::{Execution, Interpreter, SLValue};
  use rstest::rstest;

  #[test]
  fn signatures_own_their_type_declarations() {
    let variable_name = String::from("Element");
    let signature = sig(
      &[(variable_name.as_str(), &[Trait::Eq])],
      vec![TypeConst::var(variable_name.clone())],
      None,
      TypeConst::list(TypeConst::var(variable_name.clone())),
    );
    drop(variable_name);

    assert_eq!(signature.type_vars[0].0, "Element");
    assert_eq!(signature.params[0], TypeConst::var("Element"));
  }

  /// Helper: evaluate `main` with [`default_builtins`] and return the result.
  fn eval_builtin_main(source: &str) -> Result<SLValue, String> {
    let pkg =
      compile_executable_from_source(source, ("main", "main"), &default_builtins().specs())?;
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    exec.run_until_done()
  }

  /// Compile `source`, execute every instruction before `main`'s final
  /// expression call, and leave that call ready to step. This lets tests build
  /// large operands without a limit, then install a limit immediately before
  /// the builtin's temporary allocation.
  fn before_final_call(source: &str) -> Execution {
    let pkg = compile_executable_from_source(source, ("main", "main"), &default_builtins().specs())
      .unwrap();
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

  fn assert_final_builtin_reservation_fails(
    source: &str,
    arity: usize,
    result_external_bytes: usize,
  ) {
    assert!(result_external_bytes > 0);
    let mut exec = before_final_call(source);
    let scratch_bytes = arity * std::mem::size_of::<Gc<'static, Accounted<'static>>>();
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
      "builtin allocated GC results before rejecting its reservation: {source}"
    );
  }

  #[test]
  fn allocating_builtins_reserve_before_building_results() {
    let large = "x".repeat(512);
    let ptr = std::mem::size_of::<Gc<'static, Accounted<'static>>>();
    let cases = [
      (
        format!("(fn main () (std.concat \"{large}\" \"{large}\"))"),
        2,
        1024,
      ),
      ("(fn main () (std.list 1 2 3 4))".to_string(), 4, 4 * ptr),
      (
        "(fn main () (std.push (std.list 1 2 3) 4))".to_string(),
        2,
        4 * ptr,
      ),
      (
        "(fn main () (std.range 0 10000))".to_string(),
        2,
        10_000 * ptr,
      ),
      (
        "(fn main () (fn id (x:Int) ->Int x) (std.map (std.list 1 2 3) id))".to_string(),
        2,
        3 * ptr,
      ),
      (
        "(fn main () (std.slice (std.list 1 2 3 4) 1 3))".to_string(),
        3,
        2 * ptr,
      ),
      (
        format!("(fn main () (std.slice \"{large}\" 0 512))"),
        3,
        large.len(),
      ),
    ];

    for (source, arity, bytes) in cases {
      assert_final_builtin_reservation_fails(&source, arity, bytes);
    }
  }

  #[test]
  fn list_idx_returns_the_existing_value_without_cloning() {
    let large = "x".repeat(64 * 1024);
    let source = format!("(fn main () (std.idx (std.list \"{large}\") 0))");
    let mut exec = before_final_call(&source);
    let scratch_bytes = 2 * std::mem::size_of::<Gc<'static, Accounted<'static>>>();
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
      "(fn main () (std.list {}))",
      (0..128)
        .map(|i| i.to_string())
        .collect::<Vec<_>>()
        .join(" ")
    );
    let mut exec = before_final_call(&source);
    let scratch_bytes = 128 * std::mem::size_of::<Gc<'static, Accounted<'static>>>();
    exec.set_memory_limit(Some(exec.memory_usage() + scratch_bytes - 1));
    let gc_before = exec.gc_count();

    let err = exec.step().unwrap_err();
    assert!(err.contains("memory limit exceeded"), "unexpected: {err}");
    assert_eq!(exec.gc_count(), gc_before);
  }

  #[test]
  fn range_checks_gc_growth_during_the_builtin_call() {
    let mut exec = before_final_call("(fn main () (std.range 0 10000))");
    let ptr = std::mem::size_of::<Gc<'static, Accounted<'static>>>();
    let scratch_bytes = 2 * ptr;
    let result_vec_bytes = 10_000 * ptr;
    exec.set_memory_limit(Some(
      exec.memory_usage() + scratch_bytes + result_vec_bytes + 1024,
    ));
    let gc_before = exec.gc_count();

    let err = exec.step().unwrap_err();
    assert!(err.contains("memory limit exceeded"), "unexpected: {err}");
    let allocated = exec.gc_count() - gc_before;
    assert!(
      allocated > 0 && allocated < 10_000,
      "range should stop during construction, allocated {allocated} values"
    );
  }

  #[test]
  fn string_slicing_uses_character_indices() {
    assert_eq!(
      eval_builtin_main("(fn main () (std.slice \"aé🦀z\" 1 3))").unwrap(),
      SLValue::String("é🦀".to_string())
    );
    assert_eq!(
      eval_builtin_main("(fn main () (std.slice \"aé🦀z\" -3 -1))").unwrap(),
      SLValue::String("é🦀".to_string())
    );
    assert_eq!(
      eval_builtin_main("(fn main () (std.slice \"aé🦀z\" -2 -1))").unwrap(),
      SLValue::String("🦀".to_string())
    );
  }

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
    let result = eval_builtin_main(&source).unwrap();
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
    // (std.map (std.range 0 10) (fn roll (_idx:Int) ->Int (rand.roll! rng 20)))
    //
    // `std.map` applies `roll` to each element of `(std.range 0 10)` and
    // collects the rolls. `roll` ignores its argument (the index) and mutates
    // the shared `rng` cell via `rand.roll!`. The closure transform wraps
    // captured `rng` in a cell, so `rand.roll!` can mutate it.
    let src = format!(
      "(fn main ()\n\
        (let rng (rand.rng {seed} \"{name}\"))\n\
        (fn roll (_idx:Int) ->Int (rand.roll! rng 20))\n\
        (std.map (std.range 0 10) roll))"
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
      other => panic!("expected List from std.map, got {:?}", other),
    };
    assert_eq!(got, expected.to_vec(), "seed={} name={:?}", seed, name);
  }

  /// `std.range` produces a half-open list of ints, like Python's `range`.
  #[test]
  fn range_basic() {
    assert_eq!(
      eval_builtin_main("(fn main () (std.range 0 5))").unwrap(),
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
      eval_builtin_main("(fn main () (std.range 3 3))").unwrap(),
      SLValue::List(vec![])
    );
    assert_eq!(
      eval_builtin_main("(fn main () (std.range 5 2))").unwrap(),
      SLValue::List(vec![])
    );
  }

  #[test]
  fn range_negative_start() {
    assert_eq!(
      eval_builtin_main("(fn main () (std.range -2 2))").unwrap(),
      SLValue::List(vec![
        SLValue::Int(-2),
        SLValue::Int(-1),
        SLValue::Int(0),
        SLValue::Int(1)
      ])
    );
  }

  /// `std.map` applies a function to each element of a list.
  #[test]
  fn map_doubles() {
    assert_eq!(
      eval_builtin_main(
        "(fn main ()
           (fn dbl (x:Int) ->Int (std.+ x x))
           (std.map (std.list 1 2 3) dbl))"
      )
      .unwrap(),
      SLValue::List(vec![SLValue::Int(2), SLValue::Int(4), SLValue::Int(6)])
    );
  }

  #[test]
  fn map_empty_list() {
    assert_eq!(
      eval_builtin_main(
        "(fn main ()
           (fn id (x:Int) ->Int x)
           (std.map (std.list) id))"
      )
      .unwrap(),
      SLValue::List(vec![])
    );
  }

  #[test]
  fn map_with_local_closure() {
    assert_eq!(
      eval_builtin_main(
        "(fn main ()
           (fn inc (x:Int) ->Int (std.+ x 1))
           (std.map (std.range 0 5) inc))"
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
  fn map_non_list_errors() {
    let err = eval_builtin_main(
      "(fn main ()
         (fn id (x:Int) ->Int x)
         (std.map 5 id))",
    )
    .unwrap_err();
    assert!(err.contains("expected `(List"), "got: {}", err);
  }

  /// `rand.roll!` rejects non-positive sides with a runtime error.
  #[test]
  fn rand_roll_rejects_non_positive_sides() {
    let err = eval_builtin_main("(fn main () (rand.roll! (rand.rng 0 \"x\") 0))").unwrap_err();
    assert!(err.contains("sides must be positive"), "got: {}", err);
  }

  /// `rand.roll!` rejects a non-Cell rng.
  #[test]
  fn rand_roll_rejects_non_cell_rng() {
    let err = eval_builtin_main("(fn main () (rand.roll! \"not-a-cell\" 6))").unwrap_err();
    assert!(err.contains("expected `(Cell Int)`"), "got: {}", err);
  }

  /// `rand.rng` rejects non-Int seeds.
  #[test]
  fn rand_rng_rejects_non_int_seed() {
    let err = eval_builtin_main("(fn main () (rand.rng \"x\" \"name\"))").unwrap_err();
    assert!(err.contains("expected `Int`"), "got: {}", err);
  }
}
