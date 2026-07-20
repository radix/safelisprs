use std::sync::Arc;

use blake3::Hasher;
use gc_arena::{Gc, RefLock};
use rand_chacha::rand_core::{RngCore, SeedableRng};
use rand_chacha::ChaCha8Rng;

use crate::interpreter::{CellContents, HostCtx, HostPoll, MemoryReservation, SLVal, Value};

/// A type-class-style bound that a generic builtin type variable can require.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Trait {
  /// Values support addition through the `+` builtin.
  Add,
  /// Values support subtraction through the `-` builtin.
  Sub,
  /// Values support equality comparison.
  Eq,
  /// Values support concatenation.
  Concat,
  /// Values support indexed slicing.
  Slice,
}

/// A compile-time SafeLisp type used in builtin signatures.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeConst {
  /// The integer type.
  Int,
  /// The floating-point type.
  Float,
  /// The string type.
  String,
  /// The boolean type.
  Bool,
  /// The void type.
  Void,
  /// A mutable cell containing a value of the given type.
  Cell(Box<TypeConst>),
  /// A list containing values of the given type.
  List(Box<TypeConst>),
  /// A callable type with fixed parameters and a return type.
  Fn {
    /// Parameter types accepted by the callable.
    params: Vec<TypeConst>,
    /// Return type produced by the callable.
    ret: Box<TypeConst>,
  },
  /// A generic type variable by name.
  Var(String),
}

impl TypeConst {
  /// Construct a type variable with the given name.
  pub fn var(name: impl Into<String>) -> Self {
    Self::Var(name.into())
  }

  /// Construct a cell type containing `item`.
  pub fn cell(item: TypeConst) -> Self {
    Self::Cell(Box::new(item))
  }

  /// Construct a list type containing `item`.
  pub fn list(item: TypeConst) -> Self {
    Self::List(Box::new(item))
  }

  /// Construct a function type from parameter and return types.
  pub fn function(params: Vec<TypeConst>, ret: TypeConst) -> Self {
    Self::Fn {
      params,
      ret: Box::new(ret),
    }
  }
}

/// The type signature of a builtin function.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BuiltinSignature {
  /// Generic type variables and their required trait bounds.
  pub type_vars: Vec<(String, Vec<Trait>)>,
  /// Fixed positional parameter types.
  pub params: Vec<TypeConst>,
  /// Variadic rest parameter type, if the builtin accepts extra arguments.
  pub rest: Option<TypeConst>,
  /// Return type of the builtin.
  pub ret: TypeConst,
}

/// A compile-time description of a builtin: which module/name it lives in and
/// how many arguments it takes. `num_params` is `None` for variadic builtins.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BuiltinSpec {
  /// Module name containing the builtin.
  pub module: &'static str,
  /// Function name exported by the builtin.
  pub name: &'static str,
  /// Fixed argument count, or `None` for a variadic builtin.
  pub num_params: Option<u16>,
  /// Compile-time type signature for the builtin.
  pub signature: BuiltinSignature,
}

/// Construct a [`BuiltinSignature`] from borrowed type-variable metadata.
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
type HostFn = Arc<
  dyn for<'gc, 'call> Fn(&mut HostCtx<'gc, 'call>, &[Value<'gc>]) -> Result<Value<'gc>, String>,
>;

type HostStartFn =
  Arc<dyn for<'gc, 'call> Fn(&mut HostCtx<'gc, 'call>, &[Value<'gc>]) -> Result<(), String>>;

type HostResumeFn = Arc<
  dyn for<'gc, 'call> Fn(
    &mut HostCtx<'gc, 'call>,
    Option<Value<'gc>>,
  ) -> Result<HostPoll<'gc>, String>,
>;

#[derive(Clone)]
enum BuiltinImpl {
  Sync(HostFn),
  Resumable {
    start: HostStartFn,
    resume: HostResumeFn,
  },
}

/// A builtin: metadata ([`BuiltinSpec`]) plus its host handler.
#[derive(Clone)]
pub struct Builtin {
  spec: BuiltinSpec,
  func: BuiltinImpl,
}

impl Builtin {
  /// Return this builtin's compile-time metadata.
  pub fn spec(&self) -> &BuiltinSpec {
    &self.spec
  }

  /// Invoke this builtin's handler and push the returned shared value handle.
  pub(crate) fn call<'gc, 'call>(
    &self,
    ctx: &mut HostCtx<'gc, 'call>,
    builtin_id: (u32, u32),
    args: &[Value<'gc>],
  ) -> Result<(), String> {
    match &self.func {
      BuiltinImpl::Sync(func) => {
        let result = func(ctx, args)?;
        ctx.push(result);
        Ok(())
      }
      BuiltinImpl::Resumable { start, .. } => {
        let stack_base = ctx.stack_len();
        start(ctx, args)?;
        ctx.push_host_frame_at(builtin_id, stack_base);
        Ok(())
      }
    }
  }

  pub(crate) fn resume<'gc, 'call>(
    &self,
    ctx: &mut HostCtx<'gc, 'call>,
    pending_result: Option<Value<'gc>>,
  ) -> Result<HostPoll<'gc>, String> {
    match &self.func {
      BuiltinImpl::Sync(_) => Err(format!(
        "{}::{} is not a resumable builtin",
        self.spec.module, self.spec.name
      )),
      BuiltinImpl::Resumable { resume, .. } => resume(ctx, pending_result),
    }
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
    func: impl for<'gc, 'call> Fn(&mut HostCtx<'gc, 'call>, &[Value<'gc>]) -> Result<SLVal<'gc>, String>
      + 'static,
  ) -> Self {
    Builtin {
      spec: BuiltinSpec {
        module,
        name,
        num_params,
        signature,
      },
      func: BuiltinImpl::Sync(Arc::new(move |ctx, args| {
        let value = func(ctx, args)?;
        Ok(ctx.alloc_heap(value))
      })),
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
    func: impl for<'gc, 'call> Fn(&mut HostCtx<'gc, 'call>, &[Value<'gc>]) -> Result<Value<'gc>, String>
      + 'static,
  ) -> Self {
    Builtin {
      spec: BuiltinSpec {
        module,
        name,
        num_params,
        signature,
      },
      func: BuiltinImpl::Sync(Arc::new(func)),
    }
  }

  /// Construct a builtin whose callback into SafeLisp can pause and resume
  /// through the ordinary interpreter loop. The start function stores durable
  /// state on the VM stack; the resume function advances that state by one
  /// host scheduling step.
  pub fn resumable(
    module: &'static str,
    name: &'static str,
    num_params: Option<u16>,
    signature: BuiltinSignature,
    start: impl for<'gc, 'call> Fn(&mut HostCtx<'gc, 'call>, &[Value<'gc>]) -> Result<(), String>
      + 'static,
    resume: impl for<'gc, 'call> Fn(
        &mut HostCtx<'gc, 'call>,
        Option<Value<'gc>>,
      ) -> Result<HostPoll<'gc>, String>
      + 'static,
  ) -> Self {
    Builtin {
      spec: BuiltinSpec {
        module,
        name,
        num_params,
        signature,
      },
      func: BuiltinImpl::Resumable {
        start: Arc::new(start),
        resume: Arc::new(resume),
      },
    }
  }

  /// A unary (one-arg) builtin.
  pub fn unary(
    module: &'static str,
    name: &'static str,
    signature: BuiltinSignature,
    func: impl for<'gc> Fn(Value<'gc>) -> Result<Value<'gc>, String> + 'static,
  ) -> Self {
    Builtin {
      spec: BuiltinSpec {
        module,
        name,
        num_params: Some(1),
        signature,
      },
      func: BuiltinImpl::Sync(Arc::new(move |_ctx, args| func(args[0]))),
    }
  }

  /// A binary (two-arg) builtin. `func` receives `(left, right)`.
  pub fn binary(
    module: &'static str,
    name: &'static str,
    signature: BuiltinSignature,
    func: impl for<'gc> Fn(Value<'gc>, Value<'gc>) -> Result<Value<'gc>, String> + 'static,
  ) -> Self {
    Builtin {
      spec: BuiltinSpec {
        module,
        name,
        num_params: Some(2),
        signature,
      },
      func: BuiltinImpl::Sync(Arc::new(move |_ctx, args| func(args[0], args[1]))),
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
    func: impl for<'gc> Fn(&[Value<'gc>]) -> Result<Value<'gc>, String> + 'static,
  ) -> Self {
    Builtin {
      spec: BuiltinSpec {
        module,
        name,
        num_params: None,
        signature,
      },
      func: BuiltinImpl::Sync(Arc::new(move |_ctx, args| func(args))),
    }
  }
}

/// A registry of builtins available to a program. The compiler reads the
/// [`BuiltinSpec`]s (via [`Builtins::specs`]) to register `Callable::Builtin`
/// slots; the interpreter looks up the matching handler at runtime.
#[derive(Clone, Default)]
pub struct Builtins {
  entries: Vec<Builtin>,
}

impl Builtins {
  /// Create an empty builtin registry.
  pub fn new() -> Self {
    Self::default()
  }

  /// Register a builtin (builder style).
  pub fn with_builtin(mut self, builtin: Builtin) -> Self {
    self.entries.push(builtin);
    self
  }

  /// Iterate over all registered builtins.
  pub fn iter(&self) -> impl Iterator<Item = &Builtin> {
    self.entries.iter()
  }

  /// The specs of all registered builtins, for the compiler to inject as
  /// `Callable::Builtin` slots.
  pub fn specs(&self) -> Vec<BuiltinSpec> {
    self.entries.iter().map(|b| b.spec.clone()).collect()
  }

  /// Look up a builtin by `(module, name)`.
  pub(crate) fn lookup(&self, module: &str, name: &str) -> Option<&Builtin> {
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

fn map_start<'gc, 'call>(ctx: &mut HostCtx<'gc, 'call>, args: &[Value<'gc>]) -> Result<(), String> {
  let (list, func) = (args[0], args[1]);
  match &list {
    Value::Heap(heap) if matches!(&heap.value, SLVal::List(_)) => {}
    _ => {
      return Err(format!(
        "std::map: expected a List, got {}",
        list.type_name()
      ))
    }
  }
  ctx.push(list);
  ctx.push(func);
  ctx.push(Value::Int(0)); // index we're currently working on
  ctx.push(Value::Int(0)); // number of results we've accumulated
  Ok(())
}

fn pop_map_state<'gc, 'call>(
  ctx: &mut HostCtx<'gc, 'call>,
) -> Result<(Value<'gc>, Value<'gc>, usize, usize), String> {
  let result_count = match ctx.pop()? {
    Value::Int(value) if value >= 0 => {
      usize::try_from(value).map_err(|_| "std::map: result count does not fit usize".to_string())?
    }
    other => {
      return Err(format!(
        "std::map: expected result count Int, got {}",
        other.type_name()
      ))
    }
  };
  let index = match ctx.pop()? {
    Value::Int(value) if value >= 0 => {
      usize::try_from(value).map_err(|_| "std::map: index does not fit usize".to_string())?
    }
    other => {
      return Err(format!(
        "std::map: expected index Int, got {}",
        other.type_name()
      ))
    }
  };
  let func = ctx.pop()?;
  let source_list = ctx.pop()?;
  Ok((source_list, func, index, result_count))
}

fn push_map_state<'gc, 'call>(
  ctx: &mut HostCtx<'gc, 'call>,
  source_list: Value<'gc>,
  func: Value<'gc>,
  index: usize,
  result_count: usize,
) -> Result<(), String> {
  let index = i64::try_from(index).map_err(|_| "std::map: index overflow".to_string())?;
  let result_count =
    i64::try_from(result_count).map_err(|_| "std::map: result count overflow".to_string())?;
  ctx.push(source_list);
  ctx.push(func);
  ctx.push(Value::Int(index));
  ctx.push(Value::Int(result_count));
  Ok(())
}

fn map_resume<'gc, 'call>(
  ctx: &mut HostCtx<'gc, 'call>,
  pending_result: Option<Value<'gc>>,
) -> Result<HostPoll<'gc>, String> {
  if let Some(callback_result) = pending_result {
    let (source_list, func, index, result_count) = pop_map_state(ctx)?;
    let next_index = index + 1;
    let next_result_count = result_count + 1;
    ctx.push(callback_result);
    push_map_state(ctx, source_list, func, next_index, next_result_count)?;
    return Ok(HostPoll::Pending);
  }

  let (source_list, func, index, result_count) = pop_map_state(ctx)?;
  let (len, item) = match &source_list {
    Value::Heap(heap) => match &heap.value {
      SLVal::List(items) => {
        let item = items.get(index).copied();
        (items.len(), item)
      }
      _ => {
        return Err(format!(
          "std::map: expected a List, got {}",
          source_list.type_name()
        ))
      }
    },
    _ => {
      return Err(format!(
        "std::map: expected a List, got {}",
        source_list.type_name()
      ))
    }
  };

  if index == len {
    let (mut results, _reservation) = reserved_vec(ctx, result_count, "map")?;
    for _ in 0..result_count {
      results.push(ctx.pop()?);
    }
    results.reverse();
    let result = ctx.alloc_heap(SLVal::List(results));
    Ok(HostPoll::Ready(result))
  } else {
    let item = item
      .ok_or_else(|| format!("std::map: index {index} out of range for list of length {len}"))?;
    push_map_state(ctx, source_list, func, index, result_count)?;
    ctx.call(func, &[item])?;
    Ok(HostPoll::Pending)
  }
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
      |a, b| match (a, b) {
        (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x + y)),
        (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x + y)),
        _ => Err(format!(
          "Couldn't add {} and {}",
          a.type_name(),
          b.type_name()
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
        match (a, b) {
          // `a` is the left operand, `b` is the right operand: left - right.
          (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x - y)),
          (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x - y)),
          _ => Err(format!(
            "Couldn't sub {} and {}",
            a.type_name(),
            b.type_name()
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
      |a, b| Ok(Value::Bool(a == b)),
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
        match (&a, &b) {
          (Value::Heap(a), Value::Heap(b)) => match (&a.value, &b.value) {
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
          },
          _ => Err(format!(
            "Couldn't concat {} and {}",
            a.type_name(),
            b.type_name()
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
    .with_builtin(Builtin::contextual_value(
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
        Ok(Value::Cell(Gc::new(ctx.mc(), RefLock::new(contents))))
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
      |_ctx, args| match args[0] {
        Value::Cell(cell) => Ok(cell.borrow().value),
        other => Err(format!(
          "std::get: expected a Cell, got {}",
          other.type_name()
        )),
      },
    ))
    .with_builtin(Builtin::contextual_value(
      "std",
      "set!",
      Some(2),
      sig(
        &[("A", &[])],
        vec![TypeConst::cell(TypeConst::var("A")), TypeConst::var("A")],
        None,
        TypeConst::Void,
      ),
      |ctx, args| match args[0] {
        Value::Cell(cell) => {
          Gc::write(ctx.mc(), cell).unlock().borrow_mut().set(args[1]);
          Ok(Value::Void)
        }
        other => Err(format!(
          "std::set!: expected a Cell, got {}",
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
      |a| match a {
        Value::Heap(heap) => match &heap.value {
          SLVal::List(items) => Ok(Value::Int(items.len() as i64)),
          _ => Err(format!("len: expected a List, got {}", a.type_name())),
        },
        _ => Err(format!("len: expected a List, got {}", a.type_name())),
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
        match (&a, &b) {
          (Value::Heap(a), Value::Int(i)) => match &a.value {
            SLVal::List(items) => {
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
              b.type_name()
            )),
          },
          _ => Err(format!(
            "idx: expected (List, Int), got ({}, {})",
            a.type_name(),
            b.type_name()
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
        match &a {
          Value::Heap(heap) => match &heap.value {
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
            _ => Err(format!("push: expected a List, got {}", a.type_name())),
          },
          _ => Err(format!("push: expected a List, got {}", a.type_name())),
        }
      },
    ))
    // (std::range start stop) -> List<Int>
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
        match (a, b) {
          (Value::Int(start), Value::Int(stop)) => {
            if stop <= start {
              Ok(SLVal::List(vec![]))
            } else {
              let len = usize::try_from(i128::from(stop) - i128::from(start))
                .map_err(|_| "range: result is too large for this platform".to_string())?;
              let (mut values, _reservation) = reserved_vec(ctx, len, "range")?;
              for i in start..stop {
                values.push(Value::Int(i));
              }
              Ok(SLVal::List(values))
            }
          }
          _ => Err(format!(
            "range: expected (Int, Int), got ({}, {})",
            a.type_name(),
            b.type_name()
          )),
        }
      },
    ))
    // (std::map list fn) -> List
    //   Applies `fn` (a callable value: FunctionRef or Partial) to each element
    //   of `list` and collects the results into a new list. Implemented as a
    //   builtin with [`HostCtx`] access so callback execution is scheduled
    //   through the ordinary resumable interpreter loop.
    .with_builtin(Builtin::resumable(
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
      map_start,
      map_resume,
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
        match (&a, &b, &c) {
          (Value::Heap(heap), Value::Int(start), Value::Int(stop)) => match &heap.value {
            SLVal::List(items) => {
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
            SLVal::String(s) => {
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
              a.type_name(),
              b.type_name(),
              c.type_name()
            )),
          },
          _ => Err(format!(
            "slice: expected (List, Int, Int) or (String, Int, Int), got ({}, {}, {})",
            a.type_name(),
            b.type_name(),
            c.type_name()
          )),
        }
      },
    ))
    // ── rand module ────────────────────────────────────────────────────────
    // (rand::rng seed "name") -> Cell(Int)
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
        vec![TypeConst::Int, TypeConst::String],
        None,
        TypeConst::cell(TypeConst::Int),
      ),
      |ctx, args| {
        let (seed, name) = (args[0], args[1]);
        let parent = match seed {
          Value::Int(i) => i,
          other => {
            return Err(format!(
              "rand::rng: expected Int seed, got {}",
              other.type_name()
            ))
          }
        };
        let ns = match &name {
          Value::Heap(heap) => match &heap.value {
            SLVal::String(s) => s.as_str(),
            _ => {
              return Err(format!(
                "rand::rng: expected String name, got {}",
                name.type_name()
              ))
            }
          },
          other => {
            return Err(format!(
              "rand::rng: expected String name, got {}",
              other.type_name()
            ))
          }
        };
        let state = Value::Int(rand_rng(parent, ns));
        let contents = CellContents::new(state);
        Ok(Value::Cell(Gc::new(ctx.mc(), RefLock::new(contents))))
      },
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
        vec![TypeConst::cell(TypeConst::Int), TypeConst::Int],
        None,
        TypeConst::Int,
      ),
      |ctx, args| {
        let (rng, sides) = (args[0], args[1]);
        let cell = match rng {
          Value::Cell(c) => c,
          other => {
            return Err(format!(
              "rand::roll!: expected Cell rng, got {}",
              other.type_name()
            ))
          }
        };
        let n = match sides {
          Value::Int(i) => i,
          other => {
            return Err(format!(
              "rand::roll!: expected Int sides, got {}",
              other.type_name()
            ))
          }
        };
        if n <= 0 {
          return Err(format!("rand::roll!: sides must be positive, got {}", n));
        }
        let s = match cell.borrow().value {
          Value::Int(i) => i,
          other => {
            return Err(format!(
              "rand::roll!: expected Cell to hold an Int, got {}",
              other.type_name()
            ))
          }
        };
        let (roll, next) = rand_roll(s, n);
        Gc::write(ctx.mc(), cell)
          .unlock()
          .borrow_mut()
          .set(Value::Int(next));
        Ok(Value::Int(roll))
      },
    ))
}

/// Derive a deterministic 64-bit seed from a parent seed and a name, using
/// BLAKE3. The 64-bit result is the first 8 bytes of the BLAKE3 XOF output.
fn rand_rng(parent_seed: i64, name: &str) -> i64 {
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
fn rand_roll(seed: i64, sides: i64) -> (i64, i64) {
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
mod builtins_tests;
