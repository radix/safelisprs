use std::cell::Cell;
use std::rc::Rc;
use std::time::{Duration, Instant};

use gc_arena::{collect::Collect, Arena, Gc, Mutation, RefLock, Rootable};

use crate::builtins::{default_builtins, Builtins};
use crate::compiler::{Callable, Instruction, LinkedFunction as Function, Package};

/// Per-execution memory accounting. Shared between the `Execution` (which owns
/// the canonical `Rc`) and every `Accounted` box allocated in that execution's
/// arena (each carries a cloned `Rc`). Holds the running external-bytes count
/// (Rust-heap storage owned by `SLVal` payloads but not seen by gc-arena's
/// `Metrics`) and an optional cap. `Rc<Cell<_>>` is appropriate because
/// gc-arena is single-threaded; if `Execution` ever needs to be `Send`, swap
/// for `Arc<AtomicUsize>`.
#[derive(Default)]
pub struct MemoryTracker {
  external_bytes: Cell<usize>,
  limit: Cell<Option<usize>>,
}

impl MemoryTracker {
  pub fn new() -> Self {
    Self::default()
  }

  /// Charge `n` bytes to this tracker's running count. Returns the new total.
  fn charge(&self, n: usize) -> usize {
    let new = self
      .external_bytes
      .get()
      .checked_add(n)
      .expect("external byte count overflow");
    self.external_bytes.set(new);
    new
  }

  /// Release `n` bytes from this tracker's running count (called from
  /// `Accounted::drop` when the GC sweeps an unreachable box). Returns the new
  /// total.
  fn release(&self, n: usize) -> usize {
    let prev = self.external_bytes.get();
    let new = prev.checked_sub(n).expect("external byte count underflow");
    self.external_bytes.set(new);
    new
  }

  pub fn external_bytes(&self) -> usize {
    self.external_bytes.get()
  }

  pub fn limit(&self) -> Option<usize> {
    self.limit.get()
  }

  pub fn set_limit(&self, limit: Option<usize>) {
    self.limit.set(limit);
  }
}

/// A `MemoryTracker` shared via `Rc` so it can live inside a GC'd `Accounted`
/// box. Each `Execution` mints one `Rc<MemoryTracker>`; every value allocated
/// in that execution receives a clone. The tracker stays alive until the last
/// `Accounted` box is swept (which releases its `Rc`).
pub type SharedTracker = Rc<MemoryTracker>;

/// Measure the *directly owned* Rust-heap storage of an `SLVal` payload — the
/// bytes that live outside the `GcBox` layout gc-arena records. This is what
/// `Accounted` charges the tracker at allocation and releases at sweep:
///
/// - `String`: the `String`'s heap buffer (`capacity()`, not `len`).
/// - `List`/`Partial`: the `Vec`'s backing array
///   (`capacity() * size_of::<Gc<_>>()`).
/// - Other variants: 0 (scalars, `Void`, `FunctionRef`, `Cell` whose contents
///   are themselves `Gc`-pointed and accounted by their own box).
///
/// Only *directly owned* storage is counted; pointed-to `Gc` boxes are
/// accounted by their own `Accounted` wrapper, so there's no double-counting.
fn external_bytes_of<'gc>(value: &SLVal<'gc>) -> usize {
  match value {
    SLVal::String(s) => s.capacity(),
    SLVal::List(items) => items.capacity() * std::mem::size_of::<Gc<'gc, Accounted<'gc>>>(),
    SLVal::Partial(p) => p.args.capacity() * std::mem::size_of::<Gc<'gc, Accounted<'gc>>>(),
    _ => 0,
  }
}

/// A garbage-collected SafeLisp value wrapped with per-execution memory
/// accounting. The `value: SLVal` is the actual payload; `tracker` and
/// `external_bytes` record the Rust-heap storage `value` directly owns (string
/// bytes, list/partial `Vec` backings) so the `Execution`'s memory limit can
/// see storage that gc-arena's `Metrics` does not.
///
/// `Accounted` is `!Clone` by design: every runtime value must be created via
/// `ExecRoot::alloc_value`, which charges the tracker. `Drop` releases the
/// charge — it touches only `tracker` and the `usize`, never `value` (which
/// may contain dead `Gc` pointers during sweep), satisfying the
/// `#[collect(unsafe_drop)]` safety condition.
#[derive(Collect)]
#[collect(unsafe_drop)]
pub struct Accounted<'gc> {
  pub value: SLVal<'gc>,
  #[collect(require_static)]
  tracker: SharedTracker,
  external_bytes: usize,
}

impl<'gc> std::fmt::Debug for Accounted<'gc> {
  /// Debug-format only the `SLVal` payload; the tracker and byte count are
  /// accounting metadata, not part of the value's identity.
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("Accounted").field("value", &self.value).finish()
  }
}

impl<'gc> PartialEq for Accounted<'gc> {
  /// Compare accounted values by their `SLVal` payload. The tracker and
  /// `external_bytes` are accounting metadata, not part of the value's identity.
  fn eq(&self, other: &Self) -> bool {
    self.value == other.value
  }
}

impl<'gc> Eq for Accounted<'gc> {}

impl<'gc> Accounted<'gc> {
  /// Construct an `Accounted` value, charge its external storage to `tracker`,
  /// and return it ready to be boxed via `Gc::new`. Callers should go through
  /// `ExecRoot::alloc_value` rather than calling this directly.
  fn new(value: SLVal<'gc>, tracker: SharedTracker) -> Self {
    let external_bytes = external_bytes_of(&value);
    tracker.charge(external_bytes);
    Accounted {
      value,
      tracker,
      external_bytes,
    }
  }
}

impl Drop for Accounted<'_> {
  fn drop(&mut self) {
    // SAFETY for `#[collect(unsafe_drop)]`: this must not inspect `self.value`,
    // which may contain dangling `Gc` pointers during arena teardown. We touch
    // only `tracker` (a `Rc<MemoryTracker>`, `require_static`) and the
    // `external_bytes` count.
    self.tracker.release(self.external_bytes);
  }
}

/// Per-execution state held inside an `Execution`'s own arena. Each `Execution`
/// owns a private `Arena` whose root is this type. All interpreter logic lives
/// as methods on `ExecRoot`, so that stepping a single bytecode or running to
/// completion can call `self.step(mc, …)`, `self.pop()`, `self.is_done()`, etc.
/// — the same shape the original `Rc`-based `Execution` had, just branded with
/// the gc-arena invariant `'gc` lifetime so that `Gc` pointers never escape a
/// single `mutate_root` callback.
#[derive(Collect)]
#[collect(no_drop)]
pub(crate) struct ExecRoot<'gc> {
  stack: Vec<Gc<'gc, Accounted<'gc>>>,
  frames: Vec<Frame<'gc>>,
  /// Per-execution memory tracker. Owns the running external-bytes count (Rust
  /// heap storage not seen by gc-arena's `Metrics`) and the optional cap. Each
  /// `Accounted` box allocated in this arena carries a clone of this `Rc`, so
  /// `Accounted::drop` (invoked by the GC at sweep) decrements the count for
  /// the right execution. The `require_static` field attribute on `Accounted`
  /// excludes this `Rc` from tracing.
  #[collect(require_static)]
  tracker: SharedTracker,
}

impl<'gc> ExecRoot<'gc> {
  /// The single chokepoint for creating a `Gc<Accounted>` value. Wraps the
  /// `SLVal` in `Accounted` (charging the tracker for any directly-owned
  /// external Rust-heap storage) and boxes it via `Gc::new`. Every runtime
  /// allocation must go through here so unaccounted values are hard to create
  /// by accident.
  pub(crate) fn alloc_value(&mut self, mc: &'gc Mutation<'gc>, value: SLVal<'gc>) -> Gc<'gc, Accounted<'gc>> {
    Gc::new(mc, Accounted::new(value, self.tracker.clone()))
  }

  /// Reconstruct a `Gc<Accounted>` inside this execution's arena from an owned
  /// `SLValue`, allocating fresh accounted `Gc` pointers for any non-scalar
  /// sub-values. The arena-agnostic `SLValue` is deep-copied in, with each new
  /// `Accounted` box charged to this execution's tracker.
  fn import_value(&mut self, mc: &'gc Mutation<'gc>, value: &SLValue) -> Gc<'gc, Accounted<'gc>> {
    match value {
      SLValue::Int(i) => self.alloc_value(mc, SLVal::Int(*i)),
      SLValue::Float(x) => self.alloc_value(mc, SLVal::Float(*x)),
      SLValue::String(s) => self.alloc_value(mc, SLVal::String(s.clone())),
      SLValue::Bool(b) => self.alloc_value(mc, SLVal::Bool(*b)),
      SLValue::Void => self.alloc_value(mc, SLVal::Void),
      SLValue::FunctionRef(m, n) => self.alloc_value(mc, SLVal::FunctionRef(*m, *n)),
      SLValue::Partial { function, args } => {
        let sub: Vec<Gc<'gc, Accounted<'gc>>> =
          args.iter().map(|a| self.import_value(mc, a)).collect();
        self.alloc_value(
          mc,
          SLVal::Partial(Partial {
            function: *function,
            args: sub,
          }),
        )
      }
      SLValue::Cell(inner) => {
        let inner_gc = self.import_value(mc, inner);
        self.alloc_value(mc, SLVal::Cell(Gc::new(mc, RefLock::new(inner_gc.value.clone()))))
      }
      SLValue::List(items) => {
        let sub: Vec<Gc<'gc, Accounted<'gc>>> =
          items.iter().map(|i| self.import_value(mc, i)).collect();
        self.alloc_value(mc, SLVal::List(sub))
      }
    }
  }
}

/// A stack frame. The function is stored by `(module, function)` index
/// (`FrameFunc::Indexed`) so a frame is cheap to push — no
/// `instructions: Vec<…>` clone — and the `Function` is looked up via
/// `package.get_function(...)` at `step` time. The `FrameFunc::Inline` variant
/// holds an owned `Function` for ad-hoc entry points (e.g. tests that build a
/// `Function` without registering it in a `Package`); it clones once on
/// frame push, which is fine for shallow test recursion.
#[derive(Collect, Clone)]
#[collect(no_drop)]
struct Frame<'gc> {
  function: FrameFunc,
  locals: Vec<Gc<'gc, Accounted<'gc>>>,
  ip: usize,
}

#[derive(Collect, Clone)]
#[collect(no_drop)]
enum FrameFunc {
  /// Look up the function by `(module, function)` index via the `Package` at
  /// `step` time. The production path (call_main, call_fixed, call_dynamic).
  Indexed((u32, u32)),
  /// An ad-hoc `Function` not registered in any `Package` (test entry points
  /// that build bytecode directly). Owns its instructions.
  Inline(Function),
}

// `Function` (a.k.a. `LinkedFunction`) is `'static` and holds no `Gc` pointers,
// so it gets a trivial, empty `Collect` impl. (Kept for callers that store
// `Function` directly, e.g. `Execution::enter_function`'s signature.)
gc_arena::static_collect!(Function);

/// A SafeLisp value. This is the in-arena representation: anything that needs
/// sharing or cycle-detection is held behind a `Gc` pointer.
#[derive(Debug, PartialEq, Clone, Collect)]
#[collect(no_drop)]
pub enum SLVal<'gc> {
  Int(i64),
  Float(f64),
  String(String),
  Bool(bool),
  Void,
  FunctionRef(u32, u32),
  Partial(Partial<'gc>),
  /// A cell holding a mutable `SLVal`. The `RefLock<SLVal>` box is *not*
  /// wrapped in `Accounted` (the contents are mutated in place via `SetCell`,
  /// and `Accounted` is `!Clone`), so the contents' external bytes are not
  /// charged here. This is a known undercount for cells holding strings/lists;
  /// refine in a follow-up.
  Cell(Gc<'gc, RefLock<SLVal<'gc>>>),
  List(Vec<Gc<'gc, Accounted<'gc>>>),
}

#[derive(Debug, PartialEq, Clone, Collect)]
#[collect(no_drop)]
pub struct Partial<'gc> {
  function: (u32, u32),
  args: Vec<Gc<'gc, Accounted<'gc>>>,
}

/// An owned, arena-agnostic value that can escape the arena and be fed back
/// into any other (or the same) execution. Non-scalar values are represented
/// recursively with owned `SLValue` sub-values, so converting across the arena
/// boundary performs a deep copy: `Gc` pointers in one arena are unboxed into
/// owned values and re-boxed into fresh `Gc` pointers in the destination arena.
#[derive(Debug, PartialEq, Clone)]
pub enum SLValue {
  Int(i64),
  Float(f64),
  String(String),
  Bool(bool),
  Void,
  FunctionRef(u32, u32),
  Partial {
    function: (u32, u32),
    args: Vec<SLValue>,
  },
  Cell(Box<SLValue>),
  List(Vec<SLValue>),
}

impl<'gc> SLVal<'gc> {
  /// Convert an `SLVal` into an owned `SLValue`, deep-copying any `Gc`-held
  /// sub-values out of the arena.
  fn to_value(&self) -> SLValue {
    match self {
      SLVal::Int(i) => SLValue::Int(*i),
      SLVal::Float(x) => SLValue::Float(*x),
      SLVal::String(s) => SLValue::String(s.clone()),
      SLVal::Bool(b) => SLValue::Bool(*b),
      SLVal::Void => SLValue::Void,
      SLVal::FunctionRef(m, n) => SLValue::FunctionRef(*m, *n),
      SLVal::Partial(p) => SLValue::Partial {
        function: p.function,
        args: p.args.iter().map(|a| a.value.to_value()).collect(),
      },
      SLVal::Cell(r) => SLValue::Cell(Box::new(r.borrow().to_value())),
      SLVal::List(items) => SLValue::List(items.iter().map(|i| i.value.to_value()).collect()),
    }
  }
}

pub struct Interpreter {
  package: Package,
  builtins: Builtins,
}

impl Interpreter {
  pub fn with_builtins(package: Package, builtins: Builtins) -> Self {
    Interpreter { package, builtins }
  }

  /// Construct an interpreter that will run with [`default_builtins`].
  pub fn new(package: Package) -> Self {
    Interpreter::with_builtins(package, default_builtins())
  }
}

impl Interpreter {
  /// Set up an `Execution` ready to run `main`.
  pub fn call_main(&self) -> Result<Execution, String> {
    if let Some((module, function)) = self.package.main {
      let callable = self
        .package
        .get_function(module, function)
        .ok_or_else(|| format!("Couldn't find module {} function {}", module, function))?;
      if let Callable::Function(_) = callable {
        let mut exec = Execution::new(self.package.clone(), self.builtins.clone());
        exec.enter_function_at(module, function, vec![])?;
        Ok(exec)
      } else {
        Err(format!(
          "{}.{} is a builtin. We can only call regular functions.",
          module, function
        ))
      }
    } else {
      Err("This package has no main function.".to_string())
    }
  }

  /// Set up an `Execution` ready to call `slval`. The `SLValue` is deep-copied
  /// into the new execution's arena, so a value produced by one execution can
  /// be fed into any other execution (or the same one).
  pub fn call_slval(&self, slval: SLValue) -> Result<Execution, String> {
    let mut exec = Execution::new(self.package.clone(), self.builtins.clone());
    exec.push_and_call_dynamic(slval)?;
    Ok(exec)
  }
}

#[derive(Debug, PartialEq)]
pub enum Status {
  /// Execution paused after exhausting the budget for this `run` call.
  /// The execution can be resumed by calling `run` again. All state
  /// (frames, stack, IP, counter) is preserved.
  Paused,
  /// Execution completed: the frame stack is empty and the final result
  /// is carried here. The `Execution` should not be resumed after this.
  Done(SLValue),
}

/// A single, independent SafeLisp execution. Each `Execution` owns its own
/// garbage-collected `Arena`, so multiple executions are fully independent
/// and may be run in parallel (cooperatively interleaved by the caller). The
/// `Arena` holds the value stack and call frames branded with an invariant
/// `'gc` lifetime; values that must cross the arena boundary (such as the
/// result of `run`) are deep-copied to the arena-agnostic `SLValue`.
pub struct Execution {
  arena: Arena<Rootable![ExecRoot<'_>]>,
  package: Package,
  builtins: Builtins,
  pub executed: u64,
  /// Per-execution memory tracker, shared with every `Accounted` box in this
  /// arena. `Execution` owns the canonical `Rc`; cloned into each `Accounted`
  /// at allocation (see `ExecRoot::alloc_value`).
  tracker: SharedTracker,
}

impl Execution {
  pub fn new(package: Package, builtins: Builtins) -> Self {
    let tracker = Rc::new(MemoryTracker::new());
    let tracker_for_root = tracker.clone();
    let arena = Arena::new(move |_mc| ExecRoot {
      stack: Vec::new(),
      frames: Vec::new(),
      tracker: tracker_for_root.clone(),
    });
    Execution {
      arena,
      package,
      builtins,
      executed: 0,
      tracker,
    }
  }

  /// Returns true if the frame stack is empty (execution is complete).
  pub fn is_done(&self) -> bool {
    self.arena.mutate(|_, root| root.is_done())
  }

  /// The number of items currently on the value stack.
  pub fn stack_len(&self) -> usize {
    self.arena.mutate(|_, root| root.stack.len())
  }

  /// Peek the top of the value stack as an owned `SLValue`. Returns an error if
  /// the stack is empty.
  pub fn peek_value(&self) -> Result<SLValue, String> {
    let mut result = Err("PEEK on an empty stack".to_string());
    self.arena.mutate(|_, root| {
      if let Some(top) = root.stack.last().copied() {
        result = Ok(top.value.to_value());
      }
    });
    result
  }

  /// The current number of live `Gc` allocations in this execution's arena.
  /// Useful for tests that want to observe garbage collection.
  pub fn gc_count(&self) -> usize {
    self.arena.metrics().total_gc_count()
  }

  /// The total bytes currently allocated by live `Gc` pointers in this
  /// execution's arena (the gc-arena-reported portion of `memory_usage`).
  pub fn gc_allocation_bytes(&self) -> usize {
    self.arena.metrics().total_gc_allocation()
  }

  /// The current live memory in bytes: gc-arena's `total_gc_allocation` (the
  /// `GcBox` layouts) plus this execution's `external_bytes` (Rust-heap
  /// storage owned by `SLVal` payloads — String bytes, List/Partial `Vec`
  /// backings — accounted via `Accounted`). This is the value the optional
  /// memory limit is compared against on every `step`.
  ///
  /// Note (step 2): `stack`/`frames`/`locals` `Vec` overhead is not yet
  /// included; step 3 of the memory-limit plan lands that.
  pub fn memory_usage(&self) -> usize {
    self.arena.metrics().total_gc_allocation() + self.tracker.external_bytes()
  }

  /// Cap the live memory for this execution. When set, `step` (and the
  /// `run*` helpers that drive it) will return an `Err` as soon as
  /// `memory_usage` exceeds this many bytes. Set to `None` to disable the
  /// limit (the default).
  pub fn set_memory_limit(&mut self, limit: Option<usize>) {
    self.tracker.set_limit(limit);
  }

  /// The configured memory limit in bytes, or `None` if no limit is set.
  pub fn memory_limit(&self) -> Option<usize> {
    self.tracker.limit()
  }

  /// Force a full garbage-collection cycle, freeing all unreachable `Gc`
  /// pointers. Useful for tests that want to verify that collection reclaims
  /// garbage.
  pub fn collect_all(&mut self) {
    self.arena.finish_cycle();
  }

  /// Run up to `n` bytecodes. Returns `Paused` if the budget exhausted before
  /// completion, or `Done(v)` if the program completed. Errors propagate via
  /// `Err`. The cumulative count of executed bytecodes is available on
  /// `Execution::executed` after the call returns.
  pub fn run(&mut self, n: u64) -> Result<Status, String> {
    let start = self.executed;
    let mut executed = self.executed;
    let outcome = self
      .arena
      .mutate_root(|mc, root| root.run(mc, &self.package, &self.builtins, start, n, &mut executed));
    self.executed = executed;
    // Run a bit of incremental collection, paced by allocation debt.
    self.arena.collect_debt();
    outcome
  }

  /// Convenience: run to completion with no instruction limit. Returns the
  /// final value, or errors on a runtime error.
  pub fn run_until_done(&mut self) -> Result<SLValue, String> {
    let mut executed = self.executed;
    let result = self.arena.mutate_root(|mc, root| {
      root.run_until_done(mc, &self.package, &self.builtins, &mut executed)
    });
    self.executed = executed;
    self.arena.collect_debt();
    result
  }

  /// Run for up to `duration`, stepping until the deadline is reached or
  /// execution completes. Returns `Paused` if the deadline expired before
  /// completion, or `Done(v)` if the program completed. Errors propagate via
  /// `Err`. The cumulative count of executed bytecodes is available on
  /// `Execution::executed` after the call returns.
  pub fn run_for_duration(&mut self, duration: Duration) -> Result<Status, String> {
    let deadline = Instant::now() + duration;
    let mut executed = self.executed;
    let outcome = self.arena.mutate_root(|mc, root| {
      root.run_for_duration(mc, &self.package, &self.builtins, deadline, &mut executed)
    });
    self.executed = executed;
    self.arena.collect_debt();
    outcome
  }

  /// Execute one bytecode instruction from the current top frame.
  pub fn step(&mut self) -> Result<(), String> {
    let mut executed = self.executed;
    let result = self
      .arena
      .mutate_root(|mc, root| root.step(mc, &self.package, &self.builtins, &mut executed));
    self.executed = executed;
    self.arena.collect_debt();
    result
  }

  /// Push a frame for a function, with optional pre-bound values given as owned
  /// `SLValue`s (used by external/test entry points). The remaining params
  /// (after the pre-bound ones) are popped from the stack. The function is
  /// stored inline in the frame (not looked up via a `Package`), so this is
  /// suitable for ad-hoc/test entry points that build bytecode directly. For
  /// production paths prefer `enter_function_at` (indexed lookup, no clone).
  pub fn enter_function(
    &mut self,
    function: Function,
    pre_bound: Vec<SLValue>,
  ) -> Result<(), String> {
    let result = self.arena.mutate_root(|mc, root| {
      let pre_bound_gc: Vec<Gc<'_, Accounted<'_>>> =
        pre_bound.iter().map(|v| root.import_value(mc, v)).collect();
      root.enter_inline_function(mc, function.clone(), pre_bound_gc)
    });
    result
  }

  /// Push a frame for a function looked up by `(module, function)` index in
  /// this execution's `Package`, with optional pre-bound values given as owned
  /// `SLValue`s. The frame stores the index (not a clone of the function's
  /// instructions), so deep recursion is cheap.
  pub fn enter_function_at(
    &mut self,
    mod_idx: u32,
    func_idx: u32,
    pre_bound: Vec<SLValue>,
  ) -> Result<(), String> {
    let function = match self.package.get_function(mod_idx, func_idx) {
      Some(crate::compiler::Callable::Function(f)) => f.clone(),
      _ => {
        return Err(format!(
          "Function not found: {}/{}",
          mod_idx, func_idx
        ))
      }
    };
    let result = self.arena.mutate_root(|mc, root| {
      let pre_bound_gc: Vec<Gc<'_, Accounted<'_>>> =
        pre_bound.iter().map(|v| root.import_value(mc, v)).collect();
      root.enter_function(mc, mod_idx, func_idx, &function, pre_bound_gc)
    });
    result
  }

  /// Push a value onto the stack and invoke `call_dynamic` (used to set up an
  /// execution from an external `SLValue`).
  fn push_and_call_dynamic(&mut self, value: SLValue) -> Result<(), String> {
    let package = self.package.clone();
    let builtins = self.builtins.clone();
    let result = self.arena.mutate_root(|mc, root| {
      let gc = root.import_value(mc, &value);
      root.stack.push(gc);
      root.call_dynamic(mc, &package, &builtins, 0)
    });
    result
  }
}

impl<'gc> ExecRoot<'gc> {
  /// Returns true if the frame stack is empty (execution is complete).
  fn is_done(&self) -> bool {
    self.frames.is_empty()
  }

  /// Returns `Ok(())` if the live memory (gc-arena's `total_gc_allocation` +
  /// this execution's external bytes) is within the configured limit, or `Err`
  /// with a descriptive message if it has been exceeded. No-op when no limit is
  /// set. Step 2 of the memory-limit plan: only GC-box bytes + per-`Gc`-box
  /// external payload (String/List/Partial backing) are counted; stack and
  /// frame `Vec` overhead lands in step 3.
  fn check_memory_limit(&self, mc: &Mutation<'gc>) -> Result<(), String> {
    if let Some(limit) = self.tracker.limit() {
      let usage = mc.metrics().total_gc_allocation() + self.tracker.external_bytes();
      if usage > limit {
        return Err(format!(
          "memory limit exceeded: {} bytes live (limit {})",
          usage, limit
        ));
      }
    }
    Ok(())
  }

  /// Push a `Gc<Accounted>` onto the execution's value stack. Used by
  /// [`Builtin::call`] to push a builtin's result.
  pub(crate) fn push_gc(&mut self, val: Gc<'gc, Accounted<'gc>>) {
    self.stack.push(val);
  }

  /// Pop a value off the execution's value stack.
  fn pop(&mut self) -> Result<Gc<'gc, Accounted<'gc>>, String> {
    self
      .stack
      .pop()
      .ok_or_else(|| "POP on an empty stack".to_string())
  }

  /// Run up to `n` bytecodes starting from `start` executed count. Returns
  /// `Paused` if the budget exhausted before completion, or `Done(v)` if the
  /// program completed. Errors propagate via `Err`.
  fn run(
    &mut self,
    mc: &'gc Mutation<'gc>,
    package: &Package,
    builtins: &Builtins,
    start: u64,
    n: u64,
    executed: &mut u64,
  ) -> Result<Status, String> {
    while !self.is_done() && *executed - start < n {
      self.step(mc, package, builtins, executed)?;
    }
    if self.is_done() {
      let top = self.pop()?;
      Ok(Status::Done(top.value.to_value()))
    } else {
      Ok(Status::Paused)
    }
  }

  /// Run to completion with no instruction limit. Returns the final value, or
  /// errors on a runtime error.
  fn run_until_done(
    &mut self,
    mc: &'gc Mutation<'gc>,
    package: &Package,
    builtins: &Builtins,
    executed: &mut u64,
  ) -> Result<SLValue, String> {
    while !self.is_done() {
      self.step(mc, package, builtins, executed)?;
    }
    let top = self.pop()?;
    Ok(top.value.to_value())
  }

  /// Run until `deadline` is reached or execution completes. Returns `Paused`
  /// if the deadline expired before completion, or `Done(v)` if the program
  /// completed. Errors propagate via `Err`.
  fn run_for_duration(
    &mut self,
    mc: &'gc Mutation<'gc>,
    package: &Package,
    builtins: &Builtins,
    deadline: Instant,
    executed: &mut u64,
  ) -> Result<Status, String> {
    while !self.is_done() && Instant::now() < deadline {
      self.step(mc, package, builtins, executed)?;
    }
    if self.is_done() {
      let top = self.pop()?;
      Ok(Status::Done(top.value.to_value()))
    } else {
      Ok(Status::Paused)
    }
  }

  /// Execute one bytecode instruction from the current top frame.
  fn step(
    &mut self,
    mc: &'gc Mutation<'gc>,
    package: &Package,
    builtins: &Builtins,
    executed: &mut u64,
  ) -> Result<(), String> {
    *executed = executed.saturating_add(1);
    self.check_memory_limit(mc)?;

    // Resolve the current frame's instructions. `Indexed` looks up via the
    // package (production path — no clone); `Inline` reads the owned
    // `Function` directly (test path). Either way we advance `ip` after
    // fetching the instruction.
    let (ip, inst) = {
      let frame = self
        .frames
        .last()
        .ok_or_else(|| "step with no frames".to_string())?;
      let ip = frame.ip;
      let inst = match &frame.function {
        FrameFunc::Indexed((mod_idx, func_idx)) => {
          match package.get_function(*mod_idx, *func_idx) {
            Some(crate::compiler::Callable::Function(f)) => {
              if ip >= f.instructions.len() {
                return Err("ran past end of function without Return".to_string());
              }
              f.instructions[ip].clone()
            }
            _ => {
              return Err(format!(
                "Function not found while stepping: {}/{}",
                mod_idx, func_idx
              ))
            }
          }
        }
        FrameFunc::Inline(f) => {
          if ip >= f.instructions.len() {
            return Err("ran past end of function without Return".to_string());
          }
          f.instructions[ip].clone()
        }
      };
      (ip, inst)
    };
    {
      let frame = self
        .frames
        .last_mut()
        .ok_or_else(|| "step with no frames".to_string())?;
      frame.ip = ip + 1;
    }

    match inst {
      Instruction::PushInt(i) => { let v = self.alloc_value(mc, SLVal::Int(i)); self.stack.push(v); },
      Instruction::PushFloat(f) => { let v = self.alloc_value(mc, SLVal::Float(f)); self.stack.push(v); },
      Instruction::PushString(s) => { let v = self.alloc_value(mc, SLVal::String(s)); self.stack.push(v); },
      Instruction::PushBool(b) => { let v = self.alloc_value(mc, SLVal::Bool(b)); self.stack.push(v); },
      Instruction::Pop => {
        self.pop()?;
      }
      Instruction::Return => {
        // Pop the top frame; the return value stays on self.stack.
        self.frames.pop();
      }
      Instruction::SetLocal(i) => {
        let val = self.pop()?;
        let frame = self
          .frames
          .last_mut()
          .ok_or_else(|| "SetLocal with no frame".to_string())?;
        frame.locals[usize::from(i)] = val;
      }
      Instruction::LoadLocal(i) => {
        let frame = self
          .frames
          .last()
          .ok_or_else(|| "LoadLocal with no frame".to_string())?;
        self.stack.push(frame.locals[usize::from(i)]);
      }
      Instruction::Call((mod_index, func_index), arity) => {
        self.call_fixed(mc, package, builtins, mod_index, func_index, arity)?;
      }
      Instruction::CallDynamic(arity) => {
        self.call_dynamic(mc, package, builtins, arity)?;
      }
      Instruction::MakeFunctionRef((mod_index, func_index)) => {
        let v = self.alloc_value(mc, SLVal::FunctionRef(mod_index, func_index));
        self.stack.push(v);
      }
      Instruction::MakeCell => {
        let val = self.pop()?;
        let cell = self.alloc_value(
          mc,
          SLVal::Cell(Gc::new(mc, RefLock::new(val.value.clone()))),
        );
        self.stack.push(cell);
      }
      Instruction::DerefCell => {
        let val = self.pop()?;
        match &val.value {
          SLVal::Cell(r) => {
            let cloned: SLVal<'gc> = r.borrow().clone();
            let gc = self.alloc_value(mc, cloned);
            self.stack.push(gc);
          }
          other => return Err(format!("Not a cell: {:?}", other)),
        }
      }
      Instruction::SetCell => {
        // Pop the Cell (TOS) and the new value (below it), write the value
        // into the Cell, and push the value back as the result of `set!`.
        let cell_gc = self.pop()?;
        let new_val = self.pop()?;
        match &cell_gc.value {
          SLVal::Cell(cell_ref) => {
            // Mutate the Cell's contents in a GC-safe way.
            *Gc::write(mc, *cell_ref).unlock().borrow_mut() = new_val.value.clone();
          }
          other => return Err(format!("Not a cell: {:?}", other)),
        }
        self.stack.push(new_val);
      }
      Instruction::PartialApply(num_args) => {
        self.partial_apply(mc, num_args)?;
      }
      Instruction::Jump(offset) => {
        let frame = self
          .frames
          .last_mut()
          .ok_or_else(|| "Jump with no frame".to_string())?;
        // `offset` is relative to the instruction following the `Jump` (the IP
        // has already been advanced past it), so `Jump(0)` is a no-op.
        frame.ip = frame.ip.wrapping_add(offset as usize);
      }
      Instruction::JumpIfFalse(offset) => {
        let val = self.pop()?;
        match &val.value {
          SLVal::Bool(false) => {
            let frame = self
              .frames
              .last_mut()
              .ok_or_else(|| "JumpIfFalse with no frame".to_string())?;
            frame.ip = frame.ip.wrapping_add(offset as usize);
          }
          SLVal::Bool(true) => {}
          other => return Err(format!("`if` condition must be a bool, got {:?}", other)),
        }
      }
    }
    Ok(())
  }

  fn partial_apply(&mut self, mc: &'gc Mutation<'gc>, num_args: u16) -> Result<(), String> {
    let func = self.pop()?;
    let mut args = vec![];
    for _ in 0..num_args {
      args.push(self.pop()?);
    }
    args.reverse();
    let closure = match &func.value {
      SLVal::FunctionRef(mod_index, func_index) => self.alloc_value(
        mc,
        SLVal::Partial(Partial {
          function: (*mod_index, *func_index),
          args,
        }),
      ),
      _ => return Err("make_closure needs a function at TOS".to_string()),
    };
    self.stack.push(closure);
    Ok(())
  }

  /// Either pushes a frame for the function if it's defined in SafeLisp, or
  /// just call it immediately if it's a builtin. `arity` is the number of args
  /// pushed at the call site (carried on [`Instruction::Call`]); it is used to
  /// pop the right number of args for the callee and to arity-check at the call
  /// site. For variadic builtins (whose `BuiltinSpec::num_params` is `None`) the
  /// arity is the only source of the arg count.
  fn call_fixed(
    &mut self,
    mc: &'gc Mutation<'gc>,
    package: &Package,
    builtins: &Builtins,
    mod_index: u32,
    func_index: u32,
    arity: u16,
  ) -> Result<(), String> {
    let (module_name, functions) = package
      .get_module(mod_index)
      .ok_or_else(|| format!("Module not found: {}", mod_index))?;
    let (func_name, callable) = functions
      .get(func_index as usize)
      .ok_or_else(|| format!("Function not found: {}/{}", mod_index, func_index))?;
    match callable {
      Callable::Function(func) => {
        if arity != func.num_params {
          return Err(format!(
            "{}.{} expects {} arg(s) but was called with {}",
            module_name, func_name, func.num_params, arity
          ));
        }
        self.enter_function(mc, mod_index, func_index, func, vec![])?;
      }
      Callable::Builtin => {
        let builtin = builtins
          .lookup(module_name, func_name)
          .ok_or_else(|| format!("No builtin {}.{}", module_name, func_name))?;
        // For fixed-arity builtins, enforce the declared arity at the call site.
        if let Some(expected) = builtin.spec().num_params {
          if arity != expected {
            return Err(format!(
              "{}.{} expects {} arg(s) but was called with {}",
              module_name, func_name, expected, arity
            ));
          }
        }
        // For variadic builtins (`num_params == None`) the call-site arity is
        // the only source of the arg count.
        let n = arity as usize;
        // Args were pushed left-to-right; popping gives reverse order.
        let mut args = Vec::with_capacity(n);
        for _ in 0..n {
          args.push(self.pop()?);
        }
        args.reverse();
        let mut ctx = HostCtx {
          mc,
          package,
          builtins,
        };
        builtin.call(self, &mut ctx, &args)?;
      }
    }
    Ok(())
  }

  /// Prepare the callable that's on the top of stack to be called. This pushes
  /// a new frame for function callables. This is both the implementation of the
  /// `CallDynamic` instruction and the entry point used by
  /// `Interpreter::call_slval`. `arity` is the number of args pushed at the
  /// call site (carried on [`Instruction::CallDynamic`]); see [`Self::call_fixed`].
  fn call_dynamic(
    &mut self,
    mc: &'gc Mutation<'gc>,
    package: &Package,
    builtins: &Builtins,
    arity: u16,
  ) -> Result<(), String> {
    let callable = self.pop()?;
    match &callable.value {
      SLVal::FunctionRef(mod_index, func_index) => {
        self.call_fixed(mc, package, builtins, *mod_index, *func_index, arity)
      }
      SLVal::Partial(Partial {
        function: (mod_index, func_index),
        args,
      }) => {
        let (_, functions) = package
          .get_module(*mod_index)
          .ok_or_else(|| format!("Module not found: {}", mod_index))?;
        let (func_name, callable) = functions
          .get(*func_index as usize)
          .ok_or_else(|| format!("Function not found: {}/{}", mod_index, func_index))?;
        match callable {
          Callable::Function(func) => {
            // The remaining params (after the pre-bound ones) must be supplied
            // by the call site; arity-check that they line up.
            let expected = func.num_params.saturating_sub(args.len() as u16);
            if arity != expected {
              return Err(format!(
                "{}.{} expects {} more arg(s) but was called with {}",
                mod_index, func_name, expected, arity
              ));
            }
            self.enter_function(mc, *mod_index, *func_index, func, args.clone())
          }
          Callable::Builtin => Err("Can't invoke a builtin as a closure".to_string()),
        }
      }
      x => Err(format!("Can't call a non-callable! {:?}", x)),
    }
  }

  /// Push a new frame for a function, with optional pre-bound values. The
  /// remaining params (after the pre-bound ones) are popped from the stack.
  /// `function` is the looked-up `Function` (used for `num_locals`/`num_params`
  /// metadata); `(mod_idx, func_idx)` is stored in the frame so the
  /// instructions are looked up by reference at `step` time rather than cloned
  /// into the frame.
  fn enter_function(
    &mut self,
    mc: &'gc Mutation<'gc>,
    mod_idx: u32,
    func_idx: u32,
    function: &Function,
    pre_bound: Vec<Gc<'gc, Accounted<'gc>>>,
  ) -> Result<(), String> {
    let start = pre_bound.len();
    let mut locals = pre_bound;
    // Initialize all remaining local slots to `Void`. `SLVal::Void` holds no
    // `Gc` pointers, so allocating it on the GC heap is cheap and safe.
    let void = self.alloc_value(mc, SLVal::Void);
    for _ in locals.len()..usize::from(function.num_locals) {
      locals.push(void);
    }
    // The parameters are "in order" on the stack, so popping will give them to
    // us in reverse order.
    for param_idx in (start..usize::from(function.num_params)).rev() {
      locals[param_idx] = self.pop()?;
    }
    self.frames.push(Frame {
      function: FrameFunc::Indexed((mod_idx, func_idx)),
      locals,
      ip: 0,
    });
    Ok(())
  }

  /// Push a new frame for an ad-hoc `Function` not registered in any
  /// `Package` (used by test entry points that build bytecode directly). The
  /// frame owns a clone of the `Function` and reads its instructions inline at
  /// `step` time. Shallow recursion only — each push clones the instruction
  /// vector.
  fn enter_inline_function(
    &mut self,
    mc: &'gc Mutation<'gc>,
    function: Function,
    pre_bound: Vec<Gc<'gc, Accounted<'gc>>>,
  ) -> Result<(), String> {
    let start = pre_bound.len();
    let mut locals = pre_bound;
    let void = self.alloc_value(mc, SLVal::Void);
    for _ in locals.len()..usize::from(function.num_locals) {
      locals.push(void);
    }
    for param_idx in (start..usize::from(function.num_params)).rev() {
      locals[param_idx] = self.pop()?;
    }
    self.frames.push(Frame {
      function: FrameFunc::Inline(function),
      locals,
      ip: 0,
    });
    Ok(())
  }
}

/// The runtime context passed to a builtin handler ([`crate::builtins::HostFn`]).
/// It carries the GC `Mutation` context and the `Package` / `Builtins`
/// registries — enough for a builtin to look up callables and allocate values.
/// The mutable `ExecRoot` reference is passed separately to the handler so the
/// borrow checker can track its lifetime independently of the `'gc` invariance
/// (see [`HostCtx::call`]).
///
/// `package` and `builtins` get their own lifetime `'a` (independent of `'gc`)
/// because they are borrows from the `Execution`, not from the arena.
pub struct HostCtx<'gc, 'a> {
  mc: &'gc Mutation<'gc>,
  package: &'a Package,
  builtins: &'a Builtins,
}

impl<'gc, 'a> HostCtx<'gc, 'a> {
  pub fn mc(&self) -> &'gc Mutation<'gc> {
    self.mc
  }

  /// Synchronously invoke a SafeLisp callable (`FunctionRef` or `Partial`)
  /// with the given arguments, returning its result. This pushes a frame for
  /// the callable, drives a sub-loop until that frame returns, and pops the
  /// result off the stack — so the builtin gets the return value before
  /// control returns to the caller's bytecode dispatch loop.
  ///
  /// `root` is passed explicitly (rather than stored in `HostCtx`) so the
  /// borrow checker can track the `&mut ExecRoot<'gc>` lifetime independently
  /// of `'gc`'s invariance.
  ///
  /// The `callable` must be a `FunctionRef` or `Partial` value; anything else
  /// is a runtime error. The `args` are pushed left-to-right (so the first arg
  /// is the callable's first parameter).
  pub(crate) fn call(
    &self,
    root: &mut ExecRoot<'gc>,
    callable: Gc<'gc, Accounted<'gc>>,
    args: &[Gc<'gc, Accounted<'gc>>],
  ) -> Result<Gc<'gc, Accounted<'gc>>, String> {
    // Remember the current frame depth so we know when the sub-call has
    // returned: we push a frame for the callable, run until the frame stack
    // is back to the original depth, then the result is on top of the stack.
    let depth_before = root.frames.len();

    // Push args left-to-right (the interpreter pops in reverse for the callee).
    for arg in args {
      root.stack.push(*arg);
    }
    // Push the callable and invoke call_dynamic with the matching arity.
    root.stack.push(callable);
    root.call_dynamic(self.mc, self.package, self.builtins, args.len() as u16)?;

    // Drive the sub-loop: step until the frame we just pushed has returned
    // (i.e. the frame stack is back to `depth_before`). Each step may push
    // further frames (nested calls), but the net effect is that when
    // `frames.len() == depth_before`, the sub-call's result is on TOS.
    while root.frames.len() > depth_before {
      root.step(self.mc, self.package, self.builtins, &mut 0)?;
    }

    // The sub-call's return value is now on top of the stack.
    root.pop()
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::builtins::{default_builtins, Builtin, Builtins};
  use crate::compiler::{self, *};
  use std::time::Duration;

  fn eval_main(source: &str) -> SLValue {
    eval_main_with(source, default_builtins())
  }

  /// Like `eval_main`, but with a custom builtin registry.
  fn eval_main_with(source: &str, builtins: Builtins) -> SLValue {
    let pkg =
      compile_executable_from_source(&source.to_string(), ("main", "main"), &builtins.specs())
        .unwrap();
    let interp = Interpreter::with_builtins(pkg, builtins);
    let mut exec = interp.call_main().unwrap();
    exec.run_until_done().unwrap()
  }

  /// Test for a simple "identity" function that returns its argument
  #[test]
  fn test_interpret_identity() {
    // id takes one argument and returns it. main calls id with 42.
    let id = compiler::Function {
      num_locals: 1,
      num_params: 1,
      instructions: vec![Instruction::LoadLocal(0), Instruction::Return],
    };
    let main = compiler::Function {
      num_locals: 0,
      num_params: 0,
      instructions: vec![
        Instruction::PushInt(42),
        Instruction::Call((0, 0), 1),
        Instruction::Return,
      ],
    };
    let pkg = Package {
      modules: vec![(
        "main".to_string(),
        vec![
          ("id".to_string(), Callable::Function(id)),
          ("main".to_string(), Callable::Function(main)),
        ],
      )],
      main: Some((0, 1)),
    };
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    assert_eq!(exec.run_until_done().unwrap(), SLValue::Int(42));
  }

  #[test]
  fn std_eq_strings() {
    assert_eq!(
      eval_main("(fn main () (std.== \"abc\" \"abc\"))"),
      SLValue::Bool(true)
    );
    assert_eq!(
      eval_main("(fn main () (std.== \"abc\" \"abd\"))"),
      SLValue::Bool(false)
    );
  }

  #[test]
  fn std_concat_strings() {
    assert_eq!(
      eval_main("(fn main () (std.concat \"foo\" \"bar\"))"),
      SLValue::String("foobar".to_string())
    );
    assert_eq!(
      eval_main("(fn main () (std.concat \"ab\" \"CDE\"))"),
      SLValue::String("abCDE".to_string())
    );
    assert_eq!(
      eval_main("(fn main () (std.concat \"\" \"x\"))"),
      SLValue::String("x".to_string())
    );
  }

  #[test]
  fn extending_builtins() {
    let builtins = Builtins::new().with_builtin(Builtin::unary("main", "add2", |a| match &a.value {
      SLVal::Int(n) => Ok(SLVal::Int(n + 2)),
      other => Err(format!("nope: {:?}", other)),
    }));
    let source = "(fn main () (add2 3))".to_string();
    let package =
      compile_executable_from_source(&source, ("main", "main"), &builtins.specs()).unwrap();

    let interpreter = Interpreter::with_builtins(package, builtins);
    let mut exec = interpreter.call_main().unwrap();
    assert_eq!(exec.run_until_done().unwrap(), SLValue::Int(5));
  }

  #[test]
  fn closure_bytecode() {
    let main = compiler::Function {
      num_locals: 1,
      num_params: 0,
      instructions: vec![
        Instruction::PushInt(42),
        Instruction::MakeCell,
        Instruction::MakeFunctionRef((0, 0)),
        Instruction::PartialApply(1),
        Instruction::Return,
      ],
    };

    let inner = compiler::Function {
      num_locals: 1,
      num_params: 0,
      instructions: vec![
        Instruction::LoadLocal(0),
        Instruction::DerefCell,
        Instruction::Return,
      ],
    };

    let pkg = Package {
      modules: vec![(
        "main".to_string(),
        vec![
          ("inner".to_string(), Callable::Function(inner)),
          ("main".to_string(), Callable::Function(main)),
        ],
      )],
      main: Some((0, 1)),
    };

    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let result = exec.run_until_done().unwrap();
    let mut exec2 = interp.call_slval(result).unwrap();
    let result2 = exec2.run_until_done().unwrap();
    assert_eq!(result2, SLValue::Int(42));
  }

  #[test]
  fn closure_end_to_end() {
    let source = "
      (fn outer ()
        (let a 1)
        (fn inner () a)
        inner
      )
      (fn main () ((outer)))
    "
    .to_string();
    let pkg =
      compile_executable_from_source(&source, ("main", "main"), &default_builtins().specs())
        .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    assert_eq!(exec.run_until_done().unwrap(), SLValue::Int(1));
  }

  #[test]
  fn nested_closure_end_to_end() {
    let source = "
      (fn outer ()
        (let a 1)
        (fn middle ()
          (fn inner () a)
          inner)
        middle)
      (fn main () (((outer))))
    ";
    assert_eq!(eval_main(source), SLValue::Int(1));
  }

  #[test]
  fn closure_captures_outer_parameter() {
    let source = "
      (fn outer (a)
        (fn inner () a)
        inner)
      (fn main () ((outer 7)))
    ";
    assert_eq!(eval_main(source), SLValue::Int(7));
  }

  #[test]
  fn closure_capture_order() {
    let source = "
      (fn outer ()
        (let a 1)
        (let b 2)
        (fn inner ()
          (let ignore b)
          a)
        inner)
      (fn main () ((outer)))
    ";
    assert_eq!(eval_main(source), SLValue::Int(1));
  }

  #[test]
  fn non_capturing_closure_end_to_end() {
    let source = "
      (fn outer ()
        (fn inner () 5)
        inner)
      (fn main () ((outer)))
    ";
    assert_eq!(eval_main(source), SLValue::Int(5));
  }

  #[test]
  fn local_nested_function_can_be_called() {
    let source = "
      (fn outer ()
        (fn inner () 5)
        (inner))
      (fn main () (outer))
    ";
    assert_eq!(eval_main(source), SLValue::Int(5));
  }

  #[test]
  fn captured_local_nested_function_can_be_called() {
    let source = "
      (fn outer ()
        (fn inner () 5)
        (fn caller () (inner))
        caller)
      (fn main () ((outer)))
    ";
    assert_eq!(eval_main(source), SLValue::Int(5));
  }

  #[test]
  fn function_definition_expression_returns_function() {
    let source = "
      (fn outer ()
        (fn inner () 5))
      (fn main () ((outer)))
    ";
    assert_eq!(eval_main(source), SLValue::Int(5));
  }

  #[test]
  fn step_advances_ip_and_leaves_value_on_stack() {
    let pkg = Package::default();
    let code = compiler::Function {
      num_locals: 0,
      num_params: 0,
      instructions: vec![Instruction::PushInt(7), Instruction::Return],
    };
    let mut exec = Execution::new(pkg, default_builtins());
    exec.enter_function(code, vec![]).unwrap();
    // After pushing the initial frame, there's one frame at ip 0.
    assert_eq!(exec.stack_len(), 0);
    exec.step().unwrap();
    // PushInt executed: stack has the value, frame still on the stack.
    assert_eq!(exec.peek_value().unwrap(), SLValue::Int(7));
    exec.step().unwrap();
    // Return popped the frame; the value remains on the stack.
    assert!(exec.is_done());
    assert_eq!(exec.peek_value().unwrap(), SLValue::Int(7));
    let result = exec.run_until_done().unwrap();
    assert_eq!(result, SLValue::Int(7));
  }

  #[test]
  fn run_until_done_pops_final_value() {
    let pkg = Package::default();
    let code = compiler::Function {
      num_locals: 0,
      num_params: 0,
      instructions: vec![Instruction::PushInt(99), Instruction::Return],
    };
    let mut exec = Execution::new(pkg, default_builtins());
    exec.enter_function(code, vec![]).unwrap();
    let result = exec.run_until_done().unwrap();
    assert_eq!(result, SLValue::Int(99));
    // The stack should be empty after run_until_done pops the final value.
    assert_eq!(exec.stack_len(), 0);
  }

  #[test]
  fn running_past_end_without_return_errors() {
    let pkg = Package::default();
    let code = compiler::Function {
      num_locals: 0,
      num_params: 0,
      instructions: vec![Instruction::PushInt(1)],
    };
    let mut exec = Execution::new(pkg, default_builtins());
    exec.enter_function(code, vec![]).unwrap();
    exec.step().unwrap(); // PushInt
    let err = exec.step().unwrap_err();
    assert!(err.contains("ran past end"), "unexpected error: {}", err);
  }

  #[test]
  fn step_with_no_frames_errors() {
    let pkg = Package::default();
    let mut exec = Execution::new(pkg, default_builtins());
    let err = exec.step().unwrap_err();
    assert!(err.contains("no frames"), "unexpected error: {}", err);
  }

  #[test]
  fn call_dynamic_on_non_callable_errors() {
    let pkg = Package::default();
    let mut exec = Execution::new(pkg, default_builtins());
    let err = exec
      .push_and_call_dynamic(SLValue::Int(3))
      .expect_err("expected an error calling a non-callable");
    assert!(err.contains("non-callable"), "unexpected error: {}", err);
  }

  #[test]
  fn deref_cell_on_non_cell_errors() {
    let pkg = Package::default();
    let code = compiler::Function {
      num_locals: 0,
      num_params: 0,
      instructions: vec![Instruction::PushInt(1), Instruction::DerefCell],
    };
    let mut exec = Execution::new(pkg, default_builtins());
    exec.enter_function(code, vec![]).unwrap();
    exec.step().unwrap(); // PushInt
    let err = exec.step().unwrap_err(); // DerefCell
    assert!(err.contains("Not a cell"), "unexpected error: {}", err);
  }

  #[test]
  fn call_to_missing_module_errors() {
    let pkg = Package::default();
    let code = compiler::Function {
      num_locals: 0,
      num_params: 0,
      instructions: vec![Instruction::Call((0, 0), 0)],
    };
    let mut exec = Execution::new(pkg, default_builtins());
    exec.enter_function(code, vec![]).unwrap();
    let err = exec.step().unwrap_err();
    assert!(
      err.contains("Module not found"),
      "unexpected error: {}",
      err
    );
  }

  /// A dummy variadic builtin used to exercise the variadic-call mechanism:
  /// it returns the number of arguments it received as an `Int`.
  fn varargs_builtins() -> Builtins {
    default_builtins().with_builtin(Builtin::variadic("std", "varargs", |args| {
      Ok(SLVal::Int(args.len() as i64))
    }))
  }

  #[test]
  fn variadic_builtin_called_with_zero_args() {
    let builtins = varargs_builtins();
    assert_eq!(
      eval_main_with("(fn main () (std.varargs))", builtins),
      SLValue::Int(0)
    );
  }

  #[test]
  fn variadic_builtin_called_with_multiple_args() {
    let builtins = varargs_builtins();
    assert_eq!(
      eval_main_with("(fn main () (std.varargs 1 2 3))", builtins),
      SLValue::Int(3)
    );
  }

  #[test]
  fn variadic_builtin_called_with_one_arg() {
    let builtins = varargs_builtins();
    assert_eq!(
      eval_main_with("(fn main () (std.varargs 7))", builtins),
      SLValue::Int(1)
    );
  }

  #[test]
  fn fixed_arity_builtin_rejects_wrong_arg_count() {
    // `+` is a fixed binary builtin; calling it with one arg should now be
    // caught at the call site via the carried arity.
    let src = "(fn main () (std.+ 1))";
    let pkg = compile_executable_from_source(
      &src.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let err = exec.run_until_done().unwrap_err();
    assert!(
      err.contains("expects 2 arg(s) but was called with 1"),
      "unexpected error: {}",
      err
    );
  }

  #[test]
  fn partial_apply_then_call_dynamic_binds_pre_bound_args() {
    // inner takes one param and returns it. main builds a Partial with the
    // arg pre-bound, then CallDynamic's it with no extra args on the stack.
    let inner = compiler::Function {
      num_locals: 1,
      num_params: 1,
      instructions: vec![Instruction::LoadLocal(0), Instruction::Return],
    };
    let main = compiler::Function {
      num_locals: 0,
      num_params: 0,
      instructions: vec![
        Instruction::PushInt(42),
        Instruction::MakeFunctionRef((0, 0)),
        Instruction::PartialApply(1),
        Instruction::CallDynamic(0),
        Instruction::Return,
      ],
    };
    let pkg = Package {
      modules: vec![(
        "main".to_string(),
        vec![
          ("inner".to_string(), Callable::Function(inner)),
          ("main".to_string(), Callable::Function(main)),
        ],
      )],
      main: Some((0, 1)),
    };
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    assert_eq!(exec.run_until_done().unwrap(), SLValue::Int(42));
  }

  #[test]
  fn function_call_leaves_no_stack_garbage() {
    // After main returns, the execution's value stack must contain exactly
    // one value: main's return value. Intermediate let-bindings and function
    // call results should not accumulate as garbage below it.
    let source = "
      (fn id (a) a)
      (fn waste ()
        (let a 1)
        (id 1)
        (let b 2)
        b)
      (fn main () (waste) (waste) (waste))
    ";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let (mod_idx, fn_idx) = pkg.main.unwrap();
    let function = pkg.get_function(mod_idx, fn_idx).unwrap().clone();
    if let Callable::Function(function) = function {
      let mut exec = Execution::new(pkg, default_builtins());
      exec.enter_function(function, vec![]).unwrap();
      let result = exec.run_until_done().unwrap();
      assert_eq!(result, SLValue::Int(2));
      // run_until_done pops the final value, so the stack should be empty.
      assert_eq!(exec.stack_len(), 0);
    }
  }

  #[test]
  fn if_does_not_evaluate_unselected_branch() {
    // If the else branch were evaluated, calling the unimplemented
    // `boom` builtin would error at runtime. Since the then branch is selected,
    // `boom` is never called and the program returns 42 cleanly. `boom` is
    // registered as a builtin (with an erroring handler) so the call links,
    // but its behavior is never invoked.
    let source = "(fn main () (if (std.== 1 1) 42 (boom 0)))";
    let builtins =
      default_builtins().with_builtin(Builtin::unary("main", "boom", |_| Err("boom".into())));
    let pkg = compile_executable_from_source(source, ("main", "main"), &builtins.specs()).unwrap();
    let interp = Interpreter::with_builtins(pkg, builtins);
    let mut exec = interp.call_main().unwrap();
    assert_eq!(exec.run_until_done().unwrap(), SLValue::Int(42));
  }

  #[test]
  fn if_rejects_non_bool_condition() {
    let source = "(fn main () (if 1 42 0))";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let err = exec.run_until_done().unwrap_err();
    assert_eq!(err, "`if` condition must be a bool, got Int(1)");
  }

  #[test]
  fn run_completes_in_one_call() {
    let source = "(fn main () 5)";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let status = exec.run(1_000).unwrap();
    assert_eq!(status, Status::Done(SLValue::Int(5)));
  }

  #[test]
  fn run_pauses_and_resumes() {
    let source = "
      (fn main ()
        (let a 1) (let b 2) (let c 3) (let d 4) 5)";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let status = exec.run(3).unwrap();
    assert_eq!(status, Status::Paused);
    assert_eq!(exec.executed, 3);
    assert!(!exec.is_done());
    let status = exec.run(1_000).unwrap();
    assert_eq!(status, Status::Done(SLValue::Int(5)));
  }

  #[test]
  fn run_hits_limit_exactly() {
    // (std.+ 1 2) is 4 bytecodes: PushInt(1), PushInt(2), Call(std.+), Return.
    let source = "(fn main () (std.+ 1 2))";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let status = exec.run(4).unwrap();
    assert_eq!(status, Status::Done(SLValue::Int(3)));
    assert_eq!(exec.executed, 4);
  }

  #[test]
  fn run_limited_mid_program_pauses() {
    let source = "(fn main () (std.+ 1 2))";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let status = exec.run(2).unwrap();
    assert_eq!(status, Status::Paused);
    let status = exec.run(1_000).unwrap();
    assert_eq!(status, Status::Done(SLValue::Int(3)));
    assert_eq!(exec.executed, 4);
  }

  #[test]
  fn infinite_recursion_is_bounded_by_run_budget() {
    let source = "(fn loop (n) (loop n)) (fn main () (loop 1))";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let status = exec.run(10_000).unwrap();
    assert_eq!(status, Status::Paused);
    assert!(!exec.is_done());
  }

  #[test]
  fn executed_count_is_cumulative() {
    let source = "(fn main () (let a 1) (let b 2) (let c 3) 4)";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    exec.run(2).unwrap();
    assert_eq!(exec.executed, 2);
    exec.run(2).unwrap();
    assert_eq!(exec.executed, 4);
    let s3 = exec.run(1_000).unwrap();
    assert!(matches!(s3, Status::Done(_)));
    assert_eq!(exec.executed, 14);
  }

  #[test]
  fn two_executions_run_in_parallel() {
    // Two `Execution`s from one `Interpreter` should be independent: stepping
    // one does not affect the other's stack, frames, or counter. We interleave
    // `run` calls (cooperative scheduling) and confirm each produces its own
    // result. Each `main` returns its distinct constant so we can tell them
    // apart.
    let source = "(fn main () (let a 1) (let b 2) 3)";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec_a = interp.call_main().unwrap();
    let mut exec_b = interp.call_main().unwrap();

    // Run a couple of bytecodes on each, alternating, before either is done.
    assert_eq!(exec_a.run(1).unwrap(), Status::Paused);
    assert_eq!(exec_b.run(1).unwrap(), Status::Paused);
    assert_eq!(exec_a.executed, 1);
    assert_eq!(exec_b.executed, 1);

    // Run both to completion in the same interleaved fashion.
    assert_eq!(exec_a.run(1_000).unwrap(), Status::Done(SLValue::Int(3)));
    assert_eq!(exec_b.run(1_000).unwrap(), Status::Done(SLValue::Int(3)));
    assert!(exec_a.is_done());
    assert!(exec_b.is_done());
  }

  #[test]
  fn run_for_duration_completes() {
    // A trivial program completes well within a generous duration.
    let source = "(fn main () 5)";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let status = exec.run_for_duration(Duration::from_secs(1)).unwrap();
    assert_eq!(status, Status::Done(SLValue::Int(5)));
  }

  #[test]
  fn run_for_duration_pauses_on_infinite_recursion() {
    // A non-terminating program is bounded by the time budget and pauses.
    let source = "(fn loop (n) (loop n)) (fn main () (loop 1))";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let status = exec.run_for_duration(Duration::from_millis(50)).unwrap();
    assert_eq!(status, Status::Paused);
    assert!(!exec.is_done());
    // It should have made progress.
    assert!(exec.executed > 0);
  }

  #[test]
  fn collect_all_frees_unreachable_values() {
    // A program that creates garbage: `main` builds a Cell holding 42, then
    // discards it by returning `99`. After execution, the Cell (and its
    // contents) should be unreachable from the root. Forcing a full
    // collection cycle should reclaim those allocations.
    //
    // We construct this with bytecode directly because there's no surface
    // syntax for `cell` outside the closure transform.
    let main = compiler::Function {
      num_locals: 1,
      num_params: 0,
      instructions: vec![
        Instruction::PushInt(42),
        Instruction::MakeCell,    // allocate a Cell wrapping 42
        Instruction::SetLocal(0), // bind it to local 0 ("garbage")
        Instruction::PushInt(99), // push the real return value
        Instruction::Return,      // return 99; the Cell in local 0 is now dead
      ],
    };
    let pkg = Package {
      modules: vec![(
        "main".to_string(),
        vec![("main".to_string(), Callable::Function(main))],
      )],
      main: Some((0, 0)),
    };
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();

    // `call_main` enters the function, which allocates locals in the GC. Record
    // the baseline allocation count before running.
    let baseline = exec.gc_count();
    assert_eq!(baseline, 1); // the single Void local for `main`

    // Step through the program manually so we can observe allocations before
    // the final value is popped (which would make it unreachable). The program
    // is 5 instructions; stepping all 5 leaves the int `99` on the stack and
    // the Cell dead in local 0.
    for _ in 0..5 {
      exec.step().unwrap();
    }
    assert!(exec.is_done());
    assert_eq!(exec.peek_value().unwrap(), SLValue::Int(99));

    // After running, the Cell is unreachable but may not yet have been
    // collected (incremental collection difficult to predict because of the
    // "debt" system). There should be at least one live `Gc` allocation right
    // now (the dangling Cell + contents), plus the int `99` on the stack.
    let after_run = exec.gc_count();
    assert!(
      after_run >= 3,
      "expected at least 3 live Gc allocations (cell + inner value + result) after run, got {}",
      after_run,
    );

    // Forcing a full collection cycle must reclaim the unreachable Cell. The
    // int `99` on the stack is still reachable and survives.
    exec.collect_all();
    let after_collect = exec.gc_count();
    assert_eq!(
      after_collect, 1,
      "expected 1 live Gc allocation (the result 99) after collect_all, got {}",
      after_collect,
    );
  }

  #[test]
  fn gc_reclaims_intermediate_values_during_long_run() {
    // A program that produces a lot of garbage: it loops `n` times, each
    // iteration building a Cell and immediately discarding it. If the GC is
    // working, the live allocation count after the run should stay bounded
    // (proportional to the final stack, not to the number of iterations),
    // rather than growing linearly with `n`.
    //
    // loop(n): if n == 0 return 0; else let g = cell(n); loop(n-1)
    //   locals: n (0), g (1)
    //   0: LoadLocal(0)        # n
    //   1: PushInt(0)
    //   2: Call((std_mod, std_eq))  # n == 0
    //   3: JumpIfFalse(+2)      # if n != 0, skip the then-branch (offset 2)
    //   4: PushInt(0)
    //   5: Return
    //   6: LoadLocal(0)        # n
    //   7: MakeCell             # cell(n)  -- this is the garbage
    //   8: SetLocal(1)          # g = cell(n)
    //   9: LoadLocal(0)         # n
    //  10: PushInt(1)
    //  11: Call((std_mod, std_sub)) # n - 1
    //  12: Call((0, 0))         # loop(n-1)  -- tail-ish recursion
    //  13: Return
    let std_mod = 1u32; // "std" is the second module after "main"
    let std_eq = 2u32;
    let std_sub = 1u32;
    let waste = compiler::Function {
      num_locals: 2,
      num_params: 1,
      instructions: vec![
        Instruction::LoadLocal(0),
        Instruction::PushInt(0),
        Instruction::Call((std_mod, std_eq), 2),
        Instruction::JumpIfFalse(2),
        Instruction::PushInt(0),
        Instruction::Return,
        Instruction::LoadLocal(0),
        Instruction::MakeCell,
        Instruction::SetLocal(1),
        Instruction::LoadLocal(0),
        Instruction::PushInt(1),
        Instruction::Call((std_mod, std_sub), 2),
        Instruction::Call((0, 0), 1),
        Instruction::Return,
      ],
    };
    let main = compiler::Function {
      num_locals: 0,
      num_params: 0,
      instructions: vec![
        Instruction::PushInt(1000),
        Instruction::Call((0, 0), 1), // waste(1000)
        Instruction::Return,
      ],
    };
    // Build a package with std builtins and the two functions above.
    let pkg = Package {
      modules: vec![
        (
          "main".to_string(),
          vec![
            ("waste".to_string(), Callable::Function(waste)),
            ("main".to_string(), Callable::Function(main)),
          ],
        ),
        (
          "std".to_string(),
          vec![
            ("+".to_string(), Callable::Builtin),
            ("-".to_string(), Callable::Builtin),
            ("==".to_string(), Callable::Builtin),
          ],
        ),
      ],
      main: Some((0, 1)),
    };
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();

    let result = exec.run_until_done().unwrap();
    assert_eq!(result, SLValue::Int(0));

    // After 1000 iterations each allocating a Cell, if GC never ran we'd
    // expect ~1000+ live allocations. Because `run_until_done` calls
    // `collect_debt` and the program allocates enough to trigger incremental
    // collection along the way, the count should be far smaller than 1000.
    // Forcing a final collection reclaims whatever transient garbage remains,
    // leaving only the final stack value.
    exec.collect_all();
    let live = exec.gc_count();
    assert!(
      live < 100,
      "expected bounded live Gc count after collecting garbage from 1000 iterations, got {}",
      live,
    );
  }

  #[test]
  fn live_values_survive_collection() {
    // A program that returns a Cell: the Cell must survive a forced
    // collection, proving that reachable `Gc` pointers are not reclaimed.
    let main = compiler::Function {
      num_locals: 0,
      num_params: 0,
      instructions: vec![
        Instruction::PushInt(3),
        Instruction::MakeCell, // cell(3)
        Instruction::Return,
      ],
    };
    let pkg = Package {
      modules: vec![(
        "main".to_string(),
        vec![("main".to_string(), Callable::Function(main))],
      )],
      main: Some((0, 0)),
    };
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();

    // Step through all 3 instructions. The Cell ends up on the stack as the
    // result (is_done() is true, but we haven't popped it).
    for _ in 0..3 {
      exec.step().unwrap();
    }
    assert!(exec.is_done());
    assert!(matches!(exec.peek_value().unwrap(), SLValue::Cell(_)));

    // The Cell on the stack is reachable and must survive a full collection.
    // (There may also be a now-dead Void local from `enter_function`; that one
    // is expected to be reclaimed.)
    exec.collect_all();
    let after = exec.gc_count();
    assert!(
      after >= 2,
      "expected the Cell + its inner value to survive collection, got {} live allocations",
      after,
    );

    // And the value must still be readable after collection.
    assert!(matches!(exec.peek_value().unwrap(), SLValue::Cell(_)));
  }

  #[test]
  fn memory_limit_defaults_to_none() {
    let pkg = Package::default();
    let mut exec = Execution::new(pkg, default_builtins());
    assert!(exec.memory_limit().is_none());
    assert_eq!(exec.memory_usage(), exec.gc_allocation_bytes());
    exec.set_memory_limit(Some(123));
    assert_eq!(exec.memory_limit(), Some(123));
    exec.set_memory_limit(None);
    assert!(exec.memory_limit().is_none());
  }

  #[test]
  fn memory_limit_errors_when_exceeded() {
    // Allocate one Cell wrapping an Int. With a limit lower than the resulting
    // live GC heap, the very first `step` (which allocates the void local on
    // `enter_function`) or a later step should error before completing.
    let main = compiler::Function {
      num_locals: 1,
      num_params: 0,
      instructions: vec![
        Instruction::PushInt(42),
        Instruction::MakeCell,
        Instruction::SetLocal(0),
        Instruction::PushInt(99),
        Instruction::Return,
      ],
    };
    let pkg = Package {
      modules: vec![(
        "main".to_string(),
        vec![("main".to_string(), Callable::Function(main))],
      )],
      main: Some((0, 0)),
    };
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    // 1 byte is far below any allocation; the first step after entering main
    // (which has allocated a Void local in the GC) must immediately trip the
    // limit.
    exec.set_memory_limit(Some(1));
    let err = exec.run_until_done().unwrap_err();
    assert!(
      err.contains("memory limit exceeded"),
      "unexpected error: {}",
      err
    );
  }

  #[test]
  fn memory_limit_allows_small_program() {
    // A trivial program that only pushes an int and returns should fit well
    // under a generous limit and complete normally.
    let source = "(fn main () 5)";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    exec.set_memory_limit(Some(1024 * 1024));
    let result = exec.run_until_done().unwrap();
    assert_eq!(result, SLValue::Int(5));
  }

  #[test]
  fn memory_limit_triggers_during_unbounded_allocation() {
    // A program that builds ever-growing lists: `loop(n)` returns a list of `n`
    // ints by repeatedly `push`ing onto an accumulator. With a tight memory
    // limit, the run must error instead of running to completion.
    let source = "
      (fn loop (n acc)
        (if (std.== n 0)
          acc
          (loop (std.- n 1) (std.push acc n))))
      (fn main () (loop 100000 (std.list)))
    ";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    exec.set_memory_limit(Some(64 * 1024));
    let err = exec.run_until_done().unwrap_err();
    assert!(
      err.contains("memory limit exceeded"),
      "unexpected error: {}",
      err
    );
  }

  #[test]
  fn memory_limit_catches_unbounded_stack_and_frames() {
    // A deliberately adversarial program: `spin` recurses forever, taking no
    // arguments and allocating nothing on the GC heap. Every recursive call
    // pushes a new `Frame` — and each `Frame` clones the function's
    // `instructions: Vec<…>` and allocates a fresh `locals: Vec<Gc<_>>` (even
    // if empty, the `Vec` header / capacity lives on `ExecRoot`'s heap) — and
    // grows the value stack by one slot for the pending call result. These are
    // *plain Rust `Vec` allocations owned by `ExecRoot`*, not `Gc` allocations,
    // so they don't show up in `Metrics::total_gc_allocation`.
    //
    // The live GC heap stays at essentially zero (no `Gc::new` runs inside
    // `spin`), so the current GC-only memory limit never fires — even though
    // 200 000 piled-up frames consume ~24 MB of real Rust heap. A *correct*
    // memory limit would trip. This test asserts that the run errors with
    // "memory limit exceeded"; it currently FAILS because the limit is never
    // reached. Once stack/frame overhead is tracked, the error will fire and
    // the test will pass.
    let source = "
      (fn spin () (spin))
      (fn main () (spin))
    ";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    // 64 KiB is far below what 200 000 cloned instruction vectors + frame
    // slots actually consume (~24 MB), yet comfortably above this program's
    // live GC heap at any moment.
    exec.set_memory_limit(Some(64 * 1024));
    // Drive execution in small batches so `collect_debt` reclaims the per-frame
    // `Gc<Void>` garbage and the live GC heap stays bounded — while the frame
    // objects (cloning `instructions: Vec<Instruction>`) pile up unboundedly
    // on the Rust heap. The loop expects one of the `run` calls to error with
    // "memory limit exceeded"; today none does, so the loop completes and the
    // final `unwrap_err` / unreachable panic fails the test.
    let mut hit = false;
    for _ in 0..2_000 {
      match exec.run(100) {
        Ok(_) if exec.is_done() => panic!("spin terminated unexpectedly"),
        Ok(_) => {}
        Err(e) => {
          assert!(
            e.contains("memory limit exceeded"),
            "unexpected error: {}",
            e,
          );
          hit = true;
          break;
        }
      }
    }
    assert!(
      hit,
      "expected the memory limit to fire, but the run completed with only {} bytes of live GC (limit {}); \
       the GC-only limit does not see the ~200 000 piled-up frames' Rust-heap overhead",
      exec.gc_allocation_bytes(),
      64 * 1024,
    );
  }

  #[test]
  fn memory_limit_catches_large_string_contents() {
    // `SLVal::String(String)` stores the `String` inline inside the `Gc`-boxed
    // `SLVal`. The GC arena only counts the box itself (the `SLVal` enum +
    // `String`'s 24-byte ptr/len/cap header, ≈ 40 bytes) toward
    // `total_gc_allocation`. The actual string *bytes* live on the ordinary
    // Rust heap and are invisible to the GC metrics — they're freed when the
    // `Gc` box is dropped, but never counted.
    //
    // This program doubles a string on each recursive call: after 25
    // iterations the string is 2^25 = 32 MB of actual bytes, but the live GC
    // heap stays at a handful of ~40-byte `Gc<SLVal::String>` boxes (the old
    // ones become garbage and are reclaimed by `collect_debt` between
    // batches). A *correct* memory limit would trip; the current GC-only
    // limit never does. This test asserts the limit fires and currently
    // FAILS — once string-contents (or any non-Gc heap) overhead is tracked,
    // it will pass.
    let source = "
      (fn grow (s n)
        (if (std.== n 0)
          s
          (grow (std.concat s s) (std.- n 1))))
      (fn main () (grow \"x\" 25))
    ";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    // 64 KiB is far below the 32 MB string this program builds, yet
    // comfortably above the handful of `Gc<SLVal::String>` boxes live at any
    // one moment.
    exec.set_memory_limit(Some(64 * 1024));
    // Drive in small batches so `collect_debt` reclaims dead string boxes and
    // the live GC heap stays tiny — while the actual string bytes grow to
    // 32 MB on the Rust heap, unseen by the limit.
    let mut hit = false;
    for _ in 0..10_000 {
      match exec.run(50) {
        Ok(Status::Done(_)) => break,
        Ok(_) => {}
        Err(e) => {
          assert!(
            e.contains("memory limit exceeded"),
            "unexpected error: {}",
            e,
          );
          hit = true;
          break;
        }
      }
    }
    assert!(
      hit,
      "expected the memory limit to fire, but the run completed with only {} bytes of live GC (limit {}); \
       the GC-only limit does not see the ~32 MB of string contents on the Rust heap",
      exec.gc_allocation_bytes(),
      64 * 1024,
    );
  }

  #[test]
  fn set_cell_mutates_captured_variable() {
    // The classic counter: `counter` returns an `inc` closure that mutates
    // and returns a captured `count` variable. Each call to `inc` should
    // increment the shared counter, proving that `set!` writes through the
    // Cell that the closure and its parent share.
    let source = "
      (fn counter ()
        (let count 0)
        (fn inc ()
          (set! count (std.+ 1 count))
          count)
        inc)
      (fn main ()
        (let c (counter))
        (c)
        (c)
        (c))
    ";
    assert_eq!(eval_main(source), SLValue::Int(3));
  }

  #[test]
  fn set_cell_returns_new_value() {
    // `set!` should evaluate to the new value, not the old one. We use a
    // closure to get a Cell (captured variables are wrapped in Cells by the
    // closure transform).
    let source = "
      (fn make ()
        (let x 1)
        (fn inc ()
          (set! x (std.+ x 10))))
      (fn main () ((make)))
    ";
    assert_eq!(eval_main(source), SLValue::Int(11));
  }

  #[test]
  fn set_cell_on_non_cell_errors() {
    // `set!` on a local that isn't a Cell (not captured by any closure, so
    // the closure transform doesn't wrap it in a Cell) should error at runtime.
    let source = "
      (fn main ()
        (let x 1)
        (set! x 2))
    ";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let err = exec.run_until_done().unwrap_err();
    assert!(err.contains("Not a cell"), "unexpected error: {}", err);
  }

  #[test]
  fn cycle_is_collected() {
    // Build a cycle using only source-level constructs: a closure that
    // captures a variable, then `set!` that variable to the closure itself.
    // After the closure transform, `c` is a Cell holding the Partial, and the
    // Partial holds the Cell → Cell↔Partial cycle. Once the result is
    // discarded, the cycle is unreachable and must be collected. This is the
    // key advantage of gc-arena over `Rc` (which would leak cycles).
    //
    // (fn make-cycle ()
    //   (let c 0)          ; c becomes a Cell (captured by self)
    //   (fn self () c)     ; self captures c's Cell
    //   (set! c self)      ; write self into c's Cell → cycle
    //   self)              ; return the cyclic closure
    // (fn main () (make-cycle) 99)  ; discard the cycle, return 99
    let source = "
      (fn make-cycle ()
        (let c 0)
        (fn self () c)
        (set! c self)
        self)
      (fn main () (make-cycle) 99)
    ";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();

    // Run to completion: main calls make-cycle (which builds the cycle and
    // returns it on the stack), then main discards it by returning 99.
    let result = exec.run_until_done().unwrap();
    assert_eq!(result, SLValue::Int(99));

    // At this point the stack is empty (run_until_done popped 99), so the
    // cycle is unreachable. Forcing a full collection must reclaim it.
    exec.collect_all();
    let after = exec.gc_count();
    assert_eq!(
      after, 0,
      "expected 0 live Gc allocations after collecting the cycle, got {}",
      after,
    );
  }

  #[test]
  fn list_self_reference_cycle_is_collected() {
    // A list that contains a cell which in turn refers back to the same list
    // forms a cycle (List ↔ Cell). This is the list analogue of
    // `cycle_is_collected` (which builds a Partial↔Cell cycle via closures).
    let source = "
      (fn make-cycle ()
        (let l (std.list))
        (fn mkcycle () (set! l (std.list l)))
        mkcycle)
      (fn main () ((make-cycle)))
    ";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();

    let result = exec.run_until_done().unwrap();
    // `main` returns the cyclic list (a 1-element list containing itself),
    // which `run_until_done` pops and hands back to us. The cycle is now only
    // reachable through this returned `SLValue`-side copy — but the in-arena
    // cycle was held alive by `make-cycle`'s frame, which is gone.
    assert!(matches!(result, SLValue::List(_)));

    // Forcing a full collection must reclaim the in-arena cycle, leaving zero
    // live allocations (the popped result is an owned `SLValue`, not a `Gc`).
    exec.collect_all();
    let after = exec.gc_count();
    assert_eq!(
      after, 0,
      "expected 0 live Gc allocations after collecting the List<->Cell cycle, got {}",
      after,
    );
  }

  #[test]
  fn list_empty() {
    assert_eq!(eval_main("(fn main () (std.list))"), SLValue::List(vec![]));
  }

  #[test]
  fn list_with_ints() {
    assert_eq!(
      eval_main("(fn main () (std.list 1 2 3))"),
      SLValue::List(vec![SLValue::Int(1), SLValue::Int(2), SLValue::Int(3)])
    );
  }

  #[test]
  fn list_first_class_as_callable_value() {
    // `list` is a builtin (not a special form): its FunctionRef can be pushed
    // onto the stack and invoked via CallDynamic, exactly as a future `(let l
    // std.list)` binding would compile to once the compiler supports
    // referencing builtins as values.
    let std_mod = 1u32; // "std" is the second module after "main"
                        // `list` is registered after `+`, `-`, `==`, `concat`, so its function
                        // index in the `std` module is 4.
    let std_list = 4u32;
    let main = compiler::Function {
      num_locals: 0,
      num_params: 0,
      instructions: vec![
        Instruction::PushInt(1),
        Instruction::PushInt(2),
        Instruction::PushInt(3),
        Instruction::MakeFunctionRef((std_mod, std_list)),
        Instruction::CallDynamic(3),
        Instruction::Return,
      ],
    };
    let pkg = Package {
      modules: vec![
        (
          "main".to_string(),
          vec![("main".to_string(), Callable::Function(main))],
        ),
        (
          "std".to_string(),
          vec![
            ("+".to_string(), Callable::Builtin),
            ("-".to_string(), Callable::Builtin),
            ("==".to_string(), Callable::Builtin),
            ("concat".to_string(), Callable::Builtin),
            ("list".to_string(), Callable::Builtin),
          ],
        ),
      ],
      main: Some((0, 0)),
    };
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    assert_eq!(
      exec.run_until_done().unwrap(),
      SLValue::List(vec![SLValue::Int(1), SLValue::Int(2), SLValue::Int(3)])
    );
  }

  #[test]
  fn concat_empty_lists() {
    assert_eq!(
      eval_main("(fn main () (std.concat (std.list) (std.list)))"),
      SLValue::List(vec![])
    );
  }

  #[test]
  fn concat_lists() {
    assert_eq!(
      eval_main("(fn main () (std.concat (std.list 1 2) (std.list 3 4)))"),
      SLValue::List(vec![
        SLValue::Int(1),
        SLValue::Int(2),
        SLValue::Int(3),
        SLValue::Int(4)
      ])
    );
  }

  #[test]
  fn concat_list_mismatch_errors() {
    let err = eval_main_err("(fn main () (std.concat (std.list 1) \"x\"))");
    assert!(err.contains("Couldn't concat"), "unexpected error: {}", err);
  }

  #[test]
  fn list_equality() {
    assert_eq!(
      eval_main("(fn main () (std.== (std.list 1 2 3) (std.list 1 2 3)))"),
      SLValue::Bool(true)
    );
    assert_eq!(
      eval_main("(fn main () (std.== (std.list 1 2 3) (std.list 1 2 4)))"),
      SLValue::Bool(false)
    );
    assert_eq!(
      eval_main("(fn main () (std.== (std.list 1) (std.list 1 2)))"),
      SLValue::Bool(false)
    );
  }

  #[test]
  fn idx_list_positive() {
    assert_eq!(
      eval_main("(fn main () (std.idx (std.list 10 20 30) 0))"),
      SLValue::Int(10)
    );
    assert_eq!(
      eval_main("(fn main () (std.idx (std.list 10 20 30) 2))"),
      SLValue::Int(30)
    );
  }

  #[test]
  fn idx_list_negative() {
    assert_eq!(
      eval_main("(fn main () (std.idx (std.list 10 20 30) -1))"),
      SLValue::Int(30)
    );
    assert_eq!(
      eval_main("(fn main () (std.idx (std.list 10 20 30) -3))"),
      SLValue::Int(10)
    );
  }

  #[test]
  fn idx_list_out_of_range_errors() {
    let err = eval_main_err("(fn main () (std.idx (std.list 1 2) 5))");
    assert!(err.contains("out of range"), "unexpected error: {}", err);
    let err = eval_main_err("(fn main () (std.idx (std.list 1 2) -3))");
    assert!(err.contains("out of range"), "unexpected error: {}", err);
  }

  #[test]
  fn idx_string() {
    assert_eq!(
      eval_main("(fn main () (std.idx \"hello\" 0))"),
      SLValue::String("h".to_string())
    );
    assert_eq!(
      eval_main("(fn main () (std.idx \"hello\" 4))"),
      SLValue::String("o".to_string())
    );
    assert_eq!(
      eval_main("(fn main () (std.idx \"hello\" -1))"),
      SLValue::String("o".to_string())
    );
  }

  #[test]
  fn idx_string_out_of_range_errors() {
    let err = eval_main_err("(fn main () (std.idx \"hi\" 5))");
    assert!(err.contains("out of range"), "unexpected error: {}", err);
  }

  #[test]
  fn push_returns_new_list() {
    assert_eq!(
      eval_main("(fn main () (std.push (std.list 1 2) 3))"),
      SLValue::List(vec![SLValue::Int(1), SLValue::Int(2), SLValue::Int(3)])
    );
    // empty + one
    assert_eq!(
      eval_main("(fn main () (std.push (std.list) 7))"),
      SLValue::List(vec![SLValue::Int(7)])
    );
  }

  #[test]
  fn push_is_non_mutating() {
    // The original list is unaffected: indexing it afterwards still works.
    let source = "(fn main () (let l (std.list 1 2)) (std.push l 3) (std.idx l 1))";
    assert_eq!(eval_main(source), SLValue::Int(2));
  }

  #[test]
  fn slice_list_basic() {
    assert_eq!(
      eval_main("(fn main () (std.slice (std.list 1 2 3 4 5) 1 4))"),
      SLValue::List(vec![SLValue::Int(2), SLValue::Int(3), SLValue::Int(4)])
    );
  }

  #[test]
  fn slice_list_negative_indices() {
    assert_eq!(
      eval_main("(fn main () (std.slice (std.list 1 2 3 4 5) -3 -1))"),
      SLValue::List(vec![SLValue::Int(3), SLValue::Int(4)])
    );
  }

  #[test]
  fn slice_list_clamped_bounds() {
    // stop past the end clamps to length.
    assert_eq!(
      eval_main("(fn main () (std.slice (std.list 1 2 3) 1 100))"),
      SLValue::List(vec![SLValue::Int(2), SLValue::Int(3)])
    );
    // start before 0 clamps to 0.
    assert_eq!(
      eval_main("(fn main () (std.slice (std.list 1 2 3) -100 2))"),
      SLValue::List(vec![SLValue::Int(1), SLValue::Int(2)])
    );
  }

  #[test]
  fn slice_list_empty_result() {
    assert_eq!(
      eval_main("(fn main () (std.slice (std.list 1 2 3) 2 2))"),
      SLValue::List(vec![])
    );
    // start > stop yields empty.
    assert_eq!(
      eval_main("(fn main () (std.slice (std.list 1 2 3) 3 0))"),
      SLValue::List(vec![])
    );
  }

  #[test]
  fn slice_string() {
    assert_eq!(
      eval_main("(fn main () (std.slice \"hello world\" 0 5))"),
      SLValue::String("hello".to_string())
    );
    assert_eq!(
      eval_main("(fn main () (std.slice \"hello\" -2 100))"),
      SLValue::String("lo".to_string())
    );
  }

  #[test]
  fn recursive_sum_of_list() {
    // Recursion over a list using `len` for the base case, `idx` for the head,
    // and `slice` for the tail. `(std.slice l 1 (std.len l))` yields everything
    // from index 1 to the end.
    let source = "
      (fn sum (l)
        (if (std.== (std.len l) 0)
          0
          (std.+ (std.idx l 0) (sum (std.slice l 1 (std.len l))))))
      (fn main () (sum (std.list 1 2 3 4 5)))
    ";
    assert_eq!(eval_main(source), SLValue::Int(15));
  }

  /// Helper: evaluate `main` and return the error string (panics if no error).
  fn eval_main_err(source: &str) -> String {
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    exec.run_until_done().unwrap_err()
  }

  #[test]
  fn block_returns_value_of_last_expression() {
    assert_eq!(eval_main("(fn main () (block 1 2 3))"), SLValue::Int(3));
  }

  #[test]
  fn block_discards_non_final_values() {
    // The `let` inside the block is for a side effect; its value is discarded
    // and the block returns the trailing literal.
    assert_eq!(
      eval_main("(fn main () (block (let a 1) (let b 2) 3))"),
      SLValue::Int(3)
    );
  }

  #[test]
  fn block_in_if_else_branch() {
    // The else branch uses `block` to sequence two expressions; only the last
    // is returned as the if's (and main's) value.
    assert_eq!(
      eval_main("(fn main () (if false 0 (block (let a 1) 42)))"),
      SLValue::Int(42)
    );
  }

  #[test]
  fn block_let_visible_after_in_same_function() {
    // A `let` introduced inside a block introduces a local that remains visible
    // to later expressions in the enclosing function body (the compiler's
    // `locals` map is shared across the block and its enclosing scope).
    assert_eq!(
      eval_main("(fn main () (block (let a 7) a) (std.+ a 1))"),
      SLValue::Int(8)
    );
  }

  #[test]
  fn block_empty_is_a_parse_error() {
    // The error surfaces at compile time, so `eval_main_err` (which unwraps
    // compilation) would panic; instead, drive the parser directly.
    let err = crate::parser::read_multiple("(fn main () (block))").unwrap_err();
    assert!(
      err.contains("`block` must have at least one expression"),
      "got: {}",
      err
    );
  }
}
