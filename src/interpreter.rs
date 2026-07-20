use std::cell::Cell;
use std::rc::Rc;
use std::time::{Duration, Instant};

use gc_arena::{collect::Collect, Arena, Gc, Mutation, RefLock, Rootable};

use crate::builtins::{default_builtins, Builtins};
use crate::compiler::{Callable, Instruction, LinkedFunction as Function, Package};

/// Per-execution memory accounting. Shared between the `Execution` (which owns
/// the canonical `Rc`) and every `Accounted` box allocated in that execution's
/// arena (each carries a cloned `Rc`). Holds the running external-bytes count
/// (Rust-heap storage not seen by gc-arena's `Metrics`, including `SLVal`
/// payloads, interpreter vectors, and temporary reservations) and an optional
/// cap. `Rc<Cell<_>>` is appropriate because gc-arena is single-threaded; if
/// `Execution` ever needs to be `Send`, swap for `Arc<AtomicUsize>`.
#[derive(Default)]
struct MemoryTracker {
  external_bytes: Cell<usize>,
  limit: Cell<Option<usize>>,
  /// Accumulated *positive* external allocation debt waiting to be fed to
  /// gc-arena's `Metrics::adjust_debt`. Incremented by [`charge`] (external
  /// allocation grows the heap, so the collector should run sooner); *not*
  /// decremented by [`release`] (sweep-time reclaims reduce `external_bytes`
  /// but must not produce negative debt, because `collect_debt` runs outside
  /// interpreter steps and may have already reset the GC cycle — a stale
  /// negative delta would suppress collection until an equivalent amount of
  /// new allocation occurs). Drained to `adjust_debt` (and reset to 0) by
  /// [`drain_pacing_debt`] at each `check_memory_limit`.
  pending_pacing_debt: Cell<usize>,
}

impl MemoryTracker {
  fn new() -> Self {
    Self::default()
  }

  /// Charge `n` bytes to this tracker's running count and accumulate `n` as
  /// pending pacing debt (so the next `check_memory_limit` grows GC debt by
  /// `n`, pacing incremental collection to external allocation). Uses checked
  /// arithmetic so an overflow panics (rather than silently wrapping and
  /// corrupting the limit).
  fn charge(&self, n: usize) -> usize {
    let new = self
      .external_bytes
      .get()
      .checked_add(n)
      .expect("external byte count overflow");
    self.external_bytes.set(new);
    let debt = self
      .pending_pacing_debt
      .get()
      .checked_add(n)
      .expect("pending pacing debt overflow");
    self.pending_pacing_debt.set(debt);
    new
  }

  /// Release `n` bytes from this tracker's running count (called from
  /// `Accounted::drop` / `MemoryCharge::drop` when the GC sweeps an
  /// unreachable box or a `TrackedVec` is dropped). Does *not* touch
  /// `pending_pacing_debt`: a release is a sweep-time reclamation, and
  /// applying it as a negative `adjust_debt` would be stale if `collect_debt`
  /// already reset the GC cycle. Returns the new total. Uses checked
  /// arithmetic so an underflow panics.
  fn release(&self, n: usize) -> usize {
    let prev = self.external_bytes.get();
    let new = prev.checked_sub(n).expect("external byte count underflow");
    self.external_bytes.set(new);
    new
  }

  /// Temporarily reserve `n` external bytes without adding GC pacing debt.
  ///
  /// Reservations protect allocations that are being built outside the GC
  /// arena. They count toward the hard memory limit while live, but they do
  /// not represent newly reachable memory and therefore must not make
  /// gc-arena collect sooner. When a completed value is boxed in
  /// [`Accounted`], its ordinary [`Self::charge`] supplies the pacing debt.
  fn reserve(&self, n: usize) -> Result<(), String> {
    let new = self
      .external_bytes
      .get()
      .checked_add(n)
      .ok_or_else(|| "memory accounting overflow while reserving memory".to_string())?;
    self.external_bytes.set(new);
    Ok(())
  }

  fn external_bytes(&self) -> usize {
    self.external_bytes.get()
  }

  fn limit(&self) -> Option<usize> {
    self.limit.get()
  }

  fn set_limit(&self, limit: Option<usize>) {
    self.limit.set(limit);
  }

  /// Drain the accumulated positive pacing debt, returning the amount to feed
  /// to `Metrics::adjust_debt` (and resetting the accumulator to 0). Only
  /// ever returns a non-negative value: `charge` accumulates, `release`
  /// doesn't subtract. This keeps incremental collection paced to external
  /// allocation without risking a stale negative delta suppressing a fresh
  /// GC cycle. May collect slightly more aggressively than a sampled-delta
  /// scheme, but is safe and deterministic.
  fn drain_pacing_debt(&self) -> usize {
    let debt = self.pending_pacing_debt.get();
    self.pending_pacing_debt.set(0);
    debt
  }
}

/// A `MemoryTracker` shared via `Rc` so it can live inside a GC'd `Accounted`
/// box. Each `Execution` mints one `Rc<MemoryTracker>`; every value allocated
/// in that execution receives a clone. The tracker stays alive until the last
/// `Accounted` box is swept (which releases its `Rc`).
type SharedTracker = Rc<MemoryTracker>;

/// A per-execution reservation for temporary Rust-heap memory.
///
/// Reservations are created through [`HostCtx::reserve_memory`], count toward
/// that execution's memory limit while live, and release automatically on
/// success, error, or panic. A host function must keep the guard alive for as
/// long as the corresponding allocation is live.
///
/// Reservations deliberately do not add GC pacing debt. If the allocation is
/// returned as an [`SLVal`], the builtin call path immediately wraps it in
/// [`Accounted`] after the host handler returns; that permanent charge
/// supplies the pacing debt.
pub struct MemoryReservation {
  tracker: SharedTracker,
  bytes: usize,
}

impl MemoryReservation {
  fn new(tracker: SharedTracker, bytes: usize) -> Result<Self, String> {
    tracker.reserve(bytes)?;
    Ok(Self { tracker, bytes })
  }

  /// Number of bytes currently reserved by this guard.
  pub fn bytes(&self) -> usize {
    self.bytes
  }

  fn belongs_to(&self, tracker: &SharedTracker) -> bool {
    Rc::ptr_eq(&self.tracker, tracker)
  }

  fn set(&mut self, bytes: usize) -> Result<(), String> {
    if bytes > self.bytes {
      self.tracker.reserve(bytes - self.bytes)?;
    } else if bytes < self.bytes {
      self.tracker.release(self.bytes - bytes);
    }
    self.bytes = bytes;
    Ok(())
  }
}

impl Drop for MemoryReservation {
  fn drop(&mut self) {
    self.tracker.release(self.bytes);
  }
}

/// A RAII handle that releases a fixed byte charge to a [`MemoryTracker`] on
/// `Drop`. Held as a `#[collect(require_static)]` field inside `Accounted` and
/// `TrackedVec` so those outer types have *no custom `Drop`* of their own —
/// the charge cleanup lives entirely in this `'static` helper, and gc-arena's
/// safe `#[collect(no_drop)]` derive is honest.
///
/// `MemoryCharge` is `'static` (it holds only an `Rc<MemoryTracker>` and a
/// `usize`), so the `require_static` annotation is valid and the derive skips
/// tracing it. The `Drop` here touches only the tracker and the byte count —
/// never any `Gc` pointers — which is the invariant `#[collect(no_drop)]`
/// preserves.
struct MemoryCharge {
  tracker: SharedTracker,
  bytes: usize,
}

impl MemoryCharge {
  /// Create a charge of `bytes` against `tracker` (charges immediately).
  fn new(tracker: SharedTracker, bytes: usize) -> Self {
    tracker.charge(bytes);
    MemoryCharge { tracker, bytes }
  }

  /// The byte count currently held by this charge.
  fn bytes(&self) -> usize {
    self.bytes
  }

  /// Adjust this charge to a new byte count: releases the old, charges the
  /// new. Used by `TrackedVec::reconcile` when the underlying `Vec`'s capacity
  /// changes.
  fn set(&mut self, bytes: usize) {
    if bytes != self.bytes {
      self.tracker.release(self.bytes);
      self.tracker.charge(bytes);
      self.bytes = bytes;
    }
  }
}

impl Drop for MemoryCharge {
  fn drop(&mut self) {
    self.tracker.release(self.bytes);
  }
}

/// Measure the *directly owned* Rust-heap storage of an `SLVal` payload — the
/// bytes that live outside the `GcBox` layout gc-arena records. This is what
/// `Accounted` charges the tracker at allocation and releases at sweep:
///
/// - `String`: the `String`'s heap buffer (`capacity()`, not `len`).
/// - `List`/`Partial`: the `Vec`'s backing array
///   (`capacity() * size_of::<Value>()`).
///
/// Only *directly owned* storage is counted; pointed-to `Gc` boxes are
/// accounted by their own `Accounted` wrappers, so there's no double-counting.
fn external_bytes_of<'gc>(value: &SLVal<'gc>) -> usize {
  match value {
    SLVal::String(s) => s.capacity(),
    SLVal::List(items) => items.capacity() * std::mem::size_of::<Value<'gc>>(),
    SLVal::Partial(p) => p.args.capacity() * std::mem::size_of::<Value<'gc>>(),
    SLVal::Struct(s) => s.fields.capacity() * std::mem::size_of::<Value<'gc>>(),
    SLVal::Enum(e) => e.fields.capacity() * std::mem::size_of::<Value<'gc>>(),
  }
}

/// A garbage-collected SafeLisp value wrapped with per-execution memory
/// accounting. The `value: SLVal` is the actual payload; `charge` holds the
/// byte count of the Rust-heap storage `value` directly owns (string bytes,
/// list/partial `Vec` backings) and releases it to the per-execution
/// the execution's memory tracker on `Drop`.
///
/// `Accounted` is `!Clone` by design: every runtime value must be created via
/// `ExecRoot::alloc_heap`, which charges the tracker. The `charge` field is
/// `#[collect(require_static)]` (it's `'static` — only an `Rc<MemoryTracker>`
/// and a `usize`, no `Gc` pointers), so gc-arena's derive skips tracing it and
/// the `#[collect(no_drop)]` on `Accounted` is honest: the only `Drop` glue is
/// `MemoryCharge`'s, which touches no `Gc` pointers (the safety invariant).
///
/// Field order matters: `value` is declared before `charge` so Rust drops the
/// `SLVal` payload first, then releases the accounting charge.
#[derive(Collect)]
#[collect(no_drop)]
pub struct Accounted<'gc> {
  /// The actual SafeLisp heap value being accounted.
  pub value: SLVal<'gc>,
  /// Held only for its `Drop` (which releases the charge to the tracker); the
  /// byte count is read via `MemoryCharge::bytes()` only on `TrackedVec`.
  #[allow(dead_code)]
  #[collect(require_static)]
  charge: MemoryCharge,
}

impl<'gc> std::fmt::Debug for Accounted<'gc> {
  /// Debug-format only the `SLVal` payload; the charge is accounting metadata,
  /// not part of the value's identity.
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("Accounted")
      .field("value", &self.value)
      .finish()
  }
}

impl<'gc> PartialEq for Accounted<'gc> {
  /// Compare accounted values by their `SLVal` payload. The charge is
  /// accounting metadata, not part of the value's identity.
  fn eq(&self, other: &Self) -> bool {
    self.value == other.value
  }
}

impl<'gc> Eq for Accounted<'gc> {}

impl<'gc> Accounted<'gc> {
  /// Construct an `Accounted` value, charge its external storage to `tracker`,
  /// and return it ready to be boxed via `Gc::new`. Callers should go through
  /// `ExecRoot::alloc_heap` rather than calling this directly.
  fn new(value: SLVal<'gc>, tracker: SharedTracker) -> Self {
    let external_bytes = external_bytes_of(&value);
    Accounted {
      value,
      charge: MemoryCharge::new(tracker, external_bytes),
    }
  }
}

/// The shared handle used for every in-arena value. Small immutable values are
/// stored inline; values with owned Rust-heap storage or recursive structure
/// are held behind GC pointers.
#[derive(Debug, Copy, Clone, PartialEq, Collect)]
#[collect(no_drop)]
pub enum Value<'gc> {
  /// The SafeLisp void value.
  Void,
  /// A boolean value stored inline.
  Bool(bool),
  /// An integer value stored inline.
  Int(i64),
  /// A floating-point value stored inline.
  Float(f64),
  /// A reference to a package function identified by `(module, function)`.
  FunctionRef(u32, u32),
  /// A mutable cell allocated in the execution arena.
  Cell(Gc<'gc, RefLock<CellContents<'gc>>>),
  /// A heap-backed value allocated with accounting metadata.
  Heap(Gc<'gc, Accounted<'gc>>),
}

/// A `Vec<T>` that charges its backing-allocation capacity to a per-execution
/// [`MemoryTracker`]. Used for `ExecRoot.stack`, `ExecRoot.frames`, and
/// `Frame.locals` so the memory limit sees the Rust-heap overhead of the
/// interpreter's own `Vec`s (which hold `Gc` pointers but allocate plain Rust
/// heap, invisible to gc-arena's `Metrics`). The charge is
/// `capacity() * size_of::<T>()`; it's reconciled on every capacity change
/// (push/reserve/shrink_to_fit) and released when the `MemoryCharge` field
/// drops.
///
/// `TrackedVec` has no custom `Drop` — the charge cleanup lives in the
/// `charge: MemoryCharge` field, which is `#[collect(require_static)]` (it's
/// `'static`: an `Rc<MemoryTracker>` + `usize`, no `Gc` pointers). gc-arena's
/// `#[collect(no_drop)]` derive is therefore honest: the only `Drop` glue is
/// `MemoryCharge`'s, which touches no `Gc` pointers. The derive traces `inner`
/// normally (so element `Gc` pointers are traced) and skips `charge`.
///
/// Field order matters: `inner` is declared before `charge` so Rust drops the
/// `Vec<T>` payload first, then releases the accounting charge.
#[derive(Collect)]
#[collect(no_drop)]
struct TrackedVec<T> {
  inner: Vec<T>,
  #[collect(require_static)]
  charge: MemoryCharge,
}

impl<T> TrackedVec<T> {
  /// Current charge (capacity × element size).
  fn bytes_for(capacity: usize) -> usize {
    capacity * std::mem::size_of::<T>()
  }

  /// Reconcile the charge to the current `inner.capacity()`. Called after any
  /// capacity-changing op.
  fn reconcile(&mut self) {
    let new = Self::bytes_for(self.inner.capacity());
    self.charge.set(new);
  }

  pub fn new(tracker: SharedTracker) -> Self {
    TrackedVec {
      inner: Vec::new(),
      charge: MemoryCharge::new(tracker, 0),
    }
  }

  /// Build a `TrackedVec` from an existing `Vec`, adopting its capacity. Used
  /// when a `Vec` of pre-bound values is handed to `enter_function`: the
  /// `Vec`'s backing storage is charged to `tracker` so the memory limit sees
  /// it. Excess capacity is shrunk to fit so the charge is tight.
  pub fn from_vec(inner: Vec<T>, tracker: SharedTracker) -> Self {
    let mut inner = inner;
    inner.shrink_to_fit();
    let charged = Self::bytes_for(inner.capacity());
    TrackedVec {
      inner,
      charge: MemoryCharge::new(tracker, charged),
    }
  }

  pub fn push(&mut self, value: T) {
    self.inner.push(value);
    // push may have grown the capacity; reconcile if so.
    if Self::bytes_for(self.inner.capacity()) != self.charge.bytes() {
      self.reconcile();
    }
  }

  pub fn pop(&mut self) -> Option<T> {
    // pop doesn't shrink capacity, so no reconcile needed.
    self.inner.pop()
  }

  pub fn last(&self) -> Option<&T> {
    self.inner.last()
  }

  pub fn last_mut(&mut self) -> Option<&mut T> {
    self.inner.last_mut()
  }

  pub fn len(&self) -> usize {
    self.inner.len()
  }

  pub fn is_empty(&self) -> bool {
    self.inner.is_empty()
  }

  /// Shorten the vector to `len`, dropping any elements beyond it. Used by
  /// `Return` to discard a callee's entire stack segment after
  /// extracting (or discarding) its result, restoring the caller's view. Does
  /// not shrink capacity, so no reconcile is needed.
  pub fn truncate(&mut self, len: usize) {
    self.inner.truncate(len);
  }
}

impl<T> std::ops::Index<usize> for TrackedVec<T> {
  type Output = T;
  fn index(&self, index: usize) -> &T {
    &self.inner[index]
  }
}

impl<T> std::ops::IndexMut<usize> for TrackedVec<T> {
  fn index_mut(&mut self, index: usize) -> &mut T {
    &mut self.inner[index]
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
struct ExecRoot<'gc> {
  stack: TrackedVec<Value<'gc>>,
  frames: TrackedVec<Frame<'gc>>,
  /// Per-execution memory tracker. Owns the running external-bytes count (Rust
  /// heap storage not seen by gc-arena's `Metrics`) and the optional cap. Each
  /// `Accounted` box allocated in this arena carries a clone of this `Rc`, so
  /// `Accounted::drop` (invoked by the GC at sweep) decrements the count for
  /// the right execution. The `require_static` field attribute on `Accounted`
  /// excludes this `Rc` from tracing. `TrackedVec` also clones this `Rc` so
  /// `stack`/`frames`/`locals` capacity is charged to the same tracker.
  #[collect(require_static)]
  tracker: SharedTracker,
}

impl<'gc> ExecRoot<'gc> {
  /// The single chokepoint for creating heap-backed values. Wraps the
  /// `SLVal` in `Accounted` (charging the tracker for any directly-owned
  /// external Rust-heap storage) and boxes it via `Gc::new`.
  fn alloc_heap(&mut self, mc: &'gc Mutation<'gc>, value: SLVal<'gc>) -> Value<'gc> {
    Value::Heap(Gc::new(mc, Accounted::new(value, self.tracker.clone())))
  }

  /// Reconstruct a runtime `Value` inside this execution's arena from an owned
  /// `SLValue`. Scalar values are imported inline; heap-shaped values are
  /// deep-copied into fresh GC boxes.
  fn import_value(&mut self, mc: &'gc Mutation<'gc>, value: &SLValue) -> Value<'gc> {
    match value {
      SLValue::Int(i) => Value::Int(*i),
      SLValue::Float(x) => Value::Float(*x),
      SLValue::String(s) => self.alloc_heap(mc, SLVal::String(s.clone())),
      SLValue::Bool(b) => Value::Bool(*b),
      SLValue::Void => Value::Void,
      SLValue::FunctionRef(m, n) => Value::FunctionRef(*m, *n),
      SLValue::Partial { function, args } => {
        let sub: Vec<Value<'gc>> = args.iter().map(|a| self.import_value(mc, a)).collect();
        self.alloc_heap(
          mc,
          SLVal::Partial(Partial {
            function: *function,
            args: sub,
          }),
        )
      }
      SLValue::Cell(inner) => {
        let inner_value = self.import_value(mc, inner);
        let contents = CellContents::new(inner_value);
        Value::Cell(Gc::new(mc, RefLock::new(contents)))
      }
      SLValue::List(items) => {
        let sub: Vec<Value<'gc>> = items.iter().map(|i| self.import_value(mc, i)).collect();
        self.alloc_heap(mc, SLVal::List(sub))
      }
      SLValue::Struct { struct_, fields } => {
        let sub = fields
          .iter()
          .map(|value| self.import_value(mc, value))
          .collect();
        self.alloc_heap(
          mc,
          SLVal::Struct(StructInstance {
            struct_: *struct_,
            fields: sub,
          }),
        )
      }
      SLValue::Enum {
        enum_,
        variant,
        fields,
      } => {
        let sub = fields
          .iter()
          .map(|value| self.import_value(mc, value))
          .collect();
        self.alloc_heap(
          mc,
          SLVal::Enum(EnumInstance {
            enum_: *enum_,
            variant: *variant,
            fields: sub,
          }),
        )
      }
    }
  }
}

/// A stack frame.
#[derive(Collect)]
#[collect(no_drop)]
struct Frame<'gc> {
  /// The package callable slot currently executing in this frame.
  callable: (u32, u32),
  /// The index into the global `stack` at which this frame's segment begins.
  /// Everything at or above this index belongs to this frame or its callees.
  /// When we return, we can truncate to this and push the return value.
  stack_base: usize,
  kind: FrameKind<'gc>,
}

#[derive(Collect)]
#[collect(no_drop)]
enum FrameKind<'gc> {
  Function(FunctionFrame<'gc>),
  Host(HostFrame),
}

/// A SafeLisp function frame. The shared frame's callable is looked up via
/// `package.get_function(...)` at `step` time.
#[derive(Collect)]
#[collect(no_drop)]
struct FunctionFrame<'gc> {
  locals: TrackedVec<Value<'gc>>,
  ip: usize,
}

/// A frame representing a resumable builtin function that may be waiting for a
/// SafeLisp call to finish.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Collect)]
#[collect(no_drop)]
struct HostFrame {
  awaiting_call: bool,
}

/// The mutable contents of a `Cell`: a shared handle to any runtime value.
/// Cells retain existing heap handles rather than cloning their `SLVal`
/// payloads, while immediate values are copied directly.
///
/// Allocated as `Gc<RefLock<CellContents>>` (the `RefLock` provides interior
/// mutability; the `Gc` box gives it an identity in the arena and makes it
/// eligible for cycle collection). Tracing the `value` handle keeps any
/// reachable heap allocation—and therefore its external-memory charge—alive.
#[derive(Collect)]
#[collect(no_drop)]
pub struct CellContents<'gc> {
  /// The value currently stored in the cell.
  pub value: Value<'gc>,
}

impl<'gc> std::fmt::Debug for CellContents<'gc> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("CellContents")
      .field("value", &self.value)
      .finish()
  }
}

impl<'gc> PartialEq for CellContents<'gc> {
  fn eq(&self, other: &Self) -> bool {
    self.value == other.value
  }
}

impl<'gc> Eq for CellContents<'gc> {}

impl<'gc> CellContents<'gc> {
  pub(crate) fn new(value: Value<'gc>) -> Self {
    CellContents { value }
  }

  pub(crate) fn set(&mut self, value: Value<'gc>) {
    self.value = value;
  }
}

/// A SafeLisp value. This is the in-arena representation: anything that needs
/// sharing or cycle-detection is held behind a `Gc` pointer.
#[derive(Debug, PartialEq, Collect)]
#[collect(no_drop)]
pub enum SLVal<'gc> {
  /// A heap-backed string.
  String(String),
  /// A partially applied function and its captured arguments.
  Partial(Partial<'gc>),
  /// A heap-backed list.
  List(Vec<Value<'gc>>),
  /// A user-defined struct instance.
  Struct(StructInstance<'gc>),
  /// A user-defined enum instance.
  Enum(EnumInstance<'gc>),
}

/// A partially applied SafeLisp function.
#[derive(Debug, PartialEq, Collect)]
#[collect(no_drop)]
pub struct Partial<'gc> {
  /// The referenced function as `(module, function)`.
  pub function: (u32, u32),
  /// Arguments captured by the partial application.
  pub args: Vec<Value<'gc>>,
}

/// A user-defined struct value.
#[derive(Debug, PartialEq, Collect)]
#[collect(no_drop)]
pub struct StructInstance<'gc> {
  /// The referenced struct definition as `(module, type)`.
  pub struct_: (u32, u32),
  /// Field values in definition order.
  pub fields: Vec<Value<'gc>>,
}

/// A user-defined enum value.
#[derive(Debug, PartialEq, Collect)]
#[collect(no_drop)]
pub struct EnumInstance<'gc> {
  /// The referenced enum definition as `(module, type)`.
  pub enum_: (u32, u32),
  /// The zero-based variant index within the enum definition.
  pub variant: u16,
  /// Variant field values in definition order.
  pub fields: Vec<Value<'gc>>,
}

/// An owned, arena-agnostic value that can escape the arena and be fed back
/// into any other (or the same) execution. Non-scalar values are represented
/// recursively with owned `SLValue` sub-values, so converting across the arena
/// boundary performs a deep copy: `Gc` pointers in one arena are unboxed into
/// owned values and re-boxed into fresh `Gc` pointers in the destination arena.
#[derive(Debug, PartialEq, Clone)]
pub enum SLValue {
  /// An integer value.
  Int(i64),
  /// A floating-point value.
  Float(f64),
  /// A string value.
  String(String),
  /// A boolean value.
  Bool(bool),
  /// The void value.
  Void,
  /// A reference to a package function identified by `(module, function)`.
  FunctionRef(u32, u32),
  /// A partially applied function and its captured arguments.
  Partial {
    /// The referenced function as `(module, function)`.
    function: (u32, u32),
    /// Captured argument values.
    args: Vec<SLValue>,
  },
  /// A mutable cell value represented by its current contents.
  Cell(Box<SLValue>),
  /// A list of values.
  List(Vec<SLValue>),
  /// A user-defined struct instance.
  Struct {
    /// The referenced struct definition as `(module, type)`.
    struct_: (u32, u32),
    /// Field values in definition order.
    fields: Vec<SLValue>,
  },
  /// A user-defined enum instance.
  Enum {
    /// The referenced enum definition as `(module, type)`.
    enum_: (u32, u32),
    /// The zero-based variant index within the enum definition.
    variant: u16,
    /// Variant field values in definition order.
    fields: Vec<SLValue>,
  },
}

impl<'gc> Value<'gc> {
  /// A bounded-size description suitable for runtime type errors. Default
  /// builtins use this instead of debug-formatting guest-controlled values,
  /// which could itself allocate an unbounded diagnostic string.
  pub fn type_name(&self) -> &'static str {
    match self {
      Value::Int(_) => "Int",
      Value::Float(_) => "Float",
      Value::Heap(gc) => gc.value.type_name(),
      Value::Bool(_) => "Bool",
      Value::Void => "Void",
      Value::FunctionRef(_, _) => "FunctionRef",
      Value::Cell(_) => "Cell",
    }
  }

  /// A bounded-size value description for runtime errors. Scalar payloads are
  /// useful and fixed-size, while guest-sized strings, lists, and partial
  /// arguments are represented only by type.
  fn error_description(&self) -> String {
    match self {
      Value::Int(value) => format!("Int({value})"),
      Value::Float(value) => format!("Float({value:?})"),
      Value::Bool(value) => format!("Bool({value})"),
      Value::Void => "Void".to_string(),
      Value::FunctionRef(module, function) => {
        format!("FunctionRef({module}, {function})")
      }
      Value::Heap(_) | Value::Cell(_) => self.type_name().to_string(),
    }
  }

  /// Convert a runtime `Value` into an owned `SLValue`, deep-copying any `Gc`-held
  /// sub-values out of the arena.
  pub fn to_value(self) -> SLValue {
    match self {
      Value::Int(i) => SLValue::Int(i),
      Value::Float(x) => SLValue::Float(x),
      Value::Heap(gc) => gc.value.to_value(),
      Value::Bool(b) => SLValue::Bool(b),
      Value::Void => SLValue::Void,
      Value::FunctionRef(m, n) => SLValue::FunctionRef(m, n),
      Value::Cell(r) => SLValue::Cell(Box::new(r.borrow().value.to_value())),
    }
  }
}

impl<'gc> SLVal<'gc> {
  /// A bounded-size description of this heap value's SafeLisp type.
  pub fn type_name(&self) -> &'static str {
    match self {
      SLVal::String(_) => "String",
      SLVal::Partial(_) => "Partial",
      SLVal::List(_) => "List",
      SLVal::Struct(_) => "Struct",
      SLVal::Enum(_) => "Enum",
    }
  }

  /// Convert this in-arena heap value into an owned, arena-agnostic value.
  pub fn to_value(&self) -> SLValue {
    match self {
      SLVal::String(s) => SLValue::String(s.clone()),
      SLVal::Partial(p) => SLValue::Partial {
        function: p.function,
        args: p.args.iter().map(|value| value.to_value()).collect(),
      },
      SLVal::List(items) => SLValue::List(items.iter().map(|value| value.to_value()).collect()),
      SLVal::Struct(s) => SLValue::Struct {
        struct_: s.struct_,
        fields: s.fields.iter().map(|value| value.to_value()).collect(),
      },
      SLVal::Enum(e) => SLValue::Enum {
        enum_: e.enum_,
        variant: e.variant,
        fields: e.fields.iter().map(|value| value.to_value()).collect(),
      },
    }
  }
}

/// A compiled SafeLisp package paired with the builtin registry used to run it.
pub struct Interpreter {
  package: Package,
  builtins: Builtins,
}

impl Interpreter {
  /// Construct an interpreter with an explicit builtin registry.
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
    self.call_main_with(vec![])
  }

  /// Set up an `Execution` ready to run `main` with arguments.
  pub fn call_main_with(&self, args: Vec<SLValue>) -> Result<Execution, String> {
    if let Some((module, function)) = self.package.main {
      let callable = self
        .package
        .get_function(module, function)
        .ok_or_else(|| format!("Couldn't find module {} function {}", module, function))?;
      if let Callable::Function(linked_function) = callable {
        let expected = usize::from(linked_function.num_params);
        if args.len() != expected {
          return Err(format!(
            "{}.{} expects {} arg(s) but was called with {}",
            module,
            function,
            expected,
            args.len()
          ));
        }
        let mut exec = Execution::new(self.package.clone(), self.builtins.clone());
        exec.enter_function_at(module, function, args)?;
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

  /// Set up an `Execution` ready to call `callable` with arguments. The
  /// `SLValue`s are deep-copied into the new execution's arena, so values
  /// produced by one execution can be fed into any other execution (or the
  /// same one).
  pub fn call_value(&self, callable: SLValue, args: Vec<SLValue>) -> Result<Execution, String> {
    let mut exec = Execution::new(self.package.clone(), self.builtins.clone());
    exec.push_and_call_dynamic(callable, args)?;
    Ok(exec)
  }
}

/// The result of running an execution for a bounded amount of work.
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

/// Indicator for whether a resumable builtin function has completed or not.
pub enum HostPoll<'gc> {
  /// The host builtin is still waiting and should be polled again later.
  Pending,
  /// The host builtin completed with the given runtime value.
  Ready(Value<'gc>),
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
  executed: u64,
  /// Per-execution memory tracker, shared with every `Accounted` box in this
  /// arena. `Execution` owns the canonical `Rc`; cloned into each `Accounted`
  /// at allocation (see `ExecRoot::alloc_heap`).
  tracker: SharedTracker,
}

impl Execution {
  fn new(package: Package, builtins: Builtins) -> Self {
    let tracker = Rc::new(MemoryTracker::new());
    let tracker_for_root = tracker.clone();
    let arena = Arena::new(move |_mc| ExecRoot {
      stack: TrackedVec::new(tracker_for_root.clone()),
      frames: TrackedVec::new(tracker_for_root.clone()),
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

  /// The cumulative number of bytecodes executed by this execution.
  pub fn executed(&self) -> u64 {
    self.executed
  }

  /// The number of items currently on the value stack.
  #[cfg(test)]
  fn stack_len(&self) -> usize {
    self.arena.mutate(|_, root| root.stack.len())
  }

  /// Peek the top of the value stack as an owned `SLValue`. Returns an error if
  /// the stack is empty.
  pub fn peek_value(&self) -> Result<SLValue, String> {
    let mut result = Err("PEEK on an empty stack".to_string());
    self.arena.mutate(|_, root| {
      if let Some(top) = root.stack.last().copied() {
        result = Ok(top.to_value());
      }
    });
    result
  }

  /// The current number of live `Gc` allocations in this execution's arena.
  /// Useful for tests that want to observe garbage collection.
  #[cfg(test)]
  pub(crate) fn gc_count(&self) -> usize {
    self.arena.metrics().total_gc_count()
  }

  /// The total bytes currently allocated by live `Gc` pointers in this
  /// execution's arena (the gc-arena-reported portion of `memory_usage`).
  #[cfg(test)]
  fn gc_allocation_bytes(&self) -> usize {
    self.arena.metrics().total_gc_allocation()
  }

  /// The current live memory in bytes: gc-arena's `total_gc_allocation` (the
  /// `GcBox` layouts) plus this execution's `external_bytes` (Rust-heap
  /// storage owned by `SLVal` payloads — String bytes, List/Partial `Vec`
  /// backings, interpreter vectors, and temporary reservations). This is the
  /// value the optional memory limit is compared against on every `step` and
  /// before guest-sized host allocations.
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
  #[cfg(test)]
  fn collect_all(&mut self) {
    self.arena.finish_cycle();
  }

  /// Run up to `n` bytecodes. Returns `Paused` if the budget exhausted before
  /// completion, or `Done(v)` if the program completed. Errors propagate via
  /// `Err`. The cumulative count of executed bytecodes is available on
  /// [`Execution::executed`] after the call returns.
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
  /// [`Execution::executed`] after the call returns.
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

  /// Push a frame for a function looked up by `(module, function)` index in
  /// this execution's `Package`, with optional pre-bound values given as owned
  /// `SLValue`s. The frame stores the index (not a clone of the function's
  /// instructions), so deep recursion is cheap.
  fn enter_function_at(
    &mut self,
    mod_idx: u32,
    func_idx: u32,
    pre_bound: Vec<SLValue>,
  ) -> Result<(), String> {
    let function = match self.package.get_function(mod_idx, func_idx) {
      Some(crate::compiler::Callable::Function(f)) => f.clone(),
      _ => return Err(format!("Function not found: {}/{}", mod_idx, func_idx)),
    };
    let result = self.arena.mutate_root(|mc, root| {
      let pre_bound_gc: Vec<Value<'_>> =
        pre_bound.iter().map(|v| root.import_value(mc, v)).collect();
      root.enter_function(mc, mod_idx, func_idx, &function, pre_bound_gc)
    });
    result
  }

  /// Push arguments and a callable onto the stack and invoke `call_dynamic`
  /// (used to set up an execution from external `SLValue`s).
  fn push_and_call_dynamic(&mut self, callable: SLValue, args: Vec<SLValue>) -> Result<(), String> {
    let arity =
      u16::try_from(args.len()).map_err(|_| format!("too many call arguments: {}", args.len()))?;
    let package = self.package.clone();
    let builtins = self.builtins.clone();
    let result = self.arena.mutate_root(|mc, root| {
      for arg in &args {
        let gc = root.import_value(mc, arg);
        root.stack.push(gc);
      }
      let callable = root.import_value(mc, &callable);
      root.stack.push(callable);
      root.call_dynamic(mc, &package, &builtins, arity)
    });
    result
  }
}

impl<'gc> ExecRoot<'gc> {
  fn memory_usage(&self, mc: &Mutation<'gc>) -> Result<usize, String> {
    mc.metrics()
      .total_gc_allocation()
      .checked_add(self.tracker.external_bytes())
      .ok_or_else(|| "memory accounting overflow while calculating usage".to_string())
  }

  fn ensure_memory_available(
    &self,
    mc: &Mutation<'gc>,
    additional_bytes: usize,
  ) -> Result<(), String> {
    let usage = self
      .memory_usage(mc)?
      .checked_add(additional_bytes)
      .ok_or_else(|| "memory accounting overflow while reserving memory".to_string())?;
    if let Some(limit) = self.tracker.limit() {
      if usage > limit {
        return Err(format!(
          "memory limit exceeded: {} bytes live (limit {})",
          usage, limit
        ));
      }
    }
    Ok(())
  }

  fn reserve_memory(&self, mc: &Mutation<'gc>, bytes: usize) -> Result<MemoryReservation, String> {
    self.ensure_memory_available(mc, bytes)?;
    MemoryReservation::new(self.tracker.clone(), bytes)
  }

  fn reconcile_reservation(
    &self,
    mc: &Mutation<'gc>,
    reservation: &mut MemoryReservation,
    bytes: usize,
  ) -> Result<(), String> {
    if !reservation.belongs_to(&self.tracker) {
      return Err("memory reservation belongs to a different execution".to_string());
    }
    if bytes > reservation.bytes() {
      self.ensure_memory_available(mc, bytes - reservation.bytes())?;
    }
    reservation.set(bytes)
  }

  /// Returns true if the frame stack is empty (execution is complete).
  fn is_done(&self) -> bool {
    self.frames.is_empty()
  }

  /// Returns `Ok(())` if the live memory (gc-arena's `total_gc_allocation` +
  /// this execution's external bytes) is within the configured limit, or `Err`
  /// with a descriptive message if it has been exceeded. No-op limit check when
  /// no limit is set, but *always* drains the pending positive external-allocation
  /// debt into `Metrics::adjust_debt` so incremental collection stays paced to
  /// external (non-`Gc`-box) allocation — gc-arena otherwise can't see
  /// String/List backing storage and wouldn't collect soon enough to keep large
  /// external heaps bounded. Only positive debt is applied (see
  /// [`MemoryTracker::drain_pacing_debt`]) to avoid a stale negative delta
  /// suppressing a fresh GC cycle after `collect_debt` resets.
  fn check_memory_limit(&self, mc: &Mutation<'gc>) -> Result<(), String> {
    // Drain accumulated positive pacing debt into the arena's allocation debt
    // so incremental collection runs sooner when external heaps grow. Releases
    // (sweep-time reclaims) do not contribute, so this is always >= 0.
    let debt = self.tracker.drain_pacing_debt();
    if debt > 0 {
      mc.metrics().adjust_debt(debt as f64);
    }
    if let Some(limit) = self.tracker.limit() {
      let usage = self.memory_usage(mc)?;
      if usage > limit {
        return Err(format!(
          "memory limit exceeded: {} bytes live (limit {})",
          usage, limit
        ));
      }
    }
    Ok(())
  }

  /// Push a `Value` onto the execution's value stack. Used by
  /// [`Builtin::call`] to push a builtin's result.
  fn push_value(&mut self, val: Value<'gc>) {
    self.stack.push(val);
  }

  /// Pop a value off the execution's value stack.
  fn pop(&mut self) -> Result<Value<'gc>, String> {
    self
      .stack
      .pop()
      .ok_or_else(|| "POP on an empty stack".to_string())
  }

  /// The `stack_base` of the current (top) frame: the index into the global
  /// stack at which the current frame's segment begins. This is where we can
  /// truncate when returning from a function.
  fn frame_stack_base(&self) -> Result<usize, String> {
    self
      .frames
      .last()
      .map(|f| f.stack_base)
      .ok_or_else(|| "Return with no frame".to_string())
  }

  fn push_host_frame(&mut self, callable: (u32, u32), stack_base: usize) {
    self.frames.push(Frame {
      callable,
      stack_base,
      kind: FrameKind::Host(HostFrame {
        awaiting_call: false,
      }),
    });
  }

  fn current_host_frame_mut(&mut self) -> Result<&mut HostFrame, String> {
    match self.frames.last_mut() {
      Some(Frame {
        kind: FrameKind::Host(frame),
        ..
      }) => Ok(frame),
      Some(_) => Err("current frame is not a host frame".to_string()),
      None => Err("no current frame".to_string()),
    }
  }

  fn complete_host_frame(&mut self, value: Value<'gc>) -> Result<(), String> {
    let frame = self
      .frames
      .pop()
      .ok_or_else(|| "host completion with no frame".to_string())?;
    match frame.kind {
      FrameKind::Host(_) => {
        self.stack.truncate(frame.stack_base);
        self.stack.push(value);
        Ok(())
      }
      FrameKind::Function(_) => Err("host completion on a function frame".to_string()),
    }
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
      Ok(Status::Done(top.to_value()))
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
    Ok(top.to_value())
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
      Ok(Status::Done(top.to_value()))
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
    // Pre-instruction check catches limits exceeded by *previous* allocations
    // (e.g. a `Push` that pushed us over). The post-instruction check below
    // catches the current instruction's allocations at the instruction that
    // made them.
    self.check_memory_limit(mc)?;

    if let Some(callable_id) = {
      let frame = self
        .frames
        .last()
        .ok_or_else(|| "step with no frames".to_string())?;
      match &frame.kind {
        FrameKind::Host(_) => Some(frame.callable),
        FrameKind::Function(_) => None,
      }
    } {
      let module = package
        .get_module(callable_id.0)
        .ok_or_else(|| format!("Module not found: {}", callable_id.0))?;
      let (func_name, callable) = module
        .functions
        .get(callable_id.1 as usize)
        .ok_or_else(|| format!("Function not found: {}/{}", callable_id.0, callable_id.1))?;
      if !matches!(callable, Callable::Builtin) {
        return Err(format!(
          "{}.{} is not a builtin host frame",
          module.name, func_name
        ));
      }
      let builtin = builtins
        .lookup(&module.name, func_name)
        .ok_or_else(|| format!("No builtin {}::{}", module.name, func_name))?;
      let pending_result = {
        let awaiting_call = match self.frames.last() {
          Some(Frame {
            kind: FrameKind::Host(host),
            ..
          }) => host.awaiting_call,
          Some(_) => return Err("host dispatch found a function frame".to_string()),
          None => return Err("host dispatch with no frame".to_string()),
        };
        if awaiting_call {
          let result = self.pop()?;
          self.current_host_frame_mut()?.awaiting_call = false;
          Some(result)
        } else {
          None
        }
      };
      let poll = {
        let mut ctx = HostCtx {
          root: self,
          mc,
          package,
          builtins,
        };
        builtin.resume(&mut ctx, pending_result)?
      };
      if let HostPoll::Ready(value) = poll {
        self.complete_host_frame(value)?;
      }
      self.check_memory_limit(mc)?;
      return Ok(());
    }

    // Resolve the current function frame's instructions by looking up the
    // indexed function in the package, then advance `ip` after fetching the
    // instruction.
    let (ip, inst) = {
      let frame = self
        .frames
        .last()
        .ok_or_else(|| "step with no frames".to_string())?;
      let function_frame = match &frame.kind {
        FrameKind::Function(function_frame) => function_frame,
        FrameKind::Host(_) => unreachable!("host frames return from step above"),
      };
      let ip = function_frame.ip;
      let (mod_idx, func_idx) = frame.callable;
      let inst = match package.get_function(mod_idx, func_idx) {
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
      };
      (ip, inst)
    };
    {
      let frame = self
        .frames
        .last_mut()
        .ok_or_else(|| "step with no frames".to_string())?;
      let function_frame = match &mut frame.kind {
        FrameKind::Function(function_frame) => function_frame,
        FrameKind::Host(_) => unreachable!("host frames return from step above"),
      };
      function_frame.ip = ip + 1;
    }

    match inst {
      Instruction::PushInt(i) => {
        self.stack.push(Value::Int(i));
      }
      Instruction::PushFloat(f) => {
        self.stack.push(Value::Float(f));
      }
      Instruction::PushString(s) => {
        let v = self.alloc_heap(mc, SLVal::String(s));
        self.stack.push(v);
      }
      Instruction::PushBool(b) => {
        self.stack.push(Value::Bool(b));
      }
      Instruction::PushVoid => {
        self.stack.push(Value::Void);
      }
      Instruction::NewStruct(struct_) => {
        let struct_def = package
          .get_struct(struct_.0, struct_.1)
          .ok_or_else(|| format!("Struct not found: {}/{}", struct_.0, struct_.1))?;
        let field_count = struct_def.fields.len();
        let field_bytes = field_count
          .checked_mul(std::mem::size_of::<Value<'gc>>())
          .ok_or_else(|| "struct field buffer size overflow".to_string())?;
        let mut fields_reservation = self.reserve_memory(mc, field_bytes)?;
        let mut fields = Vec::new();
        fields.try_reserve_exact(field_count).map_err(|_| {
          format!(
            "failed to allocate struct field buffer for {} fields",
            field_count
          )
        })?;
        let actual_field_bytes = fields
          .capacity()
          .checked_mul(std::mem::size_of::<Value<'gc>>())
          .ok_or_else(|| "struct field buffer capacity overflow".to_string())?;
        self.reconcile_reservation(mc, &mut fields_reservation, actual_field_bytes)?;
        for _ in 0..field_count {
          fields.push(self.pop()?);
        }
        fields.reverse();
        drop(fields_reservation);
        let value = self.alloc_heap(mc, SLVal::Struct(StructInstance { struct_, fields }));
        self.stack.push(value);
      }
      Instruction::NewEnum(enum_, variant) => {
        let enum_def = package
          .get_enum(enum_.0, enum_.1)
          .ok_or_else(|| format!("Enum not found: {}/{}", enum_.0, enum_.1))?;
        let variant_def = enum_def.constructors.get(variant as usize).ok_or_else(|| {
          format!(
            "enum variant index {} out of range for enum {}/{}",
            variant, enum_.0, enum_.1
          )
        })?;
        let field_count = variant_def.fields.len();
        let field_bytes = field_count
          .checked_mul(std::mem::size_of::<Value<'gc>>())
          .ok_or_else(|| "enum field buffer size overflow".to_string())?;
        let mut fields_reservation = self.reserve_memory(mc, field_bytes)?;
        let mut fields = Vec::new();
        fields.try_reserve_exact(field_count).map_err(|_| {
          format!(
            "failed to allocate enum field buffer for {} fields",
            field_count
          )
        })?;
        let actual_field_bytes = fields
          .capacity()
          .checked_mul(std::mem::size_of::<Value<'gc>>())
          .ok_or_else(|| "enum field buffer capacity overflow".to_string())?;
        self.reconcile_reservation(mc, &mut fields_reservation, actual_field_bytes)?;
        for _ in 0..field_count {
          fields.push(self.pop()?);
        }
        fields.reverse();
        drop(fields_reservation);
        let value = self.alloc_heap(
          mc,
          SLVal::Enum(EnumInstance {
            enum_,
            variant,
            fields,
          }),
        );
        self.stack.push(value);
      }
      Instruction::GetField(field) => {
        let receiver = self.pop()?;
        let value = match receiver {
          Value::Heap(heap) => match &heap.value {
            SLVal::Struct(instance) => *instance.fields.get(field as usize).ok_or_else(|| {
              format!(
                "struct field index {} out of range for struct {}/{}",
                field, instance.struct_.0, instance.struct_.1
              )
            })?,
            _ => {
              return Err(format!(
                "field access expected a struct, got {}",
                receiver.type_name()
              ))
            }
          },
          _ => {
            return Err(format!(
              "field access expected a struct, got {}",
              receiver.type_name()
            ))
          }
        };
        self.stack.push(value);
      }
      Instruction::IsEnumVariant(variant) => {
        let receiver = self.pop()?;
        let matches = match receiver {
          Value::Heap(heap) => match &heap.value {
            SLVal::Enum(instance) => instance.variant == variant,
            _ => {
              return Err(format!(
                "match expected an enum, got {}",
                receiver.type_name()
              ))
            }
          },
          _ => {
            return Err(format!(
              "match expected an enum, got {}",
              receiver.type_name()
            ))
          }
        };
        self.stack.push(Value::Bool(matches));
      }
      Instruction::GetEnumField(field) => {
        let receiver = self.pop()?;
        let value = match receiver {
          Value::Heap(heap) => match &heap.value {
            SLVal::Enum(instance) => *instance.fields.get(field as usize).ok_or_else(|| {
              format!(
                "enum field index {} out of range for enum {}/{} variant {}",
                field, instance.enum_.0, instance.enum_.1, instance.variant
              )
            })?,
            _ => {
              return Err(format!(
                "enum field access expected an enum, got {}",
                receiver.type_name()
              ))
            }
          },
          _ => {
            return Err(format!(
              "enum field access expected an enum, got {}",
              receiver.type_name()
            ))
          }
        };
        self.stack.push(value);
      }
      Instruction::Pop => {
        self.pop()?;
      }
      Instruction::Return => {
        // Pop the top frame and everything within it on the global stack, then
        // push the return value.
        let stack_base = self.frame_stack_base()?;
        if self.stack.len() != stack_base + 1 {
          return Err(format!(
            "Return expects exactly one value in the frame segment, found {}",
            self.stack.len().saturating_sub(stack_base)
          ));
        }
        let result = self
          .stack
          .pop()
          .ok_or_else(|| "Return with an empty frame segment".to_string())?;
        self.stack.truncate(stack_base);
        self.frames.pop();
        self.stack.push(result);
      }
      Instruction::SetLocal(i) => {
        let val = self.pop()?;
        let frame = self
          .frames
          .last_mut()
          .ok_or_else(|| "SetLocal with no frame".to_string())?;
        let function_frame = match &mut frame.kind {
          FrameKind::Function(function_frame) => function_frame,
          FrameKind::Host(_) => return Err("SetLocal in host frame".to_string()),
        };
        function_frame.locals[usize::from(i)] = val;
      }
      Instruction::LoadLocal(i) => {
        let frame = self
          .frames
          .last()
          .ok_or_else(|| "LoadLocal with no frame".to_string())?;
        let function_frame = match &frame.kind {
          FrameKind::Function(function_frame) => function_frame,
          FrameKind::Host(_) => return Err("LoadLocal in host frame".to_string()),
        };
        self.stack.push(function_frame.locals[usize::from(i)]);
      }
      Instruction::Call((mod_index, func_index), arity) => {
        self.call_fixed(mc, package, builtins, mod_index, func_index, arity)?;
      }
      Instruction::CallDynamic(arity) => {
        self.call_dynamic(mc, package, builtins, arity)?;
      }
      Instruction::MakeFunctionRef((mod_index, func_index)) => {
        self.stack.push(Value::FunctionRef(mod_index, func_index));
      }
      Instruction::PartialApply(num_args) => {
        self.partial_apply(mc, num_args)?;
      }
      Instruction::Jump(offset) => {
        let frame = self
          .frames
          .last_mut()
          .ok_or_else(|| "Jump with no frame".to_string())?;
        let function_frame = match &mut frame.kind {
          FrameKind::Function(function_frame) => function_frame,
          FrameKind::Host(_) => return Err("Jump in host frame".to_string()),
        };
        // `offset` is relative to the instruction following the `Jump` (the IP
        // has already been advanced past it), so `Jump(0)` is a no-op.
        function_frame.ip = function_frame.ip.wrapping_add(offset as usize);
      }
      Instruction::JumpIfFalse(offset) => {
        let val = self.pop()?;
        match val {
          Value::Bool(false) => {
            let frame = self
              .frames
              .last_mut()
              .ok_or_else(|| "JumpIfFalse with no frame".to_string())?;
            let function_frame = match &mut frame.kind {
              FrameKind::Function(function_frame) => function_frame,
              FrameKind::Host(_) => return Err("JumpIfFalse in host frame".to_string()),
            };
            function_frame.ip = function_frame.ip.wrapping_add(offset as usize);
          }
          Value::Bool(true) => {}
          ref other => {
            return Err(format!(
              "`if` condition must be a bool, got {}",
              other.error_description()
            ))
          }
        }
      }
    }
    // Post-instruction check: catch the allocation made by *this* instruction
    // (Push/PartialApply/etc.) at the instruction that made it, rather than
    // one instruction late.
    self.check_memory_limit(mc)?;
    Ok(())
  }

  fn partial_apply(&mut self, mc: &'gc Mutation<'gc>, num_args: u16) -> Result<(), String> {
    let num_args = usize::from(num_args);
    let arg_bytes = num_args
      .checked_mul(std::mem::size_of::<Value<'gc>>())
      .ok_or_else(|| "partial argument buffer size overflow".to_string())?;
    let mut args_reservation = self.reserve_memory(mc, arg_bytes)?;
    let func = self.pop()?;
    let mut args = Vec::new();
    args
      .try_reserve_exact(num_args)
      .map_err(|_| format!("failed to allocate partial argument buffer for {num_args} values"))?;
    let actual_arg_bytes = args
      .capacity()
      .checked_mul(std::mem::size_of::<Value<'gc>>())
      .ok_or_else(|| "partial argument buffer capacity overflow".to_string())?;
    self.reconcile_reservation(mc, &mut args_reservation, actual_arg_bytes)?;
    for _ in 0..num_args {
      args.push(self.pop()?);
    }
    args.reverse();
    // No guest code or limit check can run between releasing the temporary
    // reservation and `Accounted::new` adopting the Vec's actual capacity.
    drop(args_reservation);
    let closure = match func {
      Value::FunctionRef(mod_index, func_index) => self.alloc_heap(
        mc,
        SLVal::Partial(Partial {
          function: (mod_index, func_index),
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
    let module = package
      .get_module(mod_index)
      .ok_or_else(|| format!("Module not found: {}", mod_index))?;
    let (func_name, callable) = module
      .functions
      .get(func_index as usize)
      .ok_or_else(|| format!("Function not found: {}/{}", mod_index, func_index))?;
    match callable {
      Callable::Function(func) => {
        if arity != func.num_params {
          return Err(format!(
            "{}.{} expects {} arg(s) but was called with {}",
            module.name, func_name, func.num_params, arity
          ));
        }
        self.enter_function(mc, mod_index, func_index, func, vec![])?;
      }
      Callable::Builtin => {
        let builtin = builtins
          .lookup(&module.name, func_name)
          .ok_or_else(|| format!("No builtin {}::{}", module.name, func_name))?;
        // For fixed-arity builtins, enforce the declared arity at the call site.
        if let Some(expected) = builtin.spec().num_params {
          if arity != expected {
            return Err(format!(
              "{}.{} expects {} arg(s) but was called with {}",
              module.name, func_name, expected, arity
            ));
          }
        }
        // For variadic builtins (`num_params == None`) the call-site arity is
        // the only source of the arg count.
        let n = arity as usize;
        let arg_bytes = n
          .checked_mul(std::mem::size_of::<Value<'gc>>())
          .ok_or_else(|| "builtin argument buffer size overflow".to_string())?;
        let mut args_reservation = self.reserve_memory(mc, arg_bytes)?;
        // Args were pushed left-to-right; popping gives reverse order.
        let mut args = Vec::new();
        args.try_reserve_exact(n).map_err(|_| {
          format!(
            "failed to allocate builtin argument buffer for {} values",
            n
          )
        })?;
        let actual_arg_bytes = args
          .capacity()
          .checked_mul(std::mem::size_of::<Value<'gc>>())
          .ok_or_else(|| "builtin argument buffer capacity overflow".to_string())?;
        self.reconcile_reservation(mc, &mut args_reservation, actual_arg_bytes)?;
        for _ in 0..n {
          args.push(self.pop()?);
        }
        args.reverse();
        let mut ctx = HostCtx {
          root: self,
          mc,
          package,
          builtins,
        };
        builtin.call(&mut ctx, (mod_index, func_index), &args)?;
      }
    }
    Ok(())
  }

  /// Prepare the callable that's on the top of stack to be called. This pushes
  /// a new frame for function callables. This is both the implementation of the
  /// `CallDynamic` instruction and the entry point used by
  /// `Interpreter::call_value`. `arity` is the number of args pushed at the
  /// call site (carried on [`Instruction::CallDynamic`]); see [`Self::call_fixed`].
  fn call_dynamic(
    &mut self,
    mc: &'gc Mutation<'gc>,
    package: &Package,
    builtins: &Builtins,
    arity: u16,
  ) -> Result<(), String> {
    let callable = self.pop()?;
    match callable {
      Value::FunctionRef(mod_index, func_index) => {
        self.call_fixed(mc, package, builtins, mod_index, func_index, arity)
      }
      Value::Heap(heap) => match &heap.value {
        SLVal::Partial(Partial {
          function: (mod_index, func_index),
          args,
        }) => {
          let module = package
            .get_module(*mod_index)
            .ok_or_else(|| format!("Module not found: {}", mod_index))?;
          let (func_name, callable) = module
            .functions
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
        _ => Err(format!(
          "Can't call a non-callable! {}",
          Value::Heap(heap).error_description()
        )),
      },
      x => Err(format!(
        "Can't call a non-callable! {}",
        x.error_description()
      )),
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
    _mc: &'gc Mutation<'gc>,
    mod_idx: u32,
    func_idx: u32,
    function: &Function,
    pre_bound: Vec<Value<'gc>>,
  ) -> Result<(), String> {
    let start = pre_bound.len();
    let mut locals = pre_bound;
    for _ in locals.len()..usize::from(function.num_locals) {
      locals.push(Value::Void);
    }
    // The parameters are "in order" on the stack, so popping will give them to
    // us in reverse order.
    for param_idx in (start..usize::from(function.num_params)).rev() {
      locals[param_idx] = self.pop()?;
    }
    let locals = TrackedVec::from_vec(locals, self.tracker.clone());
    // After all call arguments have been popped, the remaining stack belongs
    // to the caller; record the boundary so `Return` can restore it.
    let stack_base = self.stack.len();
    self.frames.push(Frame {
      callable: (mod_idx, func_idx),
      stack_base,
      kind: FrameKind::Function(FunctionFrame { locals, ip: 0 }),
    });
    Ok(())
  }
}

/// The runtime context passed to a builtin handler. It carries the GC
/// `Mutation` context, the `Package` / `Builtins` registries, and a
/// short-lived mutable borrow of the execution root — enough for a builtin to
/// allocate values, push results, and invoke SafeLisp callables.
///
/// # Lifetimes
///
/// - `'gc`: the arena brand, invariant. Used inside `Gc<'gc, …>`,
///   `ExecRoot<'gc>`, and `Mutation<'gc>`. Values branded `'gc` cannot escape
///   the arena callback.
/// - `'call`: the short mutable borrow of `ExecRoot` while invoking one
///   builtin. Distinct from `'gc` so the borrow checker can reborrow
///   `&'gc mut ExecRoot<'gc>` as `&'call mut ExecRoot<'gc>` (shortening the
///   outer reference without affecting the inner arena brand). This is what
///   lets `HostCtx` hold the root directly rather than passing it separately.
///
/// # Trust boundary
///
/// Host functions are trusted runtime extensions. They may allocate, block,
/// panic, or otherwise interact with the process outside SafeLisp's control.
/// A host function that allocates guest-sized Rust-heap memory must use
/// [`Self::reserve_memory`] and keep the returned guard alive with the
/// allocation. The memory limit cannot account for allocations a host
/// function deliberately hides from this context.
pub struct HostCtx<'gc, 'call> {
  root: &'call mut ExecRoot<'gc>,
  mc: &'gc Mutation<'gc>,
  package: &'call Package,
  builtins: &'call Builtins,
}

impl<'gc, 'call> HostCtx<'gc, 'call> {
  /// The GC `Mutation` context for allocating new `Gc` pointers.
  pub fn mc(&self) -> &'gc Mutation<'gc> {
    self.mc
  }

  /// Allocate a heap-backed value via the execution's chokepoint, charging the
  /// tracker for the payload's direct Rust-heap storage.
  pub fn alloc_heap(&mut self, value: SLVal<'gc>) -> Value<'gc> {
    self.root.alloc_heap(self.mc, value)
  }

  /// Push a value onto the execution's value stack.
  pub fn push(&mut self, value: Value<'gc>) {
    self.root.push_value(value);
  }

  /// Pop a value from the execution's value stack.
  pub fn pop(&mut self) -> Result<Value<'gc>, String> {
    self.root.pop()
  }

  pub(crate) fn push_host_frame_at(&mut self, callable: (u32, u32), stack_base: usize) {
    self.root.push_host_frame(callable, stack_base);
  }

  pub(crate) fn stack_len(&self) -> usize {
    self.root.stack.len()
  }

  /// Reserve temporary Rust-heap memory against this execution's memory
  /// limit before allocating it.
  ///
  /// The returned guard is tied to this execution's memory tracker and releases
  /// automatically. Keep it alive until the corresponding temporary allocation
  /// is dropped or returned from the host function. Returned `SLVal` storage is
  /// immediately adopted by the normal [`Accounted`] allocation chokepoint.
  pub fn reserve_memory(&self, bytes: usize) -> Result<MemoryReservation, String> {
    self.root.reserve_memory(self.mc, bytes)
  }

  /// Reconcile a reservation with the capacity the allocator actually
  /// supplied. This is necessary because `try_reserve_exact` is permitted to
  /// allocate more capacity than requested.
  pub fn reconcile_reservation(
    &self,
    reservation: &mut MemoryReservation,
    bytes: usize,
  ) -> Result<(), String> {
    self.root.reconcile_reservation(self.mc, reservation, bytes)
  }

  /// Schedule a SafeLisp callable (`FunctionRef` or `Partial`) with the given
  /// arguments and return immediately. The callback result will be left on top
  /// of the VM stack before this host frame is resumed again.
  pub fn call(&mut self, callable: Value<'gc>, args: &[Value<'gc>]) -> Result<(), String> {
    let arity =
      u16::try_from(args.len()).map_err(|_| format!("too many call arguments: {}", args.len()))?;
    {
      let frame = self.root.current_host_frame_mut()?;
      if frame.awaiting_call {
        return Err("host frame already has a pending call".to_string());
      }
      frame.awaiting_call = true;
    }
    for arg in args {
      self.root.stack.push(*arg);
    }
    self.root.stack.push(callable);
    self
      .root
      .call_dynamic(self.mc, self.package, self.builtins, arity)
  }
}

#[cfg(test)]
mod interpreter_tests;
