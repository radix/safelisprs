# TODO

## Memory limits: tracking the full Execution footprint

The current memory limit only counts live `Gc` allocations (via
`gc_arena::Metrics::total_gc_allocation`). It misses (1) `ExecRoot`'s own
`Vec` overhead — `stack` and `frames` (each `Frame` currently clones the
function's `instructions: Vec<Instruction>`), and (2) per-`Gc`-box auxiliary
Rust-heap storage (`String` bytes, `List`/`Partial` `Vec` backings) that lives
outside the `GcBox` layout gc-arena records. Two staged failing tests pin the
gaps: `memory_limit_catches_unbounded_stack_and_frames` and
`memory_limit_catches_large_string_contents`.

The plan (each step independently testable; commit after each):

- [x] **1. Store `Frame`'s function by `(module, function)` index, not by
      clone.** Replace `Frame.function: Function` with `(u32, u32)` plus a
      `&'static Package`-style lookup at `step` time (the `Package` is already
      threaded through `step`'s callers). Removes ~120 bytes/call of cloned
      instructions — most of the `spin` test's runaway allocation. Independent
      win; shrinks the gap before accounting even lands.
- [x] **2. Add `MemoryTracker` + `Accounted<SLVal>` wrapper.** A per-`Execution`
      `Rc<MemoryTracker>` holds `external_bytes: Cell<usize>` and
      `limit: Cell<Option<usize>>`. Wrap `SLVal` in
      `#[derive(Collect)] #[collect(unsafe_drop)] struct Accounted<'gc> {
      value: SLVal<'gc>, #[collect(require_static)] tracker:
      Rc<MemoryTracker>, external_bytes: usize }`, `!Clone`. `Drop` calls
      `tracker.release(external_bytes)` (touches no `Gc` pointers — the
      `unsafe_drop` safety condition). `external_bytes` is
      `String::capacity()` / `Vec::capacity() * size_of::<Gc<_>>()`, etc.
      Centralize all `Gc<Accounted>` creation in one `ExecRoot::alloc_value`
      chokepoint so unaccounted values are hard to create by accident.
- [ ] **3. Track `stack`/`frames`/`locals` capacity via a `TrackedVec<T>`
      wrapper.** A `Vec` wrapper whose `push`/`reserve`/`Drop` update the
      `MemoryTracker` by `capacity() * size_of::<T>()`. Use it for
      `ExecRoot.stack`, `ExecRoot.frames`, and `Frame.locals`. This is what
      makes `memory_limit_catches_unbounded_stack_and_frames` go green.
- [ ] **4. Pacing + arithmetic polish.** Call
      `Metrics::adjust_debt(delta)` when external bytes grow/shrink so large
      strings pace incremental collection. Use checked arithmetic on the
      tracker. Move the per-step limit check to *after* the instruction
      (charge-then-check) so allocations are caught at the instruction that
      made them, not one instruction late. At this point both staged failing
      tests pass; flip them from `#[ignore]`-equivalent (they currently FAIL
      loudly) to normal passing tests.

## Medium priority

- [ ] **Prototype an AST `Folder`/visitor trait** to deduplicate the AST walkers (closure transform, patch_cell_access, wasm discover/count/compile_expr, compiler compile_expr). Validate it actually reduces LOC on 1-2 walkers before rolling out.

## Low priority / investigations

- [ ] **Rename `FuncDef` to `FuncDecl`** for consistency (it's metadata, not a body).

## WASM backend: supporting the full SafeLisp language

These items track the work needed for the WASM backend to reach feature
parity with the SLC compiler + interpreter. They're roughly ordered by
dependency, but many are independent.

### Types

- [ ] **String**: Add `AST::String` support. Strings require linear memory —
      allocate a region, store the bytes, and pass a pointer + length (not
      null-terminated) as the WASM value. The host (wasmtime linker) reads/
      writes strings from linear memory via `Memory::read`/`write`.

### Closures and cells

- [ ] **`FunctionRef`**: Use `ref.func` (function-references proposal) from
      the beginning. No function-table indirection.
- [ ] **`Cell` (mutable boxes)**: Cells wrap a value in a mutable reference.
      Represent as a WASM GC struct with one mutable field. Only Cells need
      GC, since they are the only things that can become circular. All other
      types are terminal and immutable. (This will also change if we ever
      introduce other mutable data structures like lists or arbitrary
      structs.)
- [ ] **`DerefCell` / `SetCell`**: `struct.get`/`struct.set` on the GC
      struct.
- [ ] **`PartialApply` (closures)**: A closure is a function + pre-bound
      args. Represent as a GC struct (function ref + captured values).
      `CallDynamic` then looks up the closure, pushes the remaining args,
      and calls. This needs to be GC-based because it can reference Cells,
      which are also GC-based.
- [ ] **`CallDynamic`**: Calling a value at TOS that may be a FunctionRef
      or a Partial. Requires runtime dispatch on the value's tag. In WASM
      this is a `br_table` or `if`-chain on the tag.
- [ ] **Closure transform**: Run the existing `closure.rs` transform (lifting
      nested functions, wrapping captures in Cells) — it should be suitable
      for later compilation to WASM. If we run into intractable issues we
      can update the core transform.

## Interpreter: collections & builtin redesign

Scope is the interpreter only for now (the WASM backend already has its own
builtin registry and is out of scope here). The builtin registry has been
redesigned; the remaining prerequisite for implementing `list` as an ordinary
builtin (rather than a special form) is the variadic-builtins work below.

### Lists (interpreter only)

- [x] **`List` value type**: add `SLVal::List(Vec<Gc<'gc, SLVal<'gc>>>)` and
      the matching owned `SLValue::List(Vec<SLValue>)`. Update `to_value` /
      `from_value`. Backed by a Rust `Vec`; immutable — no mutating operations.
- [x] **`list` builtin**: `(list 1 2 3)` syntax, variadic (including `(list)`
      for the empty list). Implemented as a builtin, _not_ a special form, so it
      can be used first-class (`(let mk list) (mk 1 2 3)`, passed higher-order).
- [x] **`concat` on lists**: extend the existing `concat` to concatenate two
      lists (in addition to strings). Mismatched types error.
- [x] **`==` on lists**: structural equality. Derived `PartialEq` already
      covers it for non-cell contents; note the cycle caveat (a list
      containing a cell referring back to the list would infinite-loop).
- [x] **`idx`**: `(idx list index)`. Negative indices work like Python
      (`-1` is the last element). Out-of-range is a runtime error. Also works
      on strings (index by `char`, returns a 1-char string).
- [x] **`push`**: `(push l v)` returns a _new_ list with `v` appended to the
      end.
- [x] **`slice`**: `(slice l start stop)` with Python `l[start:stop]`
      semantics — negative indices, clamped bounds. Three mandatory `Int` args
      for now (no omitted bounds).
- [x] **Tests**: interpreter-only tests for the above in `src/interpreter.rs`
      `mod test`. The shared `tests/test_eval.rs` suite runs against both
      backends, so don't add list tests there until WASM has lists.
