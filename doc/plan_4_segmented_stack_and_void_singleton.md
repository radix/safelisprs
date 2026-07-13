# Plan 4: Segmented value stack and a shared `Void` value

Keep the interpreter's single value stack, but divide it into frame-owned
segments with a `stack_base` recorded on every frame. Add one GC-rooted `Void`
value per execution and reuse it everywhere. Void functions discard their
body result at the frame boundary and return the shared value without making
a per-call allocation.

This preserves the useful invariant that every expression produces exactly
one stack value. Producing zero values for a Void expression is deliberately
out of scope: `Void` is a real language type, can be passed to polymorphic
operations such as `std.==`, and callers currently compile every expression
with a stack effect of `+1`.

## Goals

- Prevent callees from leaking temporary stack values into callers.
- Make the one-result-per-call invariant explicit and checked at `Return`.
- Avoid allocating a new `Gc<Accounted<SLVal::Void>>` for every Void call,
  uninitialized-local set, builtin result, or imported Void value.
- Let a Void return discard the body's final value without emitting
  `Pop; PushVoid; Return`.
- Preserve the current calling convention for builtins, dynamic calls,
  closures, host callbacks, and top-level execution.

## Non-goals

- A separate `Vec` operand stack inside every frame. That would add an
  allocation/tracking burden to recursive calls and require explicit argument
  and result transfers between stacks.
- Zero-slot Void expressions or stack-effect-aware expression compilation.
- Changing the WASM value representation. WASM already represents Void as an
  immediate payload/tag pair and does not allocate an interpreter value.
- Changing source-language Void semantics.

## Preconditions

Verify these before implementation:

- `ExecRoot` owns one global `stack` and a `frames` stack.
- `Frame` contains its function, locals, and instruction pointer, but no stack
  boundary.
- Function entry pops call arguments from the global stack into callee locals.
- Ordinary `Return` pops only the frame and leaves the result on the global
  stack.
- Void functions currently compile to discard the body value, push a newly
  allocated Void value, and return it.
- `HostCtx::call` relies on the called function leaving exactly one result on
  the global stack.

If the calling convention has changed, update this plan before proceeding.

---

## 1. Add one shared Void value per execution

Add a rooted value to `ExecRoot`:

```rust
struct ExecRoot<'gc> {
  void: Gc<'gc, Accounted<'gc>>,
  stack: TrackedVec<Gc<'gc, Accounted<'gc>>>,
  frames: TrackedVec<Frame<'gc>>,
  tracker: SharedTracker,
}
```

Create it once in the `Arena::new` initializer using the arena's mutation
context. Because it is stored in `ExecRoot`, it remains traced and valid for
the execution's lifetime. Its external-memory charge is zero.

Route all creation requests for `SLVal::Void` through this singleton. The
preferred shape is to make `ExecRoot::alloc_value(SLVal::Void)` return
`self.void`; this keeps the existing allocation chokepoint valid for builtins
and import paths without requiring every caller to remember a special API.
Non-Void values continue through `Gc::new` normally.

Use the same singleton for:

- Void function results;
- initialization of unused local slots;
- builtins returning Void, including `std.set!`;
- `SLValue::Void` imported through `call_slval` or other external entry points;
- any retained `PushVoid` bytecode instruction.

The singleton is per execution rather than process-global because `Gc`
values are branded to one execution's arena and cannot safely cross arenas.

---

## 2. Segment the global value stack by frame

Add `stack_base` to `Frame`:

```rust
struct Frame<'gc> {
  function: FrameFunc,
  locals: TrackedVec<Gc<'gc, Accounted<'gc>>>,
  stack_base: usize,
  ip: usize,
}
```

On function entry:

1. Pop the call-site arguments into the callee's local slots as today.
2. Record `self.stack.len()` as the new frame's `stack_base` after all call
   arguments (and a dynamic callable, when applicable) have been removed.
3. Push the new frame.

Everything below `stack_base` belongs to the caller. Everything at or above
it belongs to the callee. Nested calls repeat this process on the same backing
vector, retaining its capacity across returns and recursive calls.

Apply the same rule to indexed production frames and inline test frames.

---

## 3. Make returns enforce the frame boundary

Split returning bytecode into value and Void forms:

```rust
Return,
ReturnVoid,
```

### `Return`

An ordinary return must find exactly one value in the current frame's stack
segment. Conceptually:

```rust
let base = current_frame.stack_base;
require_stack_len(base + 1)?;
let result = stack.pop().unwrap();
stack.truncate(base);
frames.pop();
stack.push(result);
```

Checking for exactly one value turns compiler or malformed-bytecode stack
imbalances into errors at the call boundary. Truncation is still useful as a
defensive cleanup, but it must not silently accept extra values in normal
execution.

### `ReturnVoid`

A Void return validates the expected frame stack shape, discards the body's
result without copying or inspecting it, removes the frame, and pushes the
execution's shared Void value:

```rust
let base = current_frame.stack_base;
require_stack_len(base + 1)?;
stack.truncate(base);
frames.pop();
stack.push(shared_void);
```

The caller still receives one logical value, so existing expression
compilation, dynamic calls, and `HostCtx::call` retain their `+1` stack-effect
assumption. No new Void allocation occurs.

Top-level execution uses the same rule. Once its frame returns, the frame
stack is empty and the single result—possibly the shared Void—is left for
`run`, `run_until_done`, or `run_for_duration` to extract.

---

## 4. Emit `ReturnVoid` for Void functions

The compiler already knows whether a function's return type is omitted or is
explicitly `Void`. Change function compilation so that:

- non-final body expressions are compiled and popped as today;
- a non-Void final expression is followed by `Return`;
- a Void final expression is left in its frame segment and followed directly
  by `ReturnVoid`.

This replaces the current Void tail sequence:

```text
...final expression
Pop
PushVoid
Return
```

with:

```text
...final expression
ReturnVoid
```

`ReturnVoid` owns the responsibility for discarding the final value and
publishing the shared Void result.

If serialized-bytecode compatibility matters, retain `PushVoid` and continue
to interpret it using the singleton, but stop emitting it for function
returns. Otherwise it may be removed once no code path uses it. Document any
bytecode-format compatibility decision in the implementing commit.

The WASM backend should keep its current immediate Void return behavior; it
does not use the interpreter frame stack or allocate `Gc` values.

---

## 5. Preserve builtin and callback behavior

The segmented global stack should require little change to builtin calls:

- A builtin invoked by guest bytecode pops arguments and pushes its result in
  the current frame's segment.
- A builtin returning Void obtains the shared singleton through
  `alloc_value(SLVal::Void)`.
- `HostCtx::call` records frame depth, pushes arguments, enters the callee,
  runs until the original depth is restored, and pops the one returned value
  as today. `ReturnVoid` supplies the shared singleton in that slot.

Add explicit tests for `HostCtx::call` with both value-returning and
Void-returning callbacks. This path is sensitive to call-boundary changes and
should not rely only on ordinary guest-call coverage.

---

## 6. Tests

Implement in this order so failures identify the responsible layer.

### Singleton tests

- Repeated `alloc_value(SLVal::Void)` calls return the same GC handle.
- Initializing locals in many calls does not allocate one Void per frame.
- Importing `SLValue::Void` uses the execution's singleton.
- Two different `Execution`s have distinct arena-local singleton handles.

Prefer pointer-identity or allocation-count assertions that do not depend on
GC scheduling.

### Frame-boundary tests

- A value-returning callee transfers exactly its result to its caller.
- A Void-returning callee discards a non-Void body result and transfers the
  shared Void value.
- Nested and recursive calls restore each caller's stack segment correctly.
- Dynamic calls and partial applications obey the same boundary.
- Malformed inline bytecode returning zero or multiple values reports a clear
  stack-imbalance error rather than consuming caller values.
- A top-level Void function completes with `SLValue::Void` and leaves no stack
  garbage.

### Behavioral regressions

- Keep the cross-backend regression where two Void functions have different
  hidden body values but compare equal as Void.
- Cover both omitted return types and explicit `->Void`.
- Verify `std.set!` still returns Void.
- Verify repeated Void calls do not grow live GC allocation due to Void
  objects.
- Keep existing stack-length, memory-limit, callback, and GC tests green.

---

## 7. Implementation order

1. Add the per-execution Void singleton and route existing Void allocations
   through it. Run the full suite; this step is independently useful.
2. Add `Frame::stack_base` and initialize it on every frame-entry path.
3. Strengthen ordinary `Return` to validate and restore its frame segment.
4. Add `ReturnVoid` and its interpreter behavior.
5. Change bytecode compilation of Void functions to emit `ReturnVoid`.
6. Add callback, malformed-bytecode, recursion, and allocation regressions.
7. Decide whether `PushVoid` remains for serialized compatibility, then update
   instruction documentation and stale comments.
8. Run formatting, lint, the complete test suite, and `git diff --check`.

Suggested verification:

```sh
cargo fmt -- --check
cargo clippy --all-targets -- -D warnings
cargo test
git diff --check
```

## Accepted design decisions

- Use a segmented global stack rather than a separate stack allocation per
  frame.
- Void calls still produce one logical stack value.
- The logical value is one arena-local shared singleton, not a new allocation
  per call.
- Void return handling discards the body's result at the frame boundary.
- WASM remains independent and continues returning its immediate Void pair.
