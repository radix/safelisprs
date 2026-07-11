# Plan 1: Language changes — first-class functions, explicit cells, list-only `idx`

Three self-contained changes to the SafeLisp language. No new subsystems; all
work happens in the existing parser, compiler, and builtins. Each change is
independently committable, in the order given.

Preconditions (verify before starting — this plan describes edits against
this state):

- `src/parser.rs` is `atoms`-based; special forms are `let`, `fn`, `if`,
  `set!`, `block`.
- `std.idx` accepts both `(List, Int)` and `(String, Int)`.
- In the compiler, a `Variable` that isn't a local is a compile error
  ("Function accesses unbound variable") — there is no way to reference a
  top-level function as a value.

---

## 1. First-class references to top-level functions

### Current behavior

Only a *nested* `fn` produces a callable value (the closure transform
rewrites it to a `PartialApply`). A top-level function's name in value
position — `(std.map xs double)` where `double` is top-level — fails to
compile: `compile_expr`'s `AST::Variable` arm only checks locals.

### New behavior

- **Bare identifier in value position**: resolve against locals first, then
  the current module's functions. A module-function hit compiles to
  `Instruction::MakeFunctionRef((module, name))`. Locals shadow module
  functions — the same order `CallFixed(Bare(..))` already uses in
  `compile_expr`.
- **Qualified identifier in value position** (`other.helper`, `std.len`):
  the parser currently turns these into `AST::Variable("std.len")`, which can
  never resolve. Instead, parse a dotted symbol in value position directly to
  `AST::FunctionRef(module, name)` (this AST variant already exists; it is
  currently only produced by the closure transform). Symbols containing `.`
  are thereby reserved for qualified references — they can't name locals.

### Implementation notes

- `compile_module` knows the module's `DefineFn` names (and compilation runs
  after the closure transform, so lifted functions are top-level too).
  Collect that name set and thread it into `compile_function` /
  `compile_expr` so the `Variable` arm can do the second lookup.
- The closure transform needs no change: a `Variable` naming a top-level
  function is in neither `locals` nor `environment`, so the transform already
  leaves it untouched (it does not become a capture).
- Apply the equivalent resolution change in the WASM backend (`src/wasm.rs`
  compiles the same post-transform AST and has the same `Variable`
  resolution).
- **Builtin references**: builtins are injected into module function tables
  (`inject_builtin_specs`), so a `FunctionRef` to a builtin *should* be
  callable through the dynamic-call path. Verify with a test like
  `(fn main () (std.map (std.list (std.list 1) (std.list 1 2)) std.len))`.
  If builtin refs turn out not to work through `CallDynamic` / `ctx.call`,
  restrict qualified value-position refs to user modules (error for builtin
  modules) and file the runtime fix as a follow-up rather than expanding
  scope here.

### Tests

- Top-level fn passed to `std.map` by bare name; called via a local variable;
  shadowing (a local named like a module fn wins).
- Qualified user-module fn as a value across modules (if multi-module sources
  are testable; otherwise same-module qualified).
- Builtin as a value (see above).
- Existing "unbound variable" error still fires for names that are neither
  locals nor module functions.

---

## 2. Explicit cells; `set!` removed

### Current behavior and why it changes

`(set! x v)` mutates a variable — but only actually works when the closure
transform happened to wrap `x` in a Cell (i.e. `x` is captured by some nested
fn). `set!` on a non-captured local fails at runtime with "Not a cell" (there
is a test asserting exactly this). Whether assignment works thus depends on
an invisible, unrelated property of the program. Additionally, if a variable
*holds* a Cell value (e.g. from `rand.rng`) and isn't captured, `set!` writes
into that cell's interior instead of rebinding — a third behavior.

Resolution: **variables become immutable bindings**; all mutation goes
through explicit, first-class cells.

### New builtins

| Builtin | Behavior |
|---|---|
| `std.cell` | 1 arg: wrap the value in a new Cell; returns the Cell |
| `std.get` | 1 arg: a Cell; returns its current contents (error if not a Cell) |
| `std.set!` | 2 args: a Cell and a value; stores the value in the Cell; returns Void (error if arg 0 is not a Cell) |

The runtime already has everything needed: `SLVal::Cell` is a first-class
value, `rand.rng` shows a builtin allocating a Cell
(`SLVal::Cell(Gc::new(mc, RefLock::new(CellContents::new(v))))`), `std.idx`
shows returning an interior value handle, and `rand.roll!` shows mutating a
cell argument in place via `Gc::write`. `std.cell` and `std.set!` are
`contextual` builtins; `std.get` is `contextual_value` (it returns an
existing handle rather than allocating).

### Removal

- Delete the `set!` branch and `parse_set` from `src/parser.rs`.
- `AST::SetCell` and `Instruction::SetCell` are no longer produced from
  source. **Leave both in place** (the instruction is part of the serialized
  format); removing them is deliberately out of scope.
- The closure transform is untouched. Its cell-wrapping of captured variables
  becomes an invisible implementation detail — captures can no longer be
  reassigned at the source level, so no source-visible behavior depends on
  it.

### Migration

~15 `set!` call sites in the test corpus (interpreter tests, closure tests,
eval tests). The rewrite pattern:

```lisp
(let x 1)  ... (set! x 2)      ... x
;; becomes
(let x (std.cell 1)) ... (std.set! x 2) ... (std.get x)
```

Specific attention:

- The shared-counter closure tests: the closure now captures the cell value;
  behavior (shared mutation) is preserved.
- `set_cell_on_non_cell_errors` (interpreter tests): the scenario it guards —
  `set!` on a non-captured local — no longer exists. Replace it with a test
  that `std.set!` on a non-Cell value errors.
- `cycle_is_collected` (interpreter tests): this test's *purpose* is to build
  a reference cycle from source-level constructs and prove the GC collects
  it. Preserve that intent: `(let c (std.cell 0))`, define a closure
  capturing `c`, then `(std.set! c the-closure)` — the Cell holds the Partial
  and the Partial's captures reach the Cell. Verify the test still exercises
  collection (its assertions on allocation counts may need adjusting).

### Tests

- Get/set roundtrip; `std.set!` returns Void; cells shared across closures
  mutate visibly; `std.get`/`std.set!` on non-Cells error; nested cells
  (`(std.cell (std.cell 1))`) behave sanely.
- `(set! x 2)` no longer parses (it becomes a call to an unknown `set!`
  function — assert on whatever error that produces).

---

## 3. `std.idx` becomes list-only

### Change

Remove the `(String, Int)` arm from `std.idx` in `src/builtins.rs` (similar
to Rust, which has no string indexing). The error message becomes
"idx: expected (List, Int), got (...)".

Rationale: string `idx` returns a one-character `String` while list `idx`
returns an element — the result *type* depends on the argument type, which
complicates future typing of the builtin for no compelling use-case.
Character access remains available through `std.slice`, which keeps its
`String` support (its result type equals its argument type):
`(std.idx s i)` ≡ `(std.slice s i (std.+ i 1))` for non-negative `i`.

### Migration

The handful of string-index tests (in `builtins.rs` and `interpreter.rs`
test modules):

- In-range accesses: rewrite to `std.slice` with equivalent concrete bounds
  (the tests know their strings; compute the indices by hand — note negative
  indices differ: `idx` counted from the end and *errored* out of range,
  while `slice` normalizes and *clamps*).
- Out-of-range *error* tests for string idx: delete (slice clamps instead of
  erroring; there is no equivalent behavior to assert).
- Add one test asserting `std.idx` on a String now errors.

---

## Verification

`cargo test` green after each of the three changes. No changes to the
serialized package format, the instruction set, or the closure transform.
