# Type system: future directions

Design notes recorded during the v1 type-system design. Nothing here is
scheduled; this file exists so v1 choices stay compatible with these
directions and so the reasoning doesn't have to be rediscovered.

## Small follow-ups

- **Typed WASM signatures.** The WASM backend emits uniform tagged
  signatures; with checker output available, it could emit proper typed
  signatures and eventually drop tags for known-typed values, in function args,
  locals, and the stack.
- **Closure transform simplification.** Since bindings are immutable,
  captured variables can no longer be reassigned — the transform's
  cell-wrapping of captures is unnecessary and captures could be passed as
  plain values.
- **Collision-free names for lifted functions.** `mangle_closure_name` uses
  only the nested function's local name, so same-named nested functions in
  different lexical scopes are both lifted as (for example)
  `get:(closure)`. Module indexing silently keeps the last definition, causing
  earlier closures to invoke the wrong body. Generate lifted names from a
  unique lexical path or transform-owned ID, and reject duplicate generated
  names as a defensive check. The regression should define two closure
  factories with different nested `get` implementations and verify that each
  returned closure invokes its own body.
- **Remove `AST::SetCell` / `Instruction::SetCell`.** No longer produced
  from source since `set!` was removed; kept only for serialization compat.
- **Nested self-recursion.** A nested `fn` can't call itself: the transform
  lifts it under a mangled name (`mangle_closure_name`), so a self-call
  resolves to an unrelated top-level name, and the checker correspondingly
  refuses to bind the fn's own name in its body. Fix the transform's name
  resolution, then bind the name in the checker.
- **Expression-level type ascription** (e.g. `(the (List Int) expr)`). The
  no-unresolved-variables rule can currently only be satisfied via `let`
  annotations; an ambiguous expression in a non-`let` position (always dead
  code under the strict rule) can only be deleted. Only needed if a non-dead
  ambiguous expression ever arises.
- **Opaque nominal types for host handles** (e.g. `Rng` instead of
  `(Cell Int)` for `rand.rng`), if letting users `std.set!` an RNG's state
  proves to be a footgun. Runtime representation unchanged; only the
  checker's name for it.
- **Preludes / import-into-namespace**, so `+` can be used unqualified
  instead of `std.+`.
- **String indexing** could return as an `Index` trait impl if associated
  types ever land (see below); it was removed from `std.idx` precisely to
  avoid needing them.

## Extending toward user-defined traits and types

The load-bearing fact about the v1 trait system is *why* it's cheap:
**traits are erased constraints.** The checker only ever asks "may `A` be
`Int` here?" — it never decides *which code runs*. The polymorphic behavior
of `+` et al. lives in the builtins' runtime tag-dispatch, which predates the
type system. Everything below is a question of whether that erasure property
survives.

### User-defined types come first

Marker/constraint traits (below) are nearly useless without user-defined
types, because the set of types is otherwise closed (`Int`, `Float`,
`String`, `Bool`, `List`, `Cell`, `Fn`) and there's nothing to classify.
User-defined algebraic/record types are the larger, prerequisite feature and
create the actual demand for user traits. Sequencing: types first, then
traits.

### Tier 1 — constraint traits (no methods): cheap, erased

`(trait Ord)` + `(impl Ord Int)`, usable in `where` clauses. Implementation
against the v1 checker is mechanical:

- `Trait` stops being a closed Rust enum and becomes an interned name.
- The hardcoded membership table becomes a map populated by a pass-0 walk
  over `trait`/`impl` forms.
- Coherence check: no duplicate `impl` for the same `(trait, type)`.

Nothing downstream of the checker changes — still fully erased. Roughly a day
or two of work. v1 keeps all membership checks funneled through a single
`satisfies` function so this stays a one-file change.

### Tier 2 — traits with methods: the cliff (breaks erasure)

`(trait Show (fn show (a:Self) ->String))` with per-type impls. Checker-side
additions are moderate and reuse v1 machinery: `Self` is a distinguished
rigid var, method signatures are schemes, calling `(show x)` is
instantiation + unification, plus coherence checks (impl signature matches
declaration; no overlapping impls). Call it a doubling of `typecheck.rs`,
conceptually nothing new.

The real cost is that `(show a)` in a generic body where `a:A` is rigid
**cannot be resolved at compile time** — the checker must now influence which
code runs, which breaks erasure and forces a dispatch strategy. This is
codegen work and belongs with a deliberate bytecode phase.

### Dispatch strategies for method-bearing traits

Three options, in increasing SafeLisp-fit and decreasing precision:

- **Monomorphization** (Rust): specialize each generic function per concrete
  instantiation. Whole-program transform, heavy codegen. Recommend ruling
  out for SafeLisp.
- **Dictionary passing** (Haskell): generic functions gain hidden leading
  parameters carrying the impl's function refs; `(show a)` calls through the
  dictionary. This is structurally *the same trick as the closure transform*
  — an AST→AST pass adding hidden leading params and threading values via
  `PartialApply`/`FunctionRef`; a dictionary can be a list of function refs.
  No new bytecode instructions, but it changes generated code. Likely the
  right path precisely because its twin already exists in `closure.rs`.
- **Runtime tag dispatch**: a package-level table keyed on
  `(trait, method, value-tag)`, consulted at call time — the smallest change
  and how existing builtins already behave, but coarse: tags can't
  distinguish `(List Int)` from `(List String)`, so impls are per-tag and
  dispatch keys only on the receiver.

### Two smaller cliffs to avoid stumbling into

- **Conditional impls** — `(impl Eq (List A) where ((A Eq)))`. Recursive
  membership ("is `(List (List Int))` Eq?") turns `satisfies` into a small
  constraint solver with termination concerns. Tightening `Eq` — considered
  and rejected for v1, which makes `Eq` universal — would secretly be this
  feature; if it's ever revisited, do it as one hardcoded recursive case,
  not a general mechanism.
- **Associated types** — e.g. an `Index` trait with an `Output` type, which
  v1 deliberately avoided by making `std.idx` list-only. Adds type-level
  functions and projection normalization to the unifier; the single biggest
  jump in checker complexity here.

### Does v1 constrain any of this?

No. Rigid vars carrying bound sets, instantiation-time bound checking, and
schemes are exactly the pieces a method-bearing trait system reuses; v1
paints no corners. The only forward-looking hygiene v1 observes is routing
trait membership through one `satisfies` function so the enum→interned-name
swap stays local.
