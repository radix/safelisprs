# Plan 3: The SafeLisp type system

Static, mandatory types for SafeLisp, checked after parsing and before the
closure transform. One checker serves both backends (interpreter and WASM).
Type errors halt compilation.

Goals:

- Mandatory annotations on function parameters and return types; inference
  for `let` bindings.
- Parametric polymorphism (generic functions, generic `List`/`Cell`).
- A small trait system so `+`, `==`, `concat`, etc. are polymorphic without
  overloading — each builtin has exactly one signature.
- Higher-order generics work: `std.map` over a polymorphic function, closures
  passed as values.

Non-goals (for now):

- Typed bytecode. The checker validates and then gets out of the way; codegen
  is unchanged.
- User-defined types or traits; associated types.

Preconditions (verify before starting):

- `src/parser.rs` is a hand-written lexer + recursive-descent parser (not
  `atoms`-based), with `#` line comments and byte-span tracking on tokens.
  Special forms: `let`, `fn`, `if`, `block`. There is no `set!` form.
- Variables are immutable; mutation goes through the builtins `std.cell`,
  `std.get`, `std.set!`, and `SLVal::Cell` is a first-class runtime value.
- Top-level functions are referenceable as values (bare name in value
  position; qualified names parse to `AST::FunctionRef`).
- `std.idx` is list-only (no `String` support).

If any of these doesn't hold, stop and flag it.

---

## 1. Surface syntax

### 1a. New tokens

Add to the lexer: `Colon` (`:`) and `Arrow` (`->`) as dedicated tokens.
Inside a symbol-character run, `:` or `->` terminates the current symbol and
is emitted separately, so `a:Int` ≡ `a : Int` and `->Int` ≡ `-> Int`.
Qualified names (`std.+`) are unaffected (no `:` or `->` in them). Note `-`
followed by `>` must lex as `Arrow`, while `-` followed by a digit is still a
negative number and `-` alone is still a symbol.

### 1b. Grammar extensions

```
let        := "let" Sym [":" Type] Expr        -- annotation optional
fn         := "fn" Sym "(" Param* ")" ["->" Type] ["where" "(" Bound* ")"] Expr+
Param      := Sym ":" Type                     -- annotation mandatory (see §7 for migration staging)
Bound      := "(" TypeVarName TraitName+ ")"   -- e.g. (A Add), (B Add Eq)

Type       := Sym                              -- Int, Float, String, Bool, Void, or a type var
            | "(" Sym Type* ")"                -- application: (List Int), (Cell T)
            | "(" "Fn" "(" Type* ")" "->" Type ")"
```

An elided `fn` return type means `Void`. Uppercase symbols that aren't
built-in type names (`Int`, `Float`, `String`, `Bool`, `Void`, `List`,
`Cell`, `Fn`) are type variables. Trait names in `where` clauses are parsed
as plain strings; the checker validates them against the known trait set.

### 1c. Examples

```lisp
# Monomorphic; elided return type is Void (result discarded)
(fn main () (std.+ 1 2))
(fn main () ->Int (std.+ 1 2))

# Polymorphic identity
(fn id (a:A) ->A a)

# Trait bounds are explicit
(fn double (a:A) ->A where ((A Add)) (std.+ a a))

# Higher-order: top-level fn passed by name
(fn sq (x:Int) ->Int (std.+ x x))
(fn main () ->(List Int) (std.map (std.range 0 5) sq))

# Nested fn as a value; let infers (Fn (Int) -> Int)
(fn main () ->(List Int)
  (let dbl (fn dbl (x:Int) ->Int (std.+ x x)))
  (std.map (std.range 0 5) dbl))

# Annotated let
(fn main () ->Int
  (let xs:(List Int) (std.list))
  (std.len xs))

# Mutation via cells is ordinarily typed — no special rules
(fn main () ->Int
  (let c (std.cell 1))
  (std.set! c (std.+ 1 (std.get c)))
  (std.get c))
```

### 1d. AST changes

```rust
pub enum TypeAst {
  Named(String),                        // Int, List, a type var, ...
  Apply(String, Vec<TypeAst>),          // (List Int), (Cell T)
  Fn(Vec<TypeAst>, Box<TypeAst>),       // (Fn (Int Int) -> Int)
}

pub struct Bound {
  pub var: String,                      // "A"
  pub traits: Vec<String>,              // ["Add"] — validated by the checker
}

pub struct Function {
  pub name: String,
  pub params: Vec<(String, Option<TypeAst>)>,  // Option: see below
  pub return_type: Option<TypeAst>,            // None → Void
  pub bounds: Vec<Bound>,
  pub code: Vec<AST>,
}

// AST::Let gains an optional annotation:
Let(String, Option<TypeAst>, Box<AST>),
```

Parameter annotations are *syntactically* mandatory (the parser rejects a
bare param — after the migration window, §7), but the struct keeps
`Option<TypeAst>` permanently so the closure transform can synthesize capture
parameters without inventing type syntax it can't know. (The alternative — a
required `TypeAst` with the transform synthesizing placeholder syntax — was
rejected for exactly that reason.)

---

## 2. Types, type variables, and traits

All internal to the new `src/typecheck.rs`.

### 2a. Representation

```rust
pub enum Type {
  Int, Float, String, Bool, Void,
  Cell(Box<Type>),
  List(Box<Type>),
  Fn(Vec<Type>, Box<Type>),
  Var(TvRef),
}

pub type TvRef = Rc<RefCell<TypeVar>>;

pub enum TypeVar {
  /// An inference variable: not yet known. Cloning a `Type` clones the `Rc`,
  /// so all mentions share one state; linking is visible everywhere.
  /// `origin` describes where the var was created (a `let` name, or a
  /// description of the expression) so an ambiguity error (§4g) can point at
  /// it. `None` for vars with no obvious source location.
  Unbound { id: usize, bounds: Vec<Trait>, origin: Option<String> },
  /// A declared type variable being checked *inside its own function's
  /// body*. Unifies only with itself. Carries the `where`-clause bounds.
  Rigid { name: String, bounds: Vec<Trait> },
  /// Resolved: this variable IS the linked type (union-find by indirection).
  Link(Type),
}
```

`Rc<RefCell<_>>` gives the shared in-place mutation that union-find needs.

**Rigid vs. Unbound is the soundness linchpin.** When checking the body of
`(fn id (a:A) ->A a)`, `A` is *rigid*: the body must work for every `A`, so
`A` unifies with nothing but itself. `(fn id (a:A) ->A 5)` is an error —
`Int` does not unify with rigid `A`. Callers never see rigid vars: each call
site *instantiates* the signature, replacing its rigid vars with fresh
`Unbound` vars (carrying the same bounds), which then unify freely.

### 2b. Unification

`unify(a, b)` — structural, with these variable rules:

- Chase `Link`s first (and path-compress).
- `Unbound` vs anything: **occurs check** (error on `A` ~ `(List A)`), then
  check bounds (below), then link.
- `Unbound` vs `Unbound`: link one to the other, merging bound sets.
- `Unbound(bounds)` vs concrete type `T`: every bound must be satisfied by
  `T` (§2c) or error, then link.
- `Unbound(bounds)` vs `Rigid(declared)`: every required bound must appear in
  `declared`, else error — this is the "you used `A` where `Add` is required
  but didn't declare `where ((A Add))`" case. Then link the unbound var to
  the rigid one.
- `Rigid` vs `Rigid`: equal only if the same variable.
- `Rigid` vs concrete: error.

Enforcing bounds at link time, with the rigid-var rule above, is the entire
"explicit bounds" mechanism — no separate bounds-inference or
declared-vs-used pass exists.

### 2c. Traits

```rust
pub enum Trait { Add, Sub, Eq, Concat, Slice }
```

| Trait | Satisfied by | Used by |
|---|---|---|
| `Add` | `Int`, `Float` | `std.+` |
| `Sub` | `Int`, `Float` | `std.-` |
| `Eq` | every type | `std.==` |
| `Concat` | `String`, `(List A)` | `std.concat` |
| `Slice` | `String`, `(List A)` | `std.slice` |

Route all membership checks through a single `satisfies(&Type, Trait) ->
bool` function (a future user-defined-trait extension swaps its
implementation; keep the enum from leaking all over the checker).

**`Eq` is universal — every type satisfies it** (decided, not provisional).
This matches the runtime's structural `==` (Rust's derived `PartialEq` on
`SLVal`), with two consequences that are accepted deliberately:

- **Function values compare by identity** (same function slot / same captured
  args), not behavior. Allowed; revisit only if it bites in practice.
- **`==` on a cyclic cell can diverge** (cell equality recurses into
  contents, and cells can form cycles). Accepted as-is — Rust's `Rc` has the
  same property — with no type-system or runtime mitigation planned.

Universality doesn't let you compare across types: `std.==` is
`(A A) -> Bool` with a *single* type variable, so `(std.== 1 "x")` fails
unification before `Eq` is ever consulted.

There is no `Index` trait: `std.idx` is list-only, so its signature is
ordinary (`((List A) Int) -> A`) and needs no trait or special-casing. (Had
string indexing existed, `idx`'s result type would depend on its argument
type, requiring associated-type machinery — that's why it was removed.)

---

## 3. Builtin signatures

### 3a. Table

`BuiltinSpec` gains a signature, expressed over `&'static` data:

```rust
pub struct BuiltinSignature {
  pub type_vars: &'static [(&'static str, &'static [Trait])], // e.g. [("A", &[Trait::Add])]
  pub params: &'static [TypeConst],
  pub rest: Option<&'static TypeConst>,   // variadic tail: all extra args unify with this
  pub ret: TypeConst,
}

pub enum TypeConst {
  Int, Float, String, Bool, Void,
  Cell(&'static TypeConst),
  List(&'static TypeConst),
  Fn { params: &'static [TypeConst], ret: &'static TypeConst },
  Var(&'static str),
}
```

At each call site the checker converts the signature to `Type`, creating one
fresh `Unbound` var per entry in `type_vars` (with its bounds).

| Builtin | Signature |
|---|---|
| `std.+` | `(A A) -> A` where `A: Add` |
| `std.-` | `(A A) -> A` where `A: Sub` |
| `std.==` | `(A A) -> Bool` where `A: Eq` |
| `std.concat` | `(A A) -> A` where `A: Concat` |
| `std.slice` | `(A Int Int) -> A` where `A: Slice` |
| `std.idx` | `((List A) Int) -> A` |
| `std.len` | `((List A)) -> Int` |
| `std.push` | `((List A) A) -> (List A)` |
| `std.range` | `(Int Int) -> (List Int)` |
| `std.list` | variadic: `rest = A`, returns `(List A)` (homogeneous) |
| `std.map` | `((List A) (Fn (A) -> B)) -> (List B)` |
| `std.cell` | `(A) -> (Cell A)` |
| `std.get` | `((Cell A)) -> A` |
| `std.set!` | `((Cell A) A) -> Void` |
| `rand.rng` | `(Int String) -> (Cell Int)` |
| `rand.roll!` | `((Cell Int) Int) -> Int` |

`num_params` stays for the runtime arity check; the checker uses
`params.len()` / `rest` for its own arity errors. No builtin needs
special-casing in the checker — every one goes through this table.

---

## 4. The checker

New module `src/typecheck.rs`, entry point:

```rust
pub fn typecheck(asts: &[AST], builtins: &[BuiltinSpec]) -> Result<(), TypeError>;
```

Called from `compiler::_compile_from_source` and the WASM entry point, after
parsing, **before** `transform_closures_in_module`. (Modules are currently
single-`"main"`; when real multi-module input arrives, the entry point takes
`(module_name, asts)` pairs and the signature map keys on
`(module, function)` — the design below already assumes that keying.)

### 4a. Two passes

**Pass 1 — signatures.** Collect every top-level `DefineFn` into
`HashMap<(module, name), Scheme>` without looking at bodies; add builtin
signatures. Duplicate names are an error. Because all signatures exist before
any body is checked, top-level mutual recursion needs no special handling.

A `Scheme` is the declared signature with one `Rigid` var per declared type
variable (bounds from the `where` clause). A `where` bound naming a variable
that doesn't appear in the signature, or a trait name outside the known set,
is an error.

**Pass 2 — bodies.** For each function: bind params to their declared types
(mentioning the rigid vars directly), then infer each body expression in
order. Non-final expressions are statements — inferred, result discarded.
The final expression unifies with the declared return type, **except** when
the declared return type is `Void`: then the final expression is treated
like a statement and its type is discarded. Code generation discards the
runtime value as well and returns an actual `Void`, so callers cannot observe
the body's final value.

### 4b. Inference is synthesis-only

There is no bidirectional checking mode. Since every parameter and return
type is annotated, a single `infer(env, expr) -> Result<Type, TypeError>`
walker plus `unify` at the constraint points (let annotations, argument
positions, `if` branches, return position) expresses everything.

### 4c. Environments and name resolution

`Env` is a lexically-scoped map from name to either a monomorphic `Type` or
a `Scheme`. Resolution must mirror the compiler exactly:

- `Variable(name)`: env lookup (instantiate if a `Scheme`); else
  current-module function (instantiate its `Scheme`); else error.
- `CallFixed(Bare(name), args)`: env lookup **first** (a local closure —
  its type must unify with a fresh `(Fn (argtypes...) -> ret)`), then
  current-module function. Locals shadow module functions, same as the
  compiler. Builtins and functions in other modules require qualification;
  there is no prelude or implicit import lookup.
- `CallFixed(Qualified(m, f), args)` / `FunctionRef(m, f)`: signature map.

### 4d. Inference rules

| Node | Rule |
|---|---|
| `Int`/`Float`/`String`/`Bool` | the corresponding type |
| `Variable`, `FunctionRef` | §4c |
| `Let(name, ann, expr)` | `T = infer(expr)`; if `ann` present, `unify(T, resolve(ann))`; bind `name` (see §4f for when it's a `Scheme`); result `T` |
| `DefineFn(f)` (expression position) | check the nested function (§4e); bind `f.name` to its `Scheme`; result: an instantiation of the scheme (matches the transform, which rewrites this node to `Let(name, closure)`) |
| `Call(callee, args)` | `Tc = infer(callee)`; `unify(Tc, Fn(map(infer, args), fresh))`; result `fresh` |
| `CallFixed(id, args)` | resolve per §4c; instantiate; arity check (respecting `rest` for variadics); unify each arg with its param (extra args unify with the instantiated `rest` type); result: instantiated return type |
| `If(c, t, e)` | `unify(infer(c), Bool)` — conditions are strictly `Bool` now, no truthiness; `unify(infer(t), infer(e))`; result: that type |
| `Block(body)` | statements + last, same as a function body (but no `Void`-discard: a block's type is its last expression's type) |
| `Cell`/`DerefCell`/`SetCell`/`PartialApply` | unreachable — these are produced only by the closure transform, which runs after checking. Internal error if encountered |

Type annotations (`resolve(ann)`): a `TypeAst` name resolves against built-in
type names first, then the type variables in lexical scope (§4e). A `let`
annotation may mention in-scope type vars but cannot introduce new ones.

### 4e. Nested functions

Nested `fn` is how closures are written, so the checker handles `DefineFn`
in expression position fully:

- **Type-variable scoping is lexical.** A type name in the nested signature
  that matches an in-scope type variable refers to that (rigid) variable —
  so a nested fn can capture an outer `a:A` and mention `A` itself. Names
  not already in scope become the nested fn's own rigid vars. A `where`
  clause may only bound the fn's *own* vars; bounding an enclosing one is an
  error.
- The body is checked in a child scope of the *enclosing* environment
  (captures are just env lookups), with params bound as usual.
- The nested fn's **own name is not bound inside its own body**: nested
  self-recursion doesn't survive the closure transform today (the function
  is lifted under a mangled name — `mangle_closure_name` — so a self-call
  would resolve to an unrelated top-level name). The checker rejects what
  the runtime can't run. Recursion works via top-level functions.
- The resulting `Scheme` generalizes over the fn's own rigid vars only —
  enclosing rigid vars stay fixed inside it.

### 4f. Generalization: `DefineFn` only

A `let` binding is polymorphic **only when its initializer is syntactically
a `DefineFn`** — then the bound name gets the function's declared `Scheme`,
and each use instantiates fresh (so `(let id (fn id (a:A) ->A a))` used at
`Int` and `String` both work). Every other initializer binds a monomorphic
type; its inference variables are shared across uses and fixed by the first
unification.

This replaces ML-style let-generalization and the value restriction
wholesale. It's sound by construction — the only schemas that exist are
declared ones; nothing inferred is ever generalized. In particular
`(let c (std.cell (std.list)))` is monomorphic, so the classic
polymorphic-mutable-cell unsoundness cannot arise. And nothing of value is
lost: with mandatory annotations, function definitions are the only
polymorphic values in the language.

### 4g. No unresolved inference variables

At the end of checking each function body (nested functions included), **no
`Unbound` inference variable may remain** — every one must have been `Link`ed
to a fully concrete type. If any survives, it's an error: the program left a
type genuinely undetermined.

```lisp
(fn main () ->Int
  (let empty (std.list))   # empty : (List ?A) — ?A never determined → error
  (std.len empty))
```

The fix is an annotation: `(let empty:(List Int) (std.list))`.

This is deliberately conservative (like Rust's "type annotations needed" and
Haskell's ambiguity error). Two things make it cheap here that make it
painful elsewhere:

- **No defaulting needed.** The literal-driven numeric ambiguity that forces
  Haskell/Rust to have a defaulting layer doesn't exist — the lexer types `1`
  as `Int` and `1.0` as `Float` from the start. The only vars that can go
  unresolved are things like the element type of a never-populated `std.list`
  or an unused alias of a polymorphic function.
- **The rule is strict, and therefore simple.** It applies everywhere,
  including expressions in statement position whose value is discarded — a
  discarded expression with an ambiguous type is dead code. This is *one*
  scan for remaining `Unbound` vars, rather than tracking which positions are
  "observed." The check runs per function; because inferred types never
  escape a function (§4c) and generalized schemes quantify over `Rigid` vars,
  not `Unbound` ones (§4f), every leftover `Unbound` is genuinely a
  monomorphic type the body failed to pin down.

The error uses each surviving var's `origin` hint (§2a) to name what to
annotate. One current limitation: annotations exist only on `let`, so an
ambiguous expression in a non-`let` position (always dead code under this
rule) can't be annotated in place — you delete it. An expression-level
ascription form is a possible future addition if that ever becomes a real
need.

---

## 5. Errors

```rust
pub struct TypeError { pub message: String, pub context: Vec<String> }
```

`context` is a stack pushed while walking: "in function `main`", "while
checking call to `std.+`", "while checking argument 1". Rendered:

```
TypeError: expected `Int`, got `String`
  while checking argument 1 of call to `std.+`
  in function `main`

TypeError: type `String` does not satisfy trait `Add`
  while checking call to `std.+`
  in function `main`

TypeError: type variable `A` requires trait `Add` here, but its bounds are only `[Eq]`
  add `Add` to the `where` clause of `double`
  while checking call to `std.+`
  in function `double`
```

The lexer records spans; threading them into `AST`/`TypeAst`/`TypeError` is
future work, not part of this plan.

---

## 6. Pipeline and files touched

```
source
  ↓ parser (extended with type syntax)
  ↓ typecheck::typecheck            ← NEW; errors halt
  ↓ closure::transform_closures_in_module   (unchanged)
  ↓ compiler::compile_module / wasm::compile (unchanged codegen)
```

- `src/parser.rs` — `Colon`/`Arrow` tokens; type grammar; `TypeAst`,
  `Bound`; `Function`/`AST::Let` extended.
- `src/typecheck.rs` — new: types, unification, traits, schemes, walker,
  builtin-signature conversion.
- `src/builtins.rs` — `signature` field on `BuiltinSpec`, populated for
  every builtin.
- `src/compiler.rs` — read the new `Function` shape (use `.0` of each param
  pair); call `typecheck` in `_compile_from_source`.
- `src/wasm.rs` — new `Function` shape; call `typecheck` after parsing.
- `src/closure.rs` — thread the new `Function` fields through lifting
  (capture params get `None` annotations; lifted functions keep the
  original's return type and bounds); otherwise unchanged.
- `src/lib.rs` — `pub mod typecheck;`.
- Tests — annotate the whole corpus (step 6 below).

---

## 7. Implementation order

Each step lands green on its own.

1. **Type syntax.** Lexer tokens; `:` annotations — *optional* at this stage
   (unannotated params allowed and simply unchecked until step 6) — `->`
   return types, type applications, `where` clauses. New `Function`/`Let`
   shapes threaded through `closure.rs`/`compiler.rs`/`wasm.rs`
   mechanically. Existing corpus still parses and runs.
2. **Checker core.** `Type`, `TypeVar`, `unify` (occurs check, bounds,
   rigid rules), traits, instantiation. Pure functions, heavy unit tests:
   `A ~ Int`, `(List A) ~ (List Int)`, occurs-check failure, rigid-vs-Int
   failure, bounds merge, missing-bound-on-rigid failure.
3. **Builtin signature table** (§3). Inert until the walker exists.
4. **The walker** (§4). Unit tests per §4d rule, plus: polymorphic id at two
   types, missing `where` bound, nested fn capturing an outer type var,
   `std.map` with a top-level fn ref, `Void` discard, `if` with non-Bool
   condition rejected, unresolved-variable error (§4g).
5. **Wiring** into both entry points behind a temporary flag (env var),
   since the corpus isn't annotated yet.
6. **Migrate the corpus.** Annotate every test source; flip params to
   mandatory in the parser; remove the flag — checker always on. Any test
   relying on truthy non-`Bool` `if` conditions gets fixed here.
7. **Docs, TODO, cleanup.**

## Verification

`cargo test` green with the checker unconditionally on; a deliberately
ill-typed program fails compilation with a readable error; the examples in
§1c all compile and run.
