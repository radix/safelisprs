# SafeLisp Type System

Static, mandatory types for SafeLisp, checked after parsing and before the
closure transform. One checker serves both backends (interpreter and WASM).
Type errors halt compilation.

Goals:

- Mandatory annotations on function parameters and return types; inference for
  `let` bindings.
- Parametric polymorphism (generic functions, generic `List`/`Cell`).
- A small trait system so `+`, `==`, `concat`, etc. are polymorphic without
  overloading — each builtin has exactly one signature.
- Higher-order generics work: `std.map` over a polymorphic function, closures
  passed as values.

Non-goals (for now):

- Typed bytecode. The checker validates and then gets out of the way; codegen
  is unchanged. Typed WASM signatures, tag elision, etc. are follow-ups.
- User-defined types, associated types, trait implementations for user types.

---

## 1. Prerequisite language changes

Two changes to the untyped language land first, independent of the checker.
Both shrink the type system's problem space.

### 1a. First-class references to top-level functions

Today only a *nested* `fn` produces a callable value; a top-level function
name in value position is a compile error (`compiler.rs` errors on any
`Variable` that isn't a local). With mandatory types we want
`(std.map xs double)` where `double` is a top-level function, so we add:

- **Bare identifier in value position**: resolve against locals first, then
  the current module's functions. A module-function hit compiles to
  `Instruction::MakeFunctionRef((module, name))`. Locals shadow module
  functions, matching the existing `CallFixed` resolution order.
- **Qualified identifier in value position** (`other.helper`, `std.len`):
  parse to `AST::FunctionRef(module, name)` directly. Builtins are injected
  into module function tables like ordinary functions, so a `FunctionRef` to a
  builtin should be callable via `CallDynamic` — verify with a test
  (`(std.map xs std.len)`); if builtin refs don't work through the dynamic
  path, restrict qualified refs to user modules and fix the runtime as a
  follow-up.

### 1b. Explicit cells; `set!` removed

Variables become **immutable bindings**. All mutation goes through explicit,
first-class cells, via three new builtins:

| Builtin | Signature |
|---|---|
| `std.cell` | `(A) -> (Cell A)` |
| `std.get` | `((Cell A)) -> A` |
| `std.set!` | `((Cell A) A) -> Void` |

The `set!` special form is removed from the parser. The shared-counter idiom
becomes:

```lisp
(fn make-counter () ->(Fn () -> Int)
  (let count (std.cell 0))
  (fn tick () ->Int
    (std.set! count (std.+ 1 (std.get count)))
    (std.get count)))
```

Why this shape:

- `set!` on a bare variable cannot be typed coherently: whether it rebinds
  the variable or writes through a cell it happens to hold depends on whether
  the closure transform cell-wrapped it, and the non-captured case is a
  runtime error today ("Not a cell"). Explicit cells make mutation an
  ordinary, fully-typed operation — a well-typed program can't hit that error.
- The three builtins are plain generic functions; the checker needs **zero**
  mutation-specific rules or special forms.
- `rand.rng` returning `(Cell Int)` stops being an anomaly — it's the idiom.
  `rand.rng : (Int String) -> (Cell Int)` and
  `rand.roll! : ((Cell Int) Int) -> Int` keep their current types.

Implementation notes:

- The runtime already has everything: `SLVal::Cell` is a first-class value,
  and `rand.roll!` demonstrates a builtin mutating a cell argument in place
  (`Gc::write`). `std.cell` allocates like `rand.rng` does; `std.get` returns
  the inner value handle like `std.idx` does; `std.set!` writes like
  `rand.roll!` does and returns `SLVal::Void`.
- The closure transform is untouched. Its cell-wrapping of captured variables
  becomes an invisible runtime detail (captures can no longer be reassigned,
  so the wrapping is now unnecessary — removing it is a follow-up
  simplification, not part of this work).
- `AST::SetCell` / `Instruction::SetCell` are no longer produced from source.
  Leave them in place (serialization compat); removing them is a follow-up.
- ~15 `set!` usages in the test corpus migrate to the cell idiom.

---

## 2. Surface syntax

### 2a. Parser replacement

The `atoms`-based parser is replaced with a hand-written lexer +
recursive-descent parser (`src/parser.rs` rewritten; `atoms` dropped from
`Cargo.toml`). Motivation: `:` and `->` tokens with optional surrounding
whitespace, `#` line comments, and span tracking for future error locations.

Tokens: `LParen`, `RParen`, `Sym(String)`, `Int(i64)`, `Float(f64)`,
`Str(String)`, `Colon`, `Arrow`, `Eof`, each with a byte span. `:` and `->`
terminate a symbol run and are emitted as their own tokens, so `a:Int` ≡
`a : Int` and `->Int` ≡ `-> Int`. Qualified names (`std.+`) stay single
symbols. Whitespace and `#`-to-end-of-line comments are skipped.

### 2b. Grammar

```
Program    := Form*
Form       := "(" SpecialForm ")" | "(" Callee Expr* ")" | Atom
SpecialForm:= let | fn | if | block
Atom       := Int | Float | Str | Sym          -- `true`/`false` → Bool

let        := "let" Sym [":" Type] Expr        -- annotation optional
fn         := "fn" Sym "(" Param* ")" ["->" Type] ["where" "(" Bound* ")"] Expr+
Param      := Sym ":" Type                     -- annotation mandatory
Bound      := "(" TypeVarName TraitName+ ")"   -- e.g. (A Add), (B Add Eq)
if         := "if" Expr Expr Expr
block      := "block" Expr+

Type       := Sym                              -- Int, Float, String, Bool, Void, or a type var
            | "(" Sym Type* ")"                -- application: (List Int), (Cell T)
            | "(" "Fn" "(" Type* ")" "->" Type ")"
```

An elided `fn` return type means `Void`. `set!` is gone (§1b). Uppercase
symbols that aren't built-in type names (`Int`, `Float`, `String`, `Bool`,
`Void`, `List`, `Cell`, `Fn`) are type variables. Trait names in `where`
clauses are parsed as plain strings; the checker validates them against the
known trait set.

### 2c. Examples

```lisp
# Monomorphic; elided return type is Void (result discarded)
(fn main () (std.+ 1 2))
(fn main () ->Int (std.+ 1 2))

# Polymorphic identity
(fn id (a:A) ->A a)

# Trait bounds are explicit
(fn double (a:A) ->A where ((A Add)) (std.+ a a))

# Higher-order: top-level fn passed by name (§1a)
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

# Mutation via explicit cells (§1b)
(fn main () ->Int
  (let c (std.cell 1))
  (std.set! c (std.+ 1 (std.get c)))
  (std.get c))
```

### 2d. AST changes

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
  pub params: Vec<(String, Option<TypeAst>)>,  // see open question 4
  pub return_type: Option<TypeAst>,            // None → Void
  pub bounds: Vec<Bound>,
  pub code: Vec<AST>,
}

// AST::Let gains an optional annotation:
Let(String, Option<TypeAst>, Box<AST>),
// AST::SetCell is no longer produced by the parser (§1b).
```

Parameter annotations are *syntactically* mandatory (the parser rejects a
bare param — after the migration window, see §6), but the struct keeps
`Option<TypeAst>` so the closure transform can synthesize capture parameters
without inventing type syntax it can't know (open question 4).

---

## 3. Types, type variables, and traits

All internal to the new `src/typecheck.rs`.

### 3a. Representation

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
  /// description of the expression) so an ambiguity error (§5g) can point at
  /// it. `None` for vars with no obvious source location.
  Unbound { id: usize, bounds: Vec<Trait>, origin: Option<String> },
  /// A declared type variable being checked *inside its own function's
  /// body*. Unifies only with itself. Carries the `where`-clause bounds.
  Rigid { name: String, bounds: Vec<Trait> },
  /// Resolved: this variable IS the linked type (union-find by indirection).
  Link(Type),
}
```

`Rc<RefCell<_>>` gives the shared in-place mutation that union-find needs;
an id-indexed arena would also work but the `Rc` version keeps signatures
free of a context parameter.

**Rigid vs. Unbound is the soundness linchpin.** When checking the body of
`(fn id (a:A) ->A a)`, `A` is *rigid*: the body must work for every `A`, so
`A` unifies with nothing but itself. `(fn id (a:A) ->A 5)` is an error —
`Int` does not unify with rigid `A`. Callers never see rigid vars: each call
site *instantiates* the signature, replacing its rigid vars with fresh
`Unbound` vars (carrying the same bounds), which then unify freely.

### 3b. Unification

`unify(a, b)` — structural, with these variable rules:

- Chase `Link`s first (and path-compress).
- `Unbound` vs anything: **occurs check** (error on `A` ~ `(List A)`), then
  check bounds (below), then link.
- `Unbound` vs `Unbound`: link one to the other, merging bound sets.
- `Unbound(bounds)` vs concrete type `T`: every bound must be satisfied by
  `T` (§3c) or error, then link.
- `Unbound(bounds)` vs `Rigid(declared)`: every required bound must appear in
  `declared`, else error — this is the "you used `A` where `Add` is required
  but didn't declare `where ((A Add))`" case. Then link the unbound var to
  the rigid one.
- `Rigid` vs `Rigid`: equal only if the same variable.
- `Rigid` vs concrete: error.

Enforcing bounds at link time, with the rigid-var rule above, is the entire
"explicit bounds" mechanism — no separate bounds-inference or
declared-vs-used pass exists.

### 3c. Traits

```rust
pub enum Trait { Add, Sub, Eq, Concat, Slice }
```

| Trait | Satisfied by | Used by |
|---|---|---|
| `Add` | `Int`, `Float` | `std.+` |
| `Sub` | `Int`, `Float` | `std.-` |
| `Eq` | every type (v1 — open question 2) | `std.==` |
| `Concat` | `String`, `(List A)` | `std.concat` |
| `Slice` | `String`, `(List A)` | `std.slice` |

There is no `Index` trait. `std.idx`'s result type depends on its argument
type (`(List X) → X`, `String → String`), which would need associated types;
instead the checker special-cases `std.idx` (§4b, open question 3).

`Eq` being universal doesn't let you compare across types: `std.==` is
`(A A) -> Bool` with a *single* type variable, so `(std.== 1 "x")` fails
unification before `Eq` is ever consulted.

---

## 4. Builtin signatures

### 4a. Table

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
| `std.idx` | special-cased (§4b) |
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
`params.len()` / `rest` for its own arity errors.

### 4b. `std.idx`

The checker matches on the callee name and applies the rule directly: infer
the first argument's type, resolve it —

- `(List X)` → result `X`
- `String` → result `String`
- an `Unbound` or `Rigid` var → error: "cannot index a value of type `A`;
  `std.idx` requires a concrete `List` or `String`". Functions polymorphic
  over "indexable things" are not expressible in v1 (open question 3).

The second argument unifies with `Int`.

---

## 5. The checker

New module `src/typecheck.rs`, entry point:

```rust
pub fn typecheck(asts: &[AST], builtins: &[BuiltinSpec]) -> Result<(), TypeError>;
```

Called from `compiler::_compile_from_source` and the WASM entry point, after
parsing, **before** `transform_closures_in_module`. (Modules are currently
single-`"main"`; when real multi-module input arrives, the entry point takes
`(module_name, asts)` pairs and the signature map keys on
`(module, function)` — the design below already assumes that keying.)

### 5a. Two passes

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
like a statement and its type is discarded. (This makes the elided-return
form `(fn main () (std.+ 1 2))` legal; the runtime still leaves the value on
the stack, but the type system won't let a caller use it.)

### 5b. Inference is synthesis-only

There is no bidirectional checking mode. Since every parameter and return
type is annotated, a single `infer(env, expr) -> Result<Type, TypeError>`
walker plus `unify` at the constraint points (let annotations, argument
positions, `if` branches, return position) expresses everything. 

### 5c. Environments and name resolution

`Env` is a lexically-scoped map from name to either a monomorphic `Type` or
a `Scheme`. Resolution must mirror the compiler exactly:

- `Variable(name)`: env lookup (instantiate if a `Scheme`); else
  current-module function (§1a — instantiate its `Scheme`); else error.
- `CallFixed(Bare(name), args)`: env lookup **first** (a local closure —
  its type must unify with a fresh `(Fn (argtypes...) -> ret)`), then
  module function / builtin signature. Locals shadow module functions, same
  as `compiler.rs`.
- `CallFixed(Qualified(m, f), args)` / `FunctionRef(m, f)`: signature map.

### 5d. Inference rules

| Node | Rule |
|---|---|
| `Int`/`Float`/`String`/`Bool` | the corresponding type |
| `Variable`, `FunctionRef` | §5c |
| `Let(name, ann, expr)` | `T = infer(expr)`; if `ann` present, `unify(T, resolve(ann))`; bind `name` (see §5f for when it's a `Scheme`); result `T` |
| `DefineFn(f)` (expression position) | check the nested function (§5e); bind `f.name` to its `Scheme`; result: an instantiation of the scheme (matches the transform, which rewrites this node to `Let(name, closure)`) |
| `Call(callee, args)` | `Tc = infer(callee)`; `unify(Tc, Fn(map(infer, args), fresh))`; result `fresh` |
| `CallFixed(id, args)` | resolve per §5c; instantiate; arity check (respecting `rest` for variadics); unify each arg with its param (extra args unify with the instantiated `rest` type); result: instantiated return type. `std.idx` per §4b |
| `If(c, t, e)` | `unify(infer(c), Bool)` — conditions are strictly `Bool` now, no truthiness; `unify(infer(t), infer(e))`; result: that type |
| `Block(body)` | statements + last, same as a function body (but no `Void`-discard: a block's type is its last expression's type) |
| `Cell`/`DerefCell`/`SetCell`/`PartialApply` | unreachable — these are produced only by the closure transform, which runs after checking. Internal error if encountered |

### 5e. Nested functions

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
  is lifted under a mangled name, so a self-call would resolve to an
  unrelated top-level name). The checker rejects what the runtime can't run;
  supporting it is a follow-up. Recursion works via top-level functions.
- The resulting `Scheme` generalizes over the fn's own rigid vars only —
  enclosing rigid vars stay fixed inside it.

### 5f. Generalization: `DefineFn` only

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

### 5g. No unresolved inference variables

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
Haskell's ambiguity error), on the theory that an undetermined type may come
to mean something once codegen consumes types. Two things make it cheap here
that make it painful elsewhere:

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
  escape a function (§5c) and generalized schemes quantify over `Rigid` vars,
  not `Unbound` ones (§5f), every leftover `Unbound` is genuinely a
  monomorphic type the body failed to pin down.

The error uses each surviving var's `origin` hint (§3a) to name what to
annotate. One current limitation: annotations exist only on `let`, so an
ambiguous expression in a non-`let` position (always dead code under this
rule) can't be annotated in place — you delete it. An expression-level
ascription form is a follow-up if that ever becomes a real need.

---

## 6. Errors

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
a follow-up, not v1.

---

## 7. Pipeline and files touched

```
source
  ↓ parser (new: hand-written, typed syntax, # comments)
  ↓ typecheck::typecheck            ← NEW; errors halt
  ↓ closure::transform_closures_in_module   (unchanged)
  ↓ compiler::compile_module / wasm::compile (unchanged codegen)
```

- `src/parser.rs` — rewritten (lexer + recursive descent); `TypeAst`,
  `Bound`; `Function`/`AST::Let` extended; `set!` form removed.
- `src/typecheck.rs` — new: types, unification, traits, schemes, walker,
  builtin-signature conversion.
- `src/builtins.rs` — `std.cell`/`std.get`/`std.set!`; `signature` field on
  `BuiltinSpec` populated for everything.
- `src/compiler.rs` — first-class function refs (§1a); read the new
  `Function` shape (use `.0` of each param pair); call `typecheck` in
  `_compile_from_source`.
- `src/wasm.rs` — new `Function` shape; call `typecheck` after parsing.
- `src/closure.rs` — thread the new `Function` fields through lifting
  (capture params get `None` annotations); otherwise unchanged.
- `src/lib.rs` — `pub mod typecheck;`.
- Tests — migrate `set!` uses (§1b), then annotate the whole corpus (§8
  step 8).

---

## 8. Implementation order

Each step lands green on its own.

1. **First-class function references** (§1a). Compiler + parser + tests.
   Pure untyped-language feature.
2. **Cell builtins; remove `set!`** (§1b). Add the three builtins with
   tests, migrate the ~15 `set!` call sites, delete the special form.
3. **Parser rewrite.** Hand-written lexer + parser replacing `atoms`,
   behavior-compatible on the existing corpus (plus `#` comments and spans).
   No type syntax yet. Differential-test against the old parser's output on
   the existing test sources before deleting the `atoms` dependency.
4. **Type syntax.** `:` annotations (optional at this stage — unannotated
   params allowed and simply unchecked until step 7), `->` return types,
   type applications, `where` clauses. New `Function`/`Let` shapes threaded
   through `closure.rs`/`compiler.rs`/`wasm.rs` mechanically.
5. **Checker core.** `Type`, `TypeVar`, `unify` (occurs check, bounds,
   rigid rules), traits, instantiation. Pure functions, heavy unit tests:
   `A ~ Int`, `(List A) ~ (List Int)`, occurs-check failure, rigid-vs-Int
   failure, bounds merge, missing-bound-on-rigid failure.
6. **Builtin signature table** (§4). Inert until the walker exists.
7. **The walker** (§5) + wiring into both entry points behind a temporary
   flag (env var), since the corpus isn't annotated yet. Unit tests per §5d
   rule, plus: polymorphic id at two types, missing `where` bound, nested fn
   capturing an outer type var, `std.map` with a top-level fn ref, `Void`
   discard, `if` with non-Bool condition rejected.
8. **Migrate the corpus.** Annotate every test source; flip params to
   mandatory in the parser; remove the flag — checker always on. Any test
   relying on truthy non-`Bool` `if` conditions gets fixed here.
9. **Docs, TODO, cleanup.**

---

## Open questions

1. **`Eq` breadth.** v1: every type satisfies `Eq`, including `(Cell A)`,
   `(List A)` with any element, and function values (matches the runtime's
   structural `==`). Tightening to "elements must be `Eq`" is a small change
   later.
2. **Indexing polymorphism.** `std.idx` is special-cased and functions can't
   be generic over "indexable" types. Promoting `Index` to a real trait
   needs associated-type machinery (`Index::Output`); do this only if more
   output-type-depends-on-input builtins appear.
3. **`Function.params` representation.** The plan keeps
   `Vec<(String, Option<TypeAst>)>` permanently, with the parser enforcing
   mandatoriness for surface code and the closure transform filling `None`
   for synthesized capture params. The alternative — a required `TypeAst`
   with the transform synthesizing placeholder syntax — was rejected as the
   transform can't know capture types, but revisit if the `Option` proves
   annoying downstream.

## Follow-ups (recorded, not scheduled)

- Span-based error locations (lexer already captures spans).
- Post-transform verification pass (re-run the checker over transformed AST;
  requires rules for `Cell`/`DerefCell`/`PartialApply`).
- Typed WASM signatures; eventually tag elision for known-typed values.
- Closure transform simplification: captures no longer need cell-wrapping
  (bindings are immutable), so captures could be passed as plain values.
- Remove the now-unreachable `AST::SetCell` / `Instruction::SetCell`.
- Nested self-recursion (fix the transform's name resolution, then bind the
  fn's own name in its body env).
- Expression-level type ascription (e.g. `(the (List Int) expr)`) so an
  ambiguous type (§5g) can be resolved outside a `let`. Only needed if a
  non-dead ambiguous expression ever arises.
- Opaque nominal types for host handles (e.g. `Rng` instead of `(Cell Int)`)
  if letting users `std.set!` an RNG's state proves to be a footgun.
- User-defined types; trait implementations for them.
- Preludes / import-into-namespace (so `+` can be used unqualified).

---

## Appendix A: Extending toward user-defined traits and types

Notes on how the v1 design scales if SafeLisp code is later allowed to define
its own traits and types. Not scheduled; recorded so v1 choices stay
compatible with it. The load-bearing fact is *why* v1 traits are cheap:
**they are erased constraints.** The checker only ever asks "may `A` be `Int`
here?" — it never decides *which code runs*. The polymorphic behavior of `+`
et al. lives in the builtins' runtime tag-dispatch, which predates the type
system. Everything below is a question of whether that erasure property
survives.

### A1. User-defined types come first

Marker/constraint traits (below) are nearly useless without user-defined
types, because the set of types is otherwise closed (`Int`, `Float`,
`String`, `Bool`, `List`, `Cell`, `Fn`) and there's nothing to classify.
User-defined algebraic/record types are the larger, prerequisite feature and
create the actual demand for user traits. Sequencing: types first, then
traits.

### A2. Tier 1 — constraint traits (no methods): cheap, erased

`(trait Ord)` + `(impl Ord Int)`, usable in `where` clauses. Implementation
against the v1 plan is mechanical:

- `Trait` stops being a closed Rust enum and becomes an interned name.
- The hardcoded `satisfies` table becomes a map populated by a pass-0 walk
  over `trait`/`impl` forms.
- Coherence check: no duplicate `impl` for the same `(trait, type)`.

Nothing downstream of the checker changes — still fully erased. Roughly a day
or two on top of v1. Keeping all `Trait` handling funneled through a single
`satisfies` function in v1 (rather than matching the enum throughout the
checker) keeps this a one-file change.

### A3. Tier 2 — traits with methods: the cliff (breaks erasure)

`(trait Show (fn show (a:Self) ->String))` with per-type impls. Checker-side
additions are moderate and reuse v1 machinery: `Self` is a distinguished
rigid var, method signatures are `Scheme`s, calling `(show x)` is
instantiation + unification, plus coherence checks (impl signature matches
declaration; no overlapping impls). Call it a doubling of `typecheck.rs`,
conceptually nothing new.

The real cost is that `(show a)` in a generic body where `a:A` is rigid
**cannot be resolved at compile time** — the checker must now influence which
code runs, which breaks erasure and forces a dispatch strategy (A4). This is
codegen work and belongs with the deferred bytecode phase.

### A4. Dispatch strategy for method-bearing traits

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

### A5. Two smaller cliffs to avoid stumbling into

- **Conditional impls** — `(impl Eq (List A) where ((A Eq)))`. Recursive
  membership ("is `(List (List Int))` Eq?") turns `satisfies` into a small
  constraint solver with termination concerns. Open question 2 (tightening
  `Eq`) is secretly this feature; if pursued, do it as one hardcoded
  recursive case, not a general mechanism.
- **Associated types** — open question 3 (`Index::Output`). Adds type-level
  functions and projection normalization to the unifier; the single biggest
  jump in checker complexity here.

### A6. Does v1 constrain any of this?

No. Rigid vars carrying bound sets, instantiation-time bound checking, and
schemes are exactly the pieces a method-bearing trait system reuses; v1
paints no corners. The only forward-looking hygiene worth observing now is
routing trait membership through one `satisfies` function so the
enum→interned-name swap (A2) stays local.
