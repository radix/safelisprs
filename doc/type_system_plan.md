# Plan: Type System and Type Checker for SafeLisp

## Design decisions (locked in after review)

| Aspect | Decision |
|---|---|
| Pipeline stage | **Before** the closure transform and before the backend split — one checker shared by interpreter + WASM |
| Annotations | **Mandatory** on every `fn`'s params and return type (return type may be elided; defaults to `Void`) |
| `let` bindings | **Inferred** — no mandatory annotation; optional annotation allowed |
| Polymorphism | Generics for `List` and `map`; **no overloads** (see "Traits" below) |
| Trait bounds | **Explicit** via a `where` clause on `fn` — not inferred from the body |
| Strictness | **Strict** — type errors halt compilation |
| Type variables | **Uppercase** identifiers (e.g. `A`, `B`, `Elem`) |
| Surface syntax | Trailing colon form with spaces allowed around `:` and `->`. Function types written `(Fn (Int Int) -> Int)`. Type application written `(List Int)`, `(Cell T)`. **No `<>` generics syntax.** |
| Comments | `#` line comments |
| Module name | `typecheck` (not `typeck`) |
| Backend scope | Both backends share one checker |

---

## 1. Custom parser

The `atoms` crate cannot reasonably support our type syntax:
- `:` inside a parameter / let binding (with optional spaces around it) is not a token atoms distinguishes.
- `->` as a standalone symbol with spaces around it is fine in atoms (it's just a symbol), but combined with the colon handling and the desire for a uniform, controlled grammar, a custom parser is cleaner than fighting atoms.
- We want clear, explicit control over whitespace around `:` and `->` so users can write `(a : Int)`, `(a:Int)`, `(Int Int -> Int)`, `(Int Int ->Int)` etc. interchangeably.

### 1a. New parser module: `src/parser.rs` rewritten

We replace the `atoms`-based parser with a hand-written recursive-descent parser. The grammar is small enough that this is tractable and gives us full control over error messages and span tracking (which we'll want for type errors later).

The parser produces the existing `AST` plus a new `TypeAst` and an extended `Function` / `AST::Let`. We keep `AST` essentially as-is so downstream code (`closure.rs`, `compiler.rs`, `wasm.rs`) only needs to handle the new optional type annotation on `Let` and the new fields on `Function`.

The `atoms` crate dependency is removed from `Cargo.toml`.

### 1b. Lexer

A small lexer producing tokens:

```
Token {
  kind: TokenKind,
  span: Span,        // byte start..end, for future error reporting
}

enum TokenKind {
  LParen, RParen,
  Sym(String),       // identifiers and operators: `let`, `fn`, `+`, `std.+`, `a`, `Int`, `A`, ...
  Int(i64),
  Float(f64),
  Str(String),       // "..." with escapes
  Colon,             // `:`
  Arrow,             // `->`
  Eof,
}
```

Whitespace and `#` line comments (from `#` to end of line) are skipped by the lexer.

**Lexer rule for `:` and `->`:** `:` and `->` are dedicated tokens, *not* symbols. Inside a maximal symbol-character run, encountering `:` or `->` terminates the current symbol and emits `Colon` / `Arrow` as a separate token, then lexing continues. So `a:Int` lexes as `Sym("a")`, `Colon`, `Sym("Int")` — identical to `a : Int` with spaces. Likewise `Int->Int` lexes as `Sym("Int")`, `Arrow`, `Sym("Int")`. Qualified names like `std.+` use `.` and are unaffected (they don't contain `:` or `->`).

This is a small, well-contained lexer. Total estimated ~150 LOC.

### 1c. Grammar

```
Program    := Form*

Form       := "(" SpecialForm ")" | "(" Callee Expr* ")" | Atom
SpecialForm:= "let" | "fn" | "if" | "set!" | "block"
Callee     := Expr                       -- for Call (dynamic)
            | Identifier                  -- for CallFixed

Identifier := Sym                        -- bare: `foo`
            | Sym "." Sym                -- qualified: `std.+` (kept as one Sym by lexer)

Atom       := Int | Float | Str | Sym    -- Sym `true`/`false` → Bool

-- Special forms:

let        := "let" Sym [":" Type] Expr              -- optional annotation
fn         := "fn" Sym "(" Param* ")" ["->" Type] ["where" "(" Bound* ")"] Body
Param      := Sym ":" Type                            -- annotation mandatory
Bound      := "(" Sym Trait+ ")"                      -- (TypeVar Trait...) e.g. (A Add)
if         := "if" Expr Expr Expr
set!       := "set!" Expr Expr
block      := "block" Expr+

Body       := Expr+                                   -- last expr's value is the result

-- Types:

Type       := AtomType
            | "(" TypeConstructor Type* ")"           -- type application: (List Int), (Cell T)
            | "(" "Fn" "(" Type* ")" "->" Type ")"    -- function type: (Fn (Int Int) -> Int)

AtomType   := Sym                                    -- Int, Float, String, Bool, Void, or a TypeVar (uppercase)
TypeConstructor := Sym                               -- List, Cell, Fn, or a future user type
```

**Type application uses parenthesized lists** with the constructor as the head:
- `(List Int)` — list of ints
- `(Cell Int)` — cell of int
- `(Fn (Int Int) -> Int)` — function from two ints to int
- `(List A)` — polymorphic list of `A`

The arrow `->` is allowed optional spaces on either side, handled naturally by the lexer skipping whitespace between tokens.

`List` / `Cell` / `Fn` are not reserved words — they're just type constructors recognized by name in the type parser. An uppercase identifier that is not one of these built-in type names is a type variable. (We could later allow user-defined types with the same syntax.)

### 1d. Surface examples

```lisp
# Monomorphic
(fn id (a:Int) ->Int a)
(fn main () (std.+ 1 2))                           # return type elided → Void
(fn main () ->Int (std.+ 1 2))

# Polymorphic
(fn id (a:A) ->A a)
(fn main () ->Int
  (let dbl (fn dbl (x:Int) ->Int (std.+ x x)))     # let infers (Fn (Int) -> Int)
  (std.map (std.range 0 5) dbl))

# Annotated let (optional)
(fn main () ->Int
  (let xs:(List Int) (std.range 0 5))
  (std.len xs))

# Higher-order builtin
(fn main () ->(List Int)
  (std.map (std.range 0 5) (fn sq (x:Int) ->Int (std.+ x x))))

# Trait bounds via where clause
(fn add2 (a:A) ->A where ((A Add)) (std.+ a a))
(fn concat2 (a:A) ->A where ((A Concat)) (std.concat a a))
```

---

## 2. Type syntax (surface and AST)

### 2a. `TypeAst` (parser output)

```rust
#[derive(Debug, PartialEq, Clone)]
pub enum TypeAst {
  Named(String),                       // Int, Float, String, Bool, Void, List, Cell, Fn, or a type var (uppercase)
  Apply(Box<TypeAst>, Vec<TypeAst>),   // (List Int), (Cell T)
  Fn(Vec<TypeAst>, Box<TypeAst>),      // (Fn (Int Int) -> Int) — params, ret
}

#[derive(Debug, PartialEq, Clone)]
pub enum TraitRef {
  Add, Eq, Concat, Slice,               // user-writable traits (Index is internal-only — see §4d)
  Other(String),                        // reserved for future user-defined traits; rejected in v1
}

#[derive(Debug, PartialEq, Clone)]
pub struct Bound {
  pub var: String,                      // the type variable name, e.g. "A"
  pub traits: Vec<TraitRef>,             // traits it must satisfy, e.g. [Add]
}
```

The parser doesn't decide whether `Named("A")` is a type variable or a built-in type — that's the type checker's job (it knows the set of builtin type constructors: `Int`, `Float`, `String`, `Bool`, `Void`, `Cell`, `List`, `Fn`; anything else that is uppercase is a type variable). The parser also doesn't validate `TraitRef` names against the known set beyond parsing them into the enum — the checker does final validation.

### 2b. Extended `Function` and `AST::Let`

```rust
#[derive(Debug, PartialEq, Clone)]
pub struct Function {
  pub name: String,
  pub params: Vec<(String, TypeAst)>,   // was Vec<String>; type annotation mandatory
  pub return_type: Option<TypeAst>,     // None → defaults to Void at typecheck time
  pub bounds: Vec<Bound>,               // from the `where` clause; may be empty
  pub code: Vec<AST>,
}

// AST::Let gains optional annotation:
Let(String, Option<TypeAst>, Box<AST>),
```

`Option<TypeAst>` for `return_type` lets the parser represent the elided form cleanly; the type checker fills in `Void` when resolving. (We could also fill `Void` at parse time, but keeping `None` makes error messages clearer — "you elided the return type, which defaults to Void".)

---

## 3. Internal `Type` representation (in `src/typecheck.rs`)

```rust
pub enum Type {
  Int, Float, String, Bool, Void,
  Cell(Box<Type>),
  List(Box<Type>),
  Fn(FnSig),
  Var(TypeVar),
}

pub struct FnSig {
  pub params: Vec<Type>,
  pub ret: Box<Type>,
}

pub enum TypeVar {
  /// A type variable that has not yet been unified with a concrete type.
  /// Carries a unique id, an optional name hint (for error reporting),
  /// and the set of trait bounds declared on it (from the `where` clause).
  Unbound { id: usize, name: Option<String>, bounds: Vec<Trait> },
  /// A type variable that has been unified with a concrete type.
  /// Following the `Link` recovers the type. (Union-find via indirection.)
  Link(Box<Type>),
}
```

**What `Unbound` vs `Link` means (plain explanation):** A type variable starts out as `Unbound` — "we don't know what this type is yet, but it has an identity (an id)". When the unifier decides "this type variable must be `Int`", it mutates the variable in place to `Link(Box::new(Type::Int))`. Anyone who later reads the variable follows the `Link` to get `Int`. This is the standard union-find trick used in ML-style type inference (and in Rust's own type checker). There is no "Generic" variant — generalization is handled at a higher level (see §5e).

**Why no `Generic` variant:** A "generic" type variable is just an `Unbound` variable that lives in a function's declared signature and gets *instantiated* (replaced with a fresh `Unbound` variable) at every call site. The distinction between "this `Unbound` is a placeholder in the current expression" vs "this `Unbound` is a schema parameter" is managed by the schema/environment layer, not by the `TypeVar` enum itself. Keeping `TypeVar` to just `Unbound`/`Link` means the unifier is a single, simple function over `Type` — no special cases.

**Trait bounds on `Unbound`:** When the type checker sets up a function's environment, it creates `Unbound` vars for each declared type variable (e.g. `A` from `(fn add2 (a:A) ->A where ((A Add)) ...)`), and attaches the `where`-clause bounds to those vars. When such a bounded var later unifies with a concrete type (e.g. `Int`), the unifier checks `Int` satisfies each bound (`Add` is satisfied by `Int`/`Float` — yes). When two bounded `Unbound` vars unify, their bounds are merged (union). This is how the explicit `where` clause is enforced.

---

## 4. Builtins: signatures and traits

### 4a. No overloads

Each builtin has *exactly one* signature. Where the runtime currently dispatches on argument tags (e.g. `+` works on `Int`+`Int` *or* `Float`+`Float`), we instead model the operation via **traits** — a builtin's signature uses trait-bounded type variables.

### 4b. Traits

We introduce a small trait system so that `+`, `-`, `concat`, `idx`, `slice` can express "works on any type that supports this operation" without overloads.

```rust
pub enum Trait {
  Add,        // t supports (t, t) -> t            (for +, -)
  Eq,         // t supports (t, t) -> Bool         (for ==)
  Concat,     // t supports (t, t) -> t            (for concat: String or (List A))
  Index,      // internal-only, not writable in `where` (see §4d)
  Slice,      // t supports (t, Int, Int) -> t     (for slice)
}
```

A type variable carries a set of required traits (its **bounds**), declared explicitly via the `where` clause on the function. The unifier enforces the bounds: whenever a bounded `Unbound` var unifies with a concrete `Type`, it checks the concrete type satisfies each bound. If not, a type error is raised. When two bounded `Unbound` vars unify, their bounds are merged (union).

**No inference of bounds.** You decided bounds should be explicit in the source via `where`, not inferred from usage. So if a user writes `(fn add2 (a:A) ->A (std.+ a a))` *without* `where ((A Add))`, the checker errors: "type variable `A` is used in a position requiring trait `Add`, but no bound is declared — add `where ((A Add))`". This makes the checking simpler and forces the user to be explicit about their type-variable constraints.

Concretely:

```rust
pub enum TypeVar {
  Unbound { id: usize, name: Option<String>, bounds: Vec<Trait> },
  Link(Box<Type>),
}
```

When the unifier unifies a bounded `Unbound` var with a concrete `Type`, it immediately checks the concrete type satisfies each bound (e.g. `Add` is satisfied by `Int` and `Float` only; `Concat` by `String` and `(List A)`; etc.). If not, a type error is raised. When two bounded `Unbound` vars unify, their bounds are merged (union).

### 4c. Builtin signatures

Each `BuiltinSpec` gains exactly one signature, expressed in terms of `Type` and trait constraints:

| Builtin | Signature |
|---|---|
| `std.+` | `(A A -> A)` where `A: Add` |
| `std.-` | `(A A -> A)` where `A: Add` (renaming: `Add` covers `-` too; we could split into `Add`/`Sub` but they have the same member set, so one trait is fine) |
| `std.==` | `(A A -> Bool)` where `A: Eq` (one type var, as you specified) |
| `std.concat` | `(A A -> A)` where `A: Concat` |
| `std.idx` | `(A Int -> B)` with `A: Index` (internal — see §4d) |
| `std.push` | `((List A) A -> (List A))` |
| `std.slice` | `(A Int Int -> A)` where `A: Slice` |
| `std.len` | `((List A) -> Int)` |
| `std.range` | `(Int Int -> (List Int))` |
| `std.list` | variadic `(A... -> (List A))` — homogeneous, as you decided |
| `std.map` | `((List A) (Fn (A) -> B) -> (List B))` |
| `rand.rng` | `(Int String -> (Cell Int))` |
| `rand.roll!` | `((Cell Int) Int -> Int)` |

### 4d. The `Index` trait and element type

`idx` is tricky because the return type depends on the input type (`(List A) -> A`, `String -> String`). With traits and no overloads, we model it as:

- `idx`'s signature: `(A Int -> B)` with `A: Index`, plus a *type function* `Index::Output(A) = B` that the checker resolves during unification. When `A` unifies with `(List X)`, `B` unifies with `X`. When `A` unifies with `String`, `B` unifies with `String`.

This requires associated-type-like machinery, which is more than a basic trait system. **For v1, the pragmatic choice:** special-case `idx` and `slice` in the checker (the checker knows `idx`'s typing rule directly, rather than going through a generic trait mechanism). `Index` is an **internal-only trait** — it cannot appear in a user's `where` clause (the parser rejects it with "trait `Index` cannot be declared explicitly; it is resolved automatically by `std.idx`"). We still call it a "trait" in error messages, but the implementation is a hardcoded rule. If we later generalize the trait system with associated types, we can refactor `idx`/`slice` into it.

So the v1 user-facing trait system supports: `Add`, `Eq`, `Concat`, `Slice`. `Index` is special-cased internally. This keeps the implementation small while still giving us the no-overloads property you want.

### 4e. `BuiltinSpec` extension

```rust
pub struct BuiltinSpec {
  pub module: &'static str,
  pub name: &'static str,
  pub num_params: Option<u16>,           // kept for runtime arity check (unchanged)
  pub signature: BuiltinSignature,       // NEW
}

pub struct BuiltinSignature {
  pub params: &'static [TypeConst],
  pub ret: TypeConst,
  pub bounds: &'static [(TypeVarId, &'static [Trait])],
}

pub enum TypeConst {
  Int, Float, String, Bool, Void,
  Cell(&'static TypeConst),
  List(&'static TypeConst),
  Fn { params: &'static [TypeConst], ret: &'static TypeConst },
  Var(TypeVarId),                        // a type variable referenced in `bounds`
}
```

These are `&'static` so they live in compiled-in tables. The type checker converts `TypeConst` → `Type` (instantiating fresh `Unbound` vars — with the corresponding bounds — for each `Var`) at each call site.

### 4f. Trait membership

| Trait | Members |
|---|---|
| `Add` | `Int`, `Float` |
| `Eq` | `Int`, `Float`, `String`, `Bool`, `Void`, `(Cell A)`, `(List A)`, function types (all function values are `Eq` by structural identity) |
| `Concat` | `String`, `(List A)` |
| `Slice` | `String`, `(List A)` |

`Eq` membership is broad because runtime `==` works structurally on everything. (You confirmed: `==` has one type var, and `(std.== 1 "x")` should type-error because the two args must be the *same* type var `A` — so even though `Eq` is broad, the single-type-var rule prevents comparing different types.) For v1 we say all `(Cell A)`/`(List A)` are `Eq` regardless of element type, and let the runtime's potential infinite-loop on cyclic cells be a separate runtime concern. Simpler is better for v1.

---

## 5. Type checker architecture (`src/typecheck.rs`)

You should re-review from this section onward.

<<continue reviewing here>>

### 5a. Public entry point

```rust
pub fn typecheck(
  asts: &[AST],
  builtins: &[BuiltinSpec],
) -> Result<TypecheckOutput, TypeError>;
```

Called by:
- `compiler::_compile_from_source` (after the new parser runs, before `compile_modules` → before the closure transform). One-line insertion at `compiler.rs:444`.
- `wasm.rs` at the equivalent spot (after parsing, before `compile_modules` / `link`). One line at `wasm.rs:~205`.

`TypecheckOutput` carries:
- Per-function resolved `FnSig` keyed by `(module, function)`. This can be threaded into `compiler::Function` as new `param_types: Vec<Type>` and `return_type: Type` fields (used later for WASM signature generation and as documentation; not required for runtime today). For v1 we keep these fields optional / out of the serialized `Function` to avoid breaking existing `.slc` files.
- Diagnostics (empty on success).

### 5b. The checking algorithm — two passes per module

**Pass 1 — collect signatures.** Walk all top-level `AST::DefineFn`. Convert each `Function::params` (now `(String, TypeAst)` pairs), `Function::return_type` (or `Void` if elided), and `Function::bounds` (the `where` clause) into a `FnSig`, recording type variables and their trait bounds. Insert into a `HashMap<(String /*module*/, String /*fn*/), FnSig>`. Also insert builtin sigs from `BuiltinSpec`. Detect duplicate definitions.

**Pass 2 — check bodies.** For each function, build an environment `Env` (a `HashMap<String, Type>` plus a parent pointer for lexical scopes) mapping param names → `Type` (from the sig, with bounds attached to the type vars). Walk each body expression under an *expected type* `Option<Type>` (bidirectional: synthesis + checking modes). Statements (non-final body expressions) are checked with `expected = None` and their type is discarded. The final body expression must unify with the function's declared return type.

**Bounds enforcement during body checking:** when a builtin call requires a trait on one of its type variables (e.g. `std.+` requires `A: Add`), the checker attaches the required trait as a bound to the fresh `Unbound` var instantiated for that call. If that var is *also* a function-level type variable (declared in the `where` clause), the checker verifies the required trait is among the declared bounds — if not, it errors with "type variable `A` is used in a position requiring trait `Add`, but only bounds `[Eq]` are declared". This is the explicit-bounds check: usage in the body cannot add bounds that the user didn't declare.

### 5c. Per-AST-node checking rules

| AST node | Synthesis (no expected) | Checking (with expected) |
|---|---|---|
| `Int(_)` | `Int` | unify expected with `Int` |
| `Float(_)` | `Float` | likewise |
| `String(_)` | `String` | likewise |
| `Bool(_)` | `Bool` | likewise |
| `Variable(name)` | lookup in `Env`; if a function name, lookup in sigs and produce `Fn(...)` (instantiate fresh vars if polymorphic) | unify |
| `Let(name, ty, expr)` | synth `expr` → `T`; if `ty` given, unify `T` with parsed `ty`; bind `name: T` in env; result type = `T`. (Inference: if `ty` is `None`, bind with `T` directly.) | same; the `let` expression's value is the init expr's value |
| `DefineFn(f)` | only valid at top level (Pass 1 records it) | n/a in expression position |
| `Call(callable_expr, args)` | synth each arg; synth `callable_expr` → must unify with `(Fn (params) -> ret)`; unify arg types with params; result = `ret` | bidirectional: if expected given, push expected-ret into the callable's ret during synthesis |
| `CallFixed(ident, args)` | resolve `ident` → sig; unify arg count and arg types with sig params (instantiate fresh vars if polymorphic); result = sig ret. For builtins with trait bounds, attach bounds to the fresh vars and check them at concrete-type unification time; verify any bounds on function-level type vars are declared in the `where` clause. | bidirectional: push expected-ret down |
| `If(cond, then, els)` | synth `cond` → unify with `Bool`; synth `then` and `els` → unify them together; result = that type | unify `then`/`els` with expected |
| `Block(body)` | synth each; result = last's synth | check each statement with `None`, check last with expected |
| `Cell(expr)` | synth `expr` → `T`; result = `(Cell T)` | unify `(Cell T)` with expected |
| `DerefCell(expr)` | synth `expr` → must unify with `(Cell T)`; result = `T` | n/a (unify result with expected) |
| `SetCell(target, value)` | synth `target` → `(Cell T)`; synth `value` → unify with `T`; result = `T` (= the new value) | n/a |
| `PartialApply(callable, args)` | synth `callable` → `(Fn (params) -> ret)`; unify each arg with leading params; result = `(Fn (remaining_params) -> ret)` | n/a |
| `FunctionRef(m, f)` | lookup sig → `(Fn (params) -> ret)` (instantiated if polymorphic) | n/a |

The transform-produced variants (`Cell`, `DerefCell`, `SetCell`, `PartialApply`, `FunctionRef`) are in the AST enum but only produced by `closure.rs`. Since the checker runs **before** the transform, the checker won't see them in user code — but it *will* see them if anyone calls `typecheck` on post-transform AST. We implement their rules anyway (cheap), and they're needed if we later add a post-transform verification pass.

### 5d. Builtin call resolution (no overloads)

Each builtin has exactly one signature (with type vars and possibly trait bounds). At a `CallFixed` to a builtin:
1. Instantiate the signature: replace each `Var(id)` in the signature with a fresh `Unbound` type variable, carrying the bounds from the builtin's `bounds` table.
2. Unify each argument's synthesized type with the corresponding instantiated param type.
3. The result is the instantiated `ret`.
4. During unification, whenever a bounded `Unbound` var becomes concrete (via `Link`), immediately check the concrete type satisfies the bounds. If two bounded `Unbound` vars unify, merge bounds.
5. **Bounds-vs-`where` check:** if a builtin-call's fresh var corresponds (via unification) to a function-level type variable declared in the `where` clause, verify that every bound the builtin requires is among the declared bounds. If the builtin requires `Add` but the `where` clause only declares `Eq`, error: "type variable `A` requires trait `Add` for `std.+` but only `[Eq]` is declared in `where`".

For `idx` (special-cased, no general associated-type machinery):
- Signature is `(A Int -> B)` with `A: Index` (internal). The checker implements the rule directly: when `A` unifies with `(List X)`, unify `B` with `X`; when `A` unifies with `String`, unify `B` with `String`. Since `Index` is internal-only, a user's `where` clause cannot reference it; instead, a function that uses `std.idx` on a type variable must declare no `Index` bound (the checker resolves it automatically). If the user *wants* to constrain `A` to "indexable types", they'd need a user-facing way to express that — deferred to a follow-up (see §9). For v1, a function like `(fn first (xs:A) ->B (std.idx xs 0))` would have `A` and `B` as free type vars with the `idx`-implied constraint resolved at each call site — meaning the function is only callable with concrete indexable types, and the `where` clause is silent on `Index`.

### 5e. Type variables, instantiation, and generalization (plain explanation)

- **Type variables in a `fn` signature** (e.g. `(fn id (a:A) ->A a)`): `A` is a *schema parameter* of `id`. The signature is `forall A. (Fn (A) -> A)`. Every *call* to `id` gets a *fresh copy* of `A` — so `(id 5)` instantiates `A := fresh1`, unifies `fresh1` with `Int`, and the result is `Int`. This is "instantiation": copy the schema, replacing each schema-level type var with a fresh unbound var (carrying its declared bounds).

- **Type variables in `let` bindings** (e.g. `(let id (fn id (a:A) ->A a))`): the inferred type of the init expr may contain unbound type vars. Should `id` be polymorphic at each use site after the `let`? In ML this is "let-polymorphism" / "let-generalization": yes, `id` becomes `forall A. (Fn (A) -> A)` and each use instantiates fresh.

  The **value restriction** is a rule that says: only generalize a `let`-bound variable if the init expression is a *syntactic value* (a literal, a function definition, a variable lookup, a `Cell` wrapping — basically anything that doesn't *call* a function or otherwise produce a mutable result that could observe the type variable in a problematic way). The reason this matters in SafeLisp: we have `Cell`, which is mutable. If you write `(let x (some_cell_of_A))` and then use `x` as both `(Cell Int)` and `(Cell String)`, you'd be unsound (the same cell can't hold both). The value restriction prevents generalizing `let`-bound variables whose init expr involves mutation or calls.

  **What this means concretely for our checker:** When checking `(let name init)`:
  1. Synth `init` → type `T`.
  2. If `init` is a syntactic value (we define a small `is_value` predicate: `AST::Int/Float/String/Bool`, `AST::DefineFn`, `AST::Variable`, `AST::FunctionRef`, `AST::Cell(...)` of a value — *not* `Call`, `CallFixed`, `If`, `Block`, `DerefCell`, `SetCell`, `PartialApply`), then generalize `T`: collect the unbound type vars in `T` that are *not* free in the enclosing environment, and wrap them as schema parameters. Each use of `name` instantiates them fresh.
  3. If `init` is not a syntactic value, do *not* generalize — `name` gets the monomorphic `T` and uses of `name` all share the same type vars (so the first use that unifies them with a concrete type fixes them for all subsequent uses).

  This is the standard ML value restriction, lightly adapted. It's a modest amount of code (~30 LOC for the generalization + instantiation helpers) and gives us proper higher-order code like `(let id (fn id (a:A) ->A a)) (pair (id 5) (id true))` typing as `(Int, Bool)` rather than erroring.

- **No cross-function generalization.** Functions don't infer their signatures (you mandated annotations), so generalization only happens at `let` bindings. This keeps the inference engine small: instantiation + unification + the `let`-generalization rule, no full HM.

### 5f. `Void` vs uninitialized

Since the checker runs before the closure transform, the "uninitialized local" concern (interpreter fills locals with `Void`) is invisible at the source level — every local is bound by `let` before use, and the checker enforces "use after bind" naturally via `Env`. The `Void` type is just a normal type that a function may declare it returns (or implicitly, by eliding `-> Type`) or that a `let` may bind (rare but legal). No definite-assignment pass is needed at this layer.

---

## 6. Error reporting

`TypeError { message: String, context: Vec<String> }` where `context` is a stack of "while checking function `main`", "while checking parameter `x` of `id`", "while checking call to `std.+`". No byte offsets in v1 (the new lexer captures spans, so we can add span-based locations in a follow-up). Errors look like:

```
TypeError: expected `Int`, got `String`
  while checking call to `std.+`
  while checking argument 1
  in function `main`
```

Trait-bound failures look like:

```
TypeError: type `String` does not implement trait `Add`
  while checking call to `std.+`
  in function `main`
```

Missing-bounds failures look like:

```
TypeError: type variable `A` is used in a position requiring trait `Add`, but only `[Eq]` is declared in `where`
  while checking call to `std.+`
  in function `add2`
```

---

## 7. Pipeline integration

```
source
  ↓ parser (custom, new)              (extended for type syntax; replaces atoms)
  ↓ typecheck::typecheck   ←── NEW    (errors halt)
  ↓ closure::transform_closures_in_module   (unchanged, trusted)
  ↓ compiler::compile_module  → Package   (interpreter backend)
  ↓ wasm::compile           → wasm::Module  (WASM backend, runs same checker)
```

Files touched:
- `src/parser.rs` — **rewritten** as a hand-written recursive-descent parser with lexer; produces `AST` + `TypeAst` + `Bound` + `TraitRef`; extends `Function` (params now `(String, TypeAst)`, return type `Option<TypeAst>`, bounds `Vec<Bound>`) and `AST::Let` (optional `TypeAst`).
- `Cargo.toml` — remove `atoms` dependency.
- `src/typecheck.rs` — NEW module: `Type`, `FnSig`, `TypeVar`, `Trait`, `Bound`, unification, instantiation, `let`-generalization, the bidirectional walker, builtin signature table, bounds-vs-`where` enforcement.
- `src/builtins.rs` — extend `BuiltinSpec` with `signature: BuiltinSignature`; populate the table for all existing builtins (`default_builtins()`).
- `src/compiler.rs` — update `compile_function` to read `params: Vec<(String, TypeAst)>` (just take the `.0` of each pair for the local-name mapping); call `typecheck::typecheck` in `_compile_from_source`.
- `src/wasm.rs` — update similarly for the new `Function`/`AST::Let` shape; call `typecheck::typecheck` after parsing.
- `src/closure.rs` — update for new `Function`/`AST::Let` shape (mechanical: thread the type annotations and bounds through the transform — when lifting a function, copy its param types, return type, and bounds onto the lifted `Function`; when cell-wrapping a captured param, the captured param's type becomes `(Cell T)`).
- `src/lib.rs` — `pub mod typecheck;`.
- `tests/test_eval.rs` and existing interpreter/builtins/closure tests — add type annotations and `where` clauses to every existing test source so they still compile under the mandatory-annotation rule. (Bulk of the migration work; see step 8.)
- `TODO.md` — record the design + follow-ups (post-transform type verification, WASM typed signatures, span-based error locations, user-facing `Index` bound syntax).

### Closure-transform interaction

The closure transform (`closure.rs`) lifts nested functions and cell-wraps captured variables. It must be updated to preserve type annotations:
- A lifted function's `params` becomes `[captures..., original_params...]`. Each capture is a `(Cell T)` (because captured vars are cell-wrapped), so the lifted function's param types are `[(Cell T_cap)..., T_orig...]`. The lifted function's return type is the same as the original. The lifted function's bounds are the same as the original (captures don't introduce new bounds — they're concrete `(Cell T)` types).
- `patch_cell_access` rewrites uses of captured names to `DerefCell(Variable(name))` — this is transparent to types: `DerefCell((Cell T)) = T`.
- The closure expression (`PartialApply(FunctionRef(...), captures)`) produces a `(Fn (remaining_params) -> ret)` type, which is what the source-level let-binding of the closure should have inferred anyway.

Because the type checker runs *before* the transform, the transform's correctness w.r.t. types is a separate (mild) proof obligation. For v1 we trust it. A follow-up task can run the checker again post-transform to verify, if desired.

---

## 8. Implementation order (each step independently committable)

1. **Custom lexer + parser.** Replace `atoms`-based parsing. Produces existing `AST` (without type annotations yet, to keep this step reviewable) + new `TypeAst`/`Bound`/`TraitRef` enums (unused initially). `#` line comments supported. All existing call sites and tests pass unchanged. Unit tests for the new parser matching the old `atoms` behavior on the existing corpus.
2. **Type-syntax parsing.** Extend the parser to handle `:` in params/let, `->` in fn return types, `(List Int)` / `(Cell T)` / `(Fn (...) -> ...)` type applications, and `where` clauses. Extend `Function` (params, return type, bounds) and `AST::Let`. Update `closure.rs`, `compiler.rs`, `wasm.rs` for the new field shapes (mechanical; ignore the type info for now). Existing tests still pass (no annotations yet, so return types are `None` → default `Void`, params have... wait, params are now *mandatory* annotated, so existing tests will fail to parse. We have two options: (a) make annotations mandatory only after step 6, gating with a flag; (b) allow unannotated params during migration and treat them as fresh unbound type vars. Option (b) is cleaner: an unannotated param gets a fresh type var, inferred from the body. This effectively makes annotations optional *during migration* and we tighten to mandatory at step 6.) Go with (b): unannotated params infer fresh vars; step 6 flips the parser to reject unannotated params.
3. **`typecheck` skeleton: `Type`, `FnSig`, `TypeVar`, `Bound`, `Trait`, unification, instantiation.** Pure data structures + unifier + instantiation. No integration yet. Unit tests on the unifier (unify `Int` with `Int`, `A` with `Int`, `(List A)` with `(List Int)`, `(Fn (A) -> A)` with `(Fn (Int) -> Int)`, failures). Bounds merging on var-var unification.
4. **Traits + trait membership table.** `Trait` enum, `satisfies(Type, Trait) -> bool`, bounds-checking at concrete-type unification. Unit tests.
5. **Builtin signature table.** Extend `BuiltinSpec` with `signature`; populate `default_builtins()` with sigs for every existing builtin. Existing code ignores the new field (it only reads `num_params`).
6. **The checker core: bidirectional walker + bounds enforcement.** Implement the walker for the un-transformed AST. Implement the bounds-vs-`where` check (§5b, §5d). Unit tests on small snippets (`(fn id (a:A) ->A a)` checks; `(fn main () ->Int (std.+ 1 "x"))` fails with trait error; `(fn add2 (a:A) ->A where ((A Add)) (std.+ a a))` checks; `(fn add2 (a:A) ->A (std.+ a a))` fails with missing-bounds error; `(fn main () ->Int (std.map (std.range 0 5) (fn sq (x:Int) ->Int (std.+ x x))))` checks). Includes `let`-generalization and instantiation.
7. **Wire into compiler + wasm entry points.** One-line calls in `_compile_from_source` and the WASM entry. Existing test corpus will now fail because nothing has annotations yet — gate the checker behind a `typecheck: bool` flag temporarily (env var or compile-time feature) so we can land the wiring without breaking everything.
8. **Migrate the test corpus.** Add annotations and `where` clauses to every test source in `interpreter.rs::test`, `tests/test_eval.rs`, `builtins.rs::test`, `closure.rs::test`. Flip the parser to reject unannotated params (make annotations truly mandatory). Flip the `typecheck` flag to "on". This is the largest mechanical step.
9. **Polish: span-based error messages, error-message quality.** Optional follow-up; can land after step 8.
10. **Docs + TODO update.**

### Estimated effort per step (rough)

| Step | Files | LOC-ish | Risk |
|---|---|---|---|
| 1 | parser.rs (rewrite), Cargo.toml | ~400 | medium (matching old parser behavior) |
| 2 | parser.rs, closure.rs, compiler.rs, wasm.rs | ~120 | low |
| 3 | new typecheck.rs | ~300 | medium (unifier correctness) |
| 4 | typecheck.rs | ~100 | low |
| 5 | builtins.rs | ~80 | low |
| 6 | typecheck.rs | ~500 | medium (bidirectional walker + bounds + let-gen) |
| 7 | compiler.rs, wasm.rs | ~20 | low |
| 8 | all test files | large, mechanical | low but tedious |
| 9 | typecheck.rs, parser.rs | ~150 | low |
| 10 | TODO.md | small | none |

---

## 9. Open questions / follow-ups (not blocking v1)

1. **User-facing `Index` bound syntax.** v1 makes `Index` internal-only (resolved automatically by `std.idx`, not writable in `where`). If users want to write functions that are polymorphic over "any indexable type", they need a way to declare an `Index` bound. Defer until v1 lands; the fix is to promote `Index` to a user-facing trait with an associated type (`Output`), which requires associated-type machinery in the checker.
2. **Closure-transform type verification.** v1 trusts the transform. A follow-up can run the checker on post-transform AST to verify it preserves types.
3. **WASM typed signatures.** The WASM backend currently emits uniform `(i64, i32)×n → (i64, i32)` signatures. With type info available post-checker, we could emit proper typed signatures (and eventually drop the tag for known-typed values). Defer.
4. **Span-based error locations.** The new lexer captures spans; threading them through `AST` and `TypeAst` to `TypeError` is a clean follow-up.
5. **User-defined types.** The type syntax already supports `Named(String)` for arbitrary type names; extending the checker with user-defined algebraic types / records is a larger follow-up.
6. **`Eq` trait membership for `Cell`/`List`.** v1 says all `(Cell A)`/`(List A)` are `Eq` (matches runtime). Tightening to "element must be `Eq`" is a small follow-up if desired.
7. **Associated types for `Index`/`Slice`.** v1 special-cases `idx`. Generalizing to associated-type traits is a follow-up if more associated-type-like builtins appear.