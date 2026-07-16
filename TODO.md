# TODO

## Medium priority

- [ ] **consider an AST `Folder`/visitor** to deduplicate the AST walkers (closure transform, patch_cell_access, wasm discover/count/compile_expr, compiler compile_expr). Only go with this if it actually reduces lines of code
- [ ] imports: `(use std)` or `(use rand)`. probably not very high priority now that we have prelude support.
- [ ] **consider expression-level type ascription**, e.g. `(the (List Int) expr)`, if ambiguous non-`let` expressions ever become useful. Today ambiguity can usually be fixed with a `let` annotation or by deleting dead code.
- [ ] **consider opaque nominal host-handle types**, e.g. `Rng` instead of `(Cell Int)` for `rand::rng`, if exposing RNG state as a mutable cell turns out to be a footgun.

## WASM backend: supporting the full SafeLisp language

WASM is really incomplete, needs a full re-evaluation of what would need to be implemented

## Future type-system directions

- [ ] **user-defined algebraic/record types** should come before user-defined traits; without new user types, marker traits do not classify much beyond the closed built-in type set.
- [ ] **constraint traits without methods** should stay erased. `(trait Ord)` plus `(impl Ord Int)` can become a pass-0 trait/impl table, with `Trait` changing from a Rust enum to an interned name and `satisfies` consulting the table.
- [ ] **method-bearing traits need a dispatch strategy** because generic calls like `(show a)` cannot be resolved by erased constraints alone. Dictionary passing is likely the best fit: add hidden dictionary params in an AST-to-AST pass, similar in shape to closure conversion. Avoid monomorphization unless there is a very strong reason.
- [ ] **avoid conditional impls unless deliberately designed**. Recursive membership like `(impl Eq (List A) where ((A Eq)))` turns trait satisfaction into a solver. If `Eq` is ever tightened from universal, prefer one hardcoded recursive case over a general mechanism at first.
- [ ] **avoid associated types until necessary**. An `Index` trait with an `Output` type would add projection normalization and type-level functions to unification. This is why `std::idx` is list-only for now; string indexing can wait for a much bigger type-system step.
