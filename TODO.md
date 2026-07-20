# TODO

## Medium priority

- [ ] imports: `(use std)` or `(use rand)`. probably not very high priority now that we have prelude support.
- [ ] **consider expression-level type ascription**, e.g. `(the (List Int) expr)`, if ambiguous non-`let` expressions ever become useful. Today ambiguity can usually be fixed with a `let` annotation or by deleting dead code.
- [x] Make rng return `Rng`, which wraps a `(Cell Int)` instead of returning one directly.

## WASM backend: supporting the full SafeLisp language

WASM is really incomplete, needs a full re-evaluation of what would need to be implemented

## Future type-system directions

I decided to forego traits as a user-facing feature for now.
