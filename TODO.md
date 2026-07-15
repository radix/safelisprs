# TODO

## Medium priority

- [ ] **consider an AST `Folder`/visitor** to deduplicate the AST walkers (closure transform, patch_cell_access, wasm discover/count/compile_expr, compiler compile_expr). Only go with this if it actually reduces lines of code
- [ ] support imports or a prelude. it's dumb to require people to use `std.+`.

## WASM backend: supporting the full SafeLisp language

WASM is really incomplete, needs a full re-evaluation of what would need to be implemented
