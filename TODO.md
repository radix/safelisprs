# TODO

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
