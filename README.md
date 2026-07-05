# Safelisp

An experimental language implementation which tries very hard to restrict the
executed code to the point that you can safely run untrusted code from randos.

## Features


Security:
- instruction limit: Run N instructions and then pause. Execution is resumable.
- time limit: Run until a duration is met.
- memory limit: Limit bytes of memory usage. This is tracked internally, not using OS metrics.

Language & Library features:
- Closures
- Random numbers: Create namespaced, deterministic RNGs.
- Garbage Collection: provided by the `gc-arena` rust crate.

## TODO

- [ ] finish the WASM backend


## About WASM

WASM is not a priority since the point is to implement my own VM, but there is a
partially implemented WASM compiler for the language.
