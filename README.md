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
- Mandatory static types, including generic functions and higher-order
  function types
- Random numbers: Create namespaced, deterministic RNGs.
- Garbage Collection: provided by the `gc-arena` rust crate.

Functions annotate every parameter and may declare a return type. An omitted
return type is `Void`; local `let` bindings are inferred and may optionally be
annotated:

```lisp
(fn double (x:Int) ->Int (std.+ x x))

(fn map-double (xs:(List Int)) ->(List Int)
  (std.map xs double))

(fn generic-double (x:A) ->A where ((A Add))
  (std.+ x x))
```

## TODO

- [ ] finish the WASM backend


## About WASM

WASM is not a priority since the point is to implement my own VM, but there is a
partially implemented WASM compiler for the language.
