# Safelisp (Rust Implementation)

An experimental language implementation which tries very hard to restrict the
executed code to the point that you can safely run untrusted code from randos.

FEATURES:
- instruction limit
- time limit
- memory limit (live GC allocation bytes; stack and frames not counted)

TODO:
- [ ] memory limits: account for stack and frame overhead
