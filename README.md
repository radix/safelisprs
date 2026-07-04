# Safelisp (Rust Implementation)

An experimental language implementation which tries very hard to restrict the
executed code to the point that you can safely run untrusted code from randos.

FEATURES:
- instruction limit
- time limit
- memory limit (live GC allocation bytes + per-Gc-box external payload
  (String/List/Partial backing) + stack/frames/locals Vec overhead)

TODO:
- [ ] memory limits: account for `Cell` contents (currently undercounted —
      the `RefLock<SLVal>` box is not wrapped in `Accounted`)
