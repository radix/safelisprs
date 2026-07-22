# Safelisp

An experimental language implementation which tries very hard to restrict the
executed code to the point that you can safely run untrusted code from randos.

 Safelisp is kinda like a lisp but not really. It is statically typed
and has whitespace layout. Sorry, haters!

## Rust API

SafeLisp is primarily exposed as a Rust library. Build a `Library` of your
custom host builtins, then you can compile source to bytecode and execute the
bytecode.

```rust
use safelisp::{
  compile_executable_from_source, Builtin, Interpreter, Library, SLValue, Signature, Status, Value,
  sig,
};

let source = r#"
fn main () -> Int
  (host::square 12)
"#;

let library = Library::default().with_builtin(Builtin::unary(
  "host",
  "square",
  sig(&[], vec![Signature::Int], None, Signature::Int),
  |value| match value {
    Value::Int(n) => Ok(Value::Int(n * n)),
    // type-checking prevents this path from being taken
    other => Err(format!("square expected Int, got {}", other.type_name())),
  },
));

let package = compile_executable_from_source(source, ("main", "main"), &library)?;
let interpreter = Interpreter::with_library(package, library);
let mut execution = interpreter.call_main()?;

assert_eq!(execution.run_until_done()?, SLValue::Int(144));
```

### Instruction Count Limits

`Execution::run(n)` executes at most `n` bytecode instructions. If the program
does not finish within that budget, it returns `Status::Paused`; the same
execution can be resumed later with another call to `run`.

```rust
let source = r#"
fn loop () -> Int
  (loop)

fn main () -> Int
  (loop)
"#;

let package = compile_executable_from_source(source, ("main", "main"), &Library::default())?;
let interpreter = Interpreter::new(package);
let mut execution = interpreter.call_main()?;

assert_eq!(execution.run(10)?, Status::Paused);
assert_eq!(execution.executed(), 10);

// Resume with another budget:
assert_eq!(execution.run(10)?, Status::Paused);
assert_eq!(execution.executed(), 20);
```

### Memory Limits

`Execution::set_memory_limit(Some(bytes))` caps live memory for that execution.
The limit includes GC-managed values plus runtime-tracked Rust heap storage such
as strings, lists, closures, and interpreter stack/frame vectors.

```rust
let source = r#"
fn grow (s:String n:Int) -> String
  if (== n 0)
    s
  else
    (grow (concat s s) (- n 1))

fn main () -> String
  (grow "x" 20)
"#;

let package = compile_executable_from_source(source, ("main", "main"), &Library::default())?;
let interpreter = Interpreter::new(package);
let mut execution = interpreter.call_main()?;
execution.set_memory_limit(Some(64 * 1024));

let error = execution.run_until_done().unwrap_err();
assert!(error.contains("memory limit exceeded"));
```

## Language Tour

SafeLisp has a Lisp core with an indentation-based layout syntax for the common
special forms. Function calls use parentheses, but special forms (`fn`, `if`,
`else`, `match`, `let`, `struct`, `enum`, `new`, and `block`) can be written
without outer parens.

### Functions

Functions require type annotations. An omitted return type means `Void`. A
function body may contain multiple expressions, and the final expression is the
return value.

```lisp
fn double (x:Int) -> Int
  (+ x x)

fn sum-to (n:Int) -> Int
  if (== n 0)
    0
  else
    (+ n (sum-to (- n 1)))

fn main () -> Int
  let x 21
  (double x)
```

Function values are first class. Builtins and user functions can be passed to
higher-order functions:

```lisp
fn inc (x:Int) -> Int
  (+ x 1)

fn main () -> (List Int)
  (map (list 1 2 3) inc)
```

### Custom Types

Programs can define structs:

```lisp
struct Point
  x:Int
  y:Int

fn length-ish (pt:Point) -> Int
  (+ pt.x pt.y)

fn origin () -> Point
  new Point
    x: 3
    y: 4

fn main () -> Int
  (length-ish (origin))
```

And we have `enum` and `match`. Variant patterns list the fields to bind by
their declared names.

```lisp
enum MaybeInt
  (Some value:Int)
  (None)

fn get-or-zero (maybe:MaybeInt) -> Int
  match maybe
    (Some value) => # this has to be `value`, not some other name
      let bumped (+ value 1)
      bumped
    (None) => 0

fn answer () -> MaybeInt
  new MaybeInt::Some
    value: 41

fn main () -> Int
  (get-or-zero (answer))
```

### Cells

Variables and struct fields are immutable. In the default language and library,
the only mutation primitive is `Cell`: create one with `cell`, read it with
`get`, and update it with `set!`.

```lisp
fn main () -> Int
  let counter (cell 0)
  (set! counter (+ (get counter) 1))
  (set! counter (+ (get counter) 41))
  (get counter)
```

### Closures

Nested functions can capture values from their surrounding scope. A closure that
needs mutable state captures a `Cell`.

```lisp
fn counter () -> (Fn () -> Int)
  let count (cell 0)
  fn inc () -> Int
    (set! count (+ 1 (get count)))
    (get count)
  inc

fn main () -> Int
  let next (counter)
  (next) # returns 1
  (next) # returns 2
  (next) # returns 3
```

### Builtin Host Functions

Host functions are registered in a `Library`. Their signatures participate in
compile-time type checking, while their Rust handlers run at interpretation
time.

```rust
let library = Library::default()
  .with_builtin(Builtin::binary(
    "host",
    "clamp-min",
    sig(&[], vec![Signature::Int, Signature::Int], None, Signature::Int),
    |value, minimum| match (value, minimum) {
      (Value::Int(value), Value::Int(minimum)) => Ok(Value::Int(value.max(minimum))),
      (value, minimum) => Err(format!(
        "clamp-min expected (Int, Int), got ({}, {})",
        value.type_name(),
        minimum.type_name(),
      )),
    },
  ));
```

SafeLisp ships with a default host library containing these prelude functions:

- arithmetic and equality: `+`, `-`, `==`
- strings and lists: `concat`, `list`, `len`, `idx`, `push`, `range`, `slice`
- mutable cells: `cell`, `get`, `set!`
- higher-order helpers: `map`
- deterministic random values: `rand::rng`, `rand::roll!`

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

## TODO

- [ ] finish the WASM backend


## About WASM

WASM is not a priority since the point is to implement my own VM, but there is a
partially implemented WASM compiler for the language.
