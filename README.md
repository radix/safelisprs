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
fn grow (s: String n: Int) -> String
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

### Parse Depth Limits

The parser is recursive descent, so deeply nested source (for example, many
thousands of nested parentheses) would otherwise overflow the native stack while
*compiling*. `CompileOptions::max_parse_depth` caps the nesting depth the parser
will accept, returning an error instead.

```rust
use safelisp::CompileOptions;

let options = CompileOptions::default().max_parse_depth(1024);
let package = safelisp::compile_executable_from_source_with_options(
  source,
  ("main", "main"),
  &library,
  &options,
)?;
```

## Language Tour

SafeLisp has a Lisp core with an indentation-based layout syntax for the common
special forms. Function calls use parentheses, but special forms (`fn`, `if`,
`else`, `match`, `let`, `struct`, `enum`, `new`, `block`, `return`, `and`,
`or`, `for`, and `bind`) can be written without outer parens.

### Functions

Functions require type annotations. An omitted return type means `Void`. A
function body may contain multiple expressions, and the final expression is the
return value.

```lisp
fn double (x: Int) -> Int
  (+ x x)

fn sum-to (n: Int) -> Int
  if (== n 0)
    0
  else
    (+ n (sum-to (- n 1)))

fn main () -> Int
  let x 21
  (double x)
```

Functions may also return early. `(return)` returns `Void`; `(return value)` returns a value.

```lisp
fn first-or-zero (xs: (List Int)) -> Int
  for x in xs
    return x
  0
```

The `else` branch of an `if` is optional. An `if` with no `else` evaluates the
then branch for side effects only and produces `Void`:

```lisp
fn bump-if-zero (x: Int) -> Int
  let result x
  if (== x 0)
    (= result 1)
  result
```

### Boolean forms

`and` and `or` accept only Boolean operands and evaluate them left to right,
stopping as soon as the result is known. They are variadic and require at least
two operands.

```lisp
fn should-send (enabled: Bool has-recipient: Bool) -> Bool
  (and enabled has-recipient)
```

### List iteration

`for x in xs` is iteration for side-effects. It evaluates to `Void`.

```lisp
fn visit-all (xs: (List Int))
  for x in xs
    (host::visit x)

fn visit-all-parenthesized (xs: (List Int))
  (for x in xs
    (host::visit x))
```

Function values are first class. Builtins and user functions can be passed to
higher-order functions:

```lisp
fn inc (x: Int) -> Int
  (+ x 1)

fn main () -> (List Int)
  (map (list 1 2 3) inc)
```

### Custom Types

Programs can define structs:

```lisp
struct Point
  x: Int
  y: Int

fn length-ish (pt: Point) -> Int
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
  (Some value: Int)
  (None)

fn get-or-zero (maybe: MaybeInt) -> Int
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

### Tuples

Anonymous tuples bundle two or more values of possibly different types. A tuple
type is written `(Tuple T1 T2 ...)`, a value is constructed with `(Tuple v1 v2
...)`, and elements are accessed positionally with `.0`, `.1`, and so on.

```lisp
fn foo () -> (Tuple Int String)
  (Tuple 3 "foo")

fn main () -> Int
  let result (foo)
  result.0
```

Tuples require at least two elements. They are first-class values: they can be
returned from functions, stored in lists, and passed to or returned from host
builtins.

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

### Variable Bindings: `let`, `shd`, and `=`

SafeLisp has three binding forms:

- `let` introduces a new binding. Once a name is bound in the current
  function, `let` cannot bind it again.
- `shd` shadows an existing binding by introducing a fresh one. It is
  equivalent to `let`, but explicitly calls out that shadowing is happening and
  does not error when the name is already in scope. It may change the binding's
  type.
- `=` assigns to an existing binding. The name must already be bound in the
  current local scope, and the new value must have the same type as the
  existing binding.

```lisp
fn main () -> Int
  let x 1
  = x 2
  x  # 2
```

Don't mistake `shd` or `=` for actual mutation. For example, if a closure
captures a value and then the outer scope shadows the name with `shd`, the
closure still sees the original captured value. In other words, neither `shd`
nor `=` allows observable mutation through closures.

```lisp
fn main () -> Int
  let a 1
  fn inner () -> Int
    a  # sees the captured value 1, not 2
  shd a 2
  (inner)
```

This returns 1, but if you move the `shd a 2` to before the `fn inner`, it will
instead return 2. See the section on `cell` to see how we allow explicit
mutation.

`shd` may change a binding's type, to facilitate common patterns like:

```lisp
fn main () -> Creature
  let creature (load-file "creature.json")
  shd creature (parse-json creature)
  creature
```

`=` requires the same type; use `shd` when you need to change it.

`=` can be used to reassign a variable even inside `for`, `if`, and `match`:

```lisp
fn find-three () -> Bool
  let found false
  for x in (range 0 10)
    if (== x 3)
      (= found true)
  found
```

`shd` inside `if`, `match`, or `for` introduces a binding that is local to that
branch or loop body; the outer binding is unchanged afterward. `=` inside those
forms reassigns the outer binding, and every branch (or loop iteration) must
agree on the type.

### Tuple Binding: `bind`

`bind` destructures a tuple into positional bindings in one form. Each pattern
is one of:

- `(let name)` — introduce a new binding.
- `(shd name)` — shadow an existing binding with a fresh one (may change type).
- `(= name)` — reassign an existing binding (requires the same type).

Patterns are matched against the tuple elements in order.

```lisp
fn main () -> Int
  let list (std::list 1 2 3 4 5)
  bind ((= list) (let result)) (remove-idx list 5)
  result
```

This desugars to a `block` that evaluates the expression once, binds each
element positionally for its side effects, and then yields the whole tuple:

```lisp
block
  let __tmp (remove-idx list 5)
  = list __tmp.0
  let result __tmp.1
  __tmp
```

A `bind` expression evaluates to the entire tuple value; in a `Void` function
that value is discarded. The number of patterns must match the tuple's arity.

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
- deterministic random values: `rand::rng`, `rand::roll!`, `rand::choice!`

### Host-Defined Types

A `Library` may also declare custom types with `with_type`. Source code refers to
them by their qualified `module::Type` name and constructs them with `new`:

- `new module::Struct` constructs a library-defined struct.
- `new module::Enum::Variant` constructs a library-defined enum variant.

```rust
let library = Library::new()
  .with_type(CustomTypeSpec::enum_(
    "host",
    "MaybeInt",
    vec![("Some", vec![("value", Signature::Int)]), ("None", vec![])],
  ))
  .with_type(CustomTypeSpec::struct_("host", "Box", vec![("value", Signature::Int)]));
```

```lisp
fn main () -> Int
  let maybe (new host::MaybeInt::Some value: 42)
  let boxed (new host::Box value: maybe)
  match maybe
    (Some value) => value
    (None) => 0
```

### Deriving Conversions for Rust Types

When a host type needs to round-trip through SafeLisp values, derive
`SafelispValue` on a Rust struct or enum instead of hand-writing the
`CustomTypeSpec` and conversion functions. The derive is available when the
`derive` feature is enabled (it is on by default):

```rust
use safelisp::SafelispValue;

#[derive(SafelispValue)]
#[safelisp(module = "arp")]
enum Dice {
  Expr { num: u8, size: u8 },
  Plus(
    #[safelisp(field = "left")] Box<Dice>,
    #[safelisp(field = "right")] Box<Dice>,
  ),
  Flat { value: i8 },
  BestOf(
    #[safelisp(field = "count")] u8,
    #[safelisp(field = "dice")] Box<Dice>,
  ),
}
```

This implements the `SafelispValue` trait (`to_value` / `from_value` /
`sl_signature`) and the `SafelispType` trait (`type_spec`). Register the type
with the compiler and convert at the host boundary:

```rust
let library = Library::new().with_type(Dice::type_spec());

// inside a builtin, where `ctx: &mut HostCtx`:
let value: Value = dice.to_value(ctx)?;
let back: Dice = Dice::from_value(ctx, value)?;
```

Every field type must itself implement `SafelispValue`; the crate provides impls
for basic Rust types.

Safelisp doesn't support positional fields in enum variants, so each one must be
given an explicit SafeLisp name with `#[safelisp(field = "name")]`. This lets
source code construct and match the variant by those names:

```lisp
fn main () -> Int
  let flat (new arp::Dice::Flat value:3)
  match (new arp::Dice::BestOf count:2 dice:flat)
    (BestOf count dice) => count
    _ => 0
```

Omitting the annotation on a positional field is a compile error.

`from_value` guards against deeply-nested guest values by limiting the depth.
This can be important to avoid blowing the stack both during the `from_value`
call and during later recursive processing of your data structures. The default
limit is `SafelispValue::DEFAULT_FROM_VALUE_DEPTH`; pass an explicit limit via
`from_value_with_depth` when you need more or less:

```rust
let back: Dice = Dice::from_value_with_depth(ctx, value, 16)?;
```

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
