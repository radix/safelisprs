# Plan 2: Replace the `atoms` parser with a hand-written lexer + parser

Rewrite `src/parser.rs` as a hand-written lexer and recursive-descent parser,
drop the `atoms` dependency, and add two capabilities the current parser
lacks: `#` line comments and source spans at every AST-construction site.
This is intentionally **not behavior-compatible** with every lexical edge case
accepted by `atoms`; the language is not yet in production, so this migration
also tightens and simplifies the language. The public API
(`read_multiple(&str) -> Result<Vec<AST>, String>`, the `AST` / `Function` /
`Identifier` types) remains unchanged.

Motivation: full control of the grammar for future language evolution, precise
positions for both syntax errors and errors detected while constructing the
AST, `#` comments, and replacement of the broad `atoms` dependency with the
small `unescape` dependency.

Preconditions (verify before starting — this plan starts from this state):

- `src/parser.rs` is `atoms`-based. Special forms: `let`, `fn`, `if`,
  `block`. There is **no** `set!` form.
- A qualified (dotted) symbol in value position parses to
  `AST::FunctionRef(module, name)`; in call-head position to
  `CallFixed(Identifier::Qualified(..))`.
- `true` / `false` parse to `AST::Bool`.

If the tree doesn't match this (e.g. `set!` still exists), stop and flag it.

---

## 1. Lexer

A token stream over the raw source:

```rust
struct Token { kind: TokenKind, span: Span }   // Span = byte start..end

enum TokenKind {
  LParen,
  RParen,
  Sym(String),    // identifiers, operators, qualified names: `let`, `+`, `std.+`, `a`
  Int(i64),
  Float(f64),
  Str(String),    // "..." with escapes processed
  Eof,
}
```

Rules:

- Whitespace separates tokens and is otherwise ignored.
- `#` begins a comment that runs to end of line; skipped like whitespace.
  This replaces `atoms`' `;` comment syntax. `;` has no special comment
  meaning in the new language and may occur in a symbol.
- A token starting with a digit, or with `-`/`+` immediately followed by a
  digit, is a *numeric candidate*. Numeric candidates must match exactly one
  of these decimal, non-scientific forms:

  ```text
  Int   := [+-]?[0-9]+
  Float := [+-]?[0-9]+\.[0-9]+
  ```

  Parse `Int` directly as `i64`; overflow is a positioned parse error and
  must not fall back to `f64`. Parse `Float` directly as `f64` and reject a
  non-finite result. A numeric candidate that matches neither grammar (for
  example `1.2.3`, `1e3`, or `123abc`) is a positioned parse error, not a
  symbol. Symbols therefore cannot begin with a digit or with a sign followed
  by a digit. Anything else that is not a paren, string, or comment is a
  `Sym`, so `-`, `+`, `==`, `std.+`, and `roll!` are symbols.
- Strings are double-quoted. Collect the escaped contents and pass them to the
  `unescape` crate; if it returns `None`, produce a positioned invalid-escape
  error. Add `unescape` as a direct dependency. Escapes in symbols are not
  supported.
- A bare `.` remains a symbol. A leading-dot form such as `.5` is also a
  symbol, because it does not begin with a digit; only the numeric forms above
  are recognized as numbers.

## 2. Parser

Recursive descent over the token stream. Grammar (including the intentional
requirement that every function has at least one body expression):

```
Program    := Form*
Form       := "(" Inner ")" | Atom
Inner      := "let" Sym Expr
            | "fn" Sym "(" Sym* ")" Expr+
            | "if" Expr Expr Expr
            | "block" Expr+
            | Expr Expr*                      -- a call
Atom       := Int | Float | Str | Sym
```

Atom / call-shape rules:

- `Sym` in value position: `true`/`false` → `AST::Bool`; a symbol containing
  `.` → split on the *first* `.` into `AST::FunctionRef(module, name)`;
  otherwise → `AST::Variable(name)`.
- Call with a symbol head → `AST::CallFixed(identifier, args)`, where the
  identifier is `Qualified(module, name)` if the symbol contains `.` (split
  on first `.`), else `Bare(name)`. Call with any other expression head →
  `AST::Call(callee, args)`.
- `fn` params must be symbols; `let` takes exactly a symbol and one
  expression; `if` exactly three expressions; `block` one or more. Special
  forms are recognized only when the symbol is the first element of a form —
  `let`, `fn`, `if`, and `block` remain ordinary symbols in other positions.
- A `fn` must have at least one body expression. `(fn name ())` is a parse
  error even though the old parser happened to accept it.
- An empty form `()` is an error ("Empty call"), as today.

Internally, use a structured error carrying at least a byte span, a message,
and any useful expected-token information. At the public `read_multiple`
boundary, render it to `String` with line/column and what was expected. The
public error type remains `String` for now.

### Span readiness invariant

Keep spans internal for now, but do not discard them before AST construction.
Parser functions should return a concrete span-carrying AST result, for
example:

```rust
type Span = Range<usize>;

struct SpannedAst {
  ast: AST,
  span: Span,
}

fn parse_expr(&mut self) -> Result<SpannedAst, ParseError>;
```

Every place that constructs an `AST` node must have the full span for that
node available. When constructing a parent node, the `SpannedAst` values for
its children must still be in scope, so their spans are also available for
validation errors and for a future span-carrying AST. A child's wrapper is
discarded only at the point where its `ast` is inserted into the current
unspanned parent; `read_multiple` similarly discards each top-level wrapper to
preserve the current public return type. This should make adding spans to the
AST later a mechanical data-model change rather than a parser rewrite.

For compound forms, the node span runs from the opening `(` through the
closing `)`. For atoms, it is the token span. For EOF-related errors, use an
empty span at the source length. Byte spans are canonical; line/column are
derived only when formatting an error.

## 3. Estimated shape

One file, `src/parser.rs`: ~150 LOC lexer + ~250 LOC parser + tests. No
other production file changes except replacing `atoms` with `unescape` in
`Cargo.toml` and updating `Cargo.lock`.

## 4. Implementation and migration testing

1. Implement the lexer and parser with spanned internal results, initially
   leaving `atoms` in the dependency list only until the new parser compiles
   and the existing SafeLisp test corpus passes.
2. Add targeted tests for the language rules that remain important: negative
   integers and floats, operators as symbols, dotted symbols in head versus
   value position, nested functions, string escapes through `unescape`, empty
   call errors, and special-form arity errors.
3. Add tests for the intentional language changes:
   - `#` comments work and `;` does not begin a comment.
   - `fn` requires at least one body expression.
   - `1e3`, `1.2.3`, and number-prefixed symbols are rejected.
   - the `i64` maximum and minimum parse, while positive and negative overflow
     produce positioned errors rather than floats.
   - malformed and non-finite decimal floats are rejected.
   - symbol escapes are not accepted as escapes.
4. Add span-readiness tests inside the parser module that inspect internal
   `SpannedAst` results: atoms, nested calls, and special forms have the
   expected byte ranges before `read_multiple` strips them.
5. Switch `read_multiple` to the new parser, run the full suite, then delete
   the old parser code and remove the `atoms` dependency.

## Verification

`cargo test` green; `atoms` absent from `Cargo.toml` and `Cargo.lock`;
`unescape` is a direct dependency; `#` comment lines work; the intentional
incompatibilities above are covered by tests; parse errors contain
line/column; and parser-internal tests demonstrate that full node spans are
available at AST-construction sites.
