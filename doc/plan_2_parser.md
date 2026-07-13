# Plan 2: Replace the `atoms` parser with a hand-written lexer + parser

Rewrite `src/parser.rs` as a hand-written lexer and recursive-descent parser,
drop the `atoms` dependency, and add two capabilities the current parser
lacks: `#` line comments and source spans on every AST node.
This is intentionally **not behavior-compatible** with every lexical edge case
accepted by `atoms`; the language is not yet in production, so this migration
also tightens and simplifies the language. The `read_multiple(&str) ->
Result<Vec<AST>, String>` API remains, but `AST` intentionally changes from an
enum into a span-bearing struct with an `ASTKind` enum.

Motivation: full control of the grammar for future language evolution, precise
positions for both syntax errors and errors detected while constructing the
AST, `#` comments, and replacement of the broad `atoms` dependency with the
small `unescape` dependency.

Preconditions (verify before starting — this plan starts from this state):

- `src/parser.rs` is `atoms`-based. Special forms: `let`, `fn`, `if`,
  `block`. There is **no** `set!` form.
- A qualified (dotted) symbol in value position parses to
  `ASTKind::FunctionRef(module, name)`; in call-head position to
  `ASTKind::CallFixed(Identifier::Qualified(..))`.
- `true` / `false` parse to `ASTKind::Bool`.

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

- `Sym` in value position: `true`/`false` → `ASTKind::Bool`; a symbol containing
  `.` → split on the *first* `.` into `ASTKind::FunctionRef(module, name)`;
  otherwise → `ASTKind::Variable(name)`.
- Call with a symbol head → `ASTKind::CallFixed(identifier, args)`, where the
  identifier is `Qualified(module, name)` if the symbol contains `.` (split
  on first `.`), else `Bare(name)`. Call with any other expression head →
  `ASTKind::Call(callee, args)`.
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

### Span-bearing AST

Every AST node carries its byte span directly:

```rust
pub type Span = Range<usize>;

pub struct AST {
  pub kind: ASTKind,
  pub span: Span,
}

pub enum ASTKind {
  // Let, DefineFn, Call, atoms, transformation-only nodes, etc.
}

fn parse_expr(&mut self) -> Result<AST, ParseError>;
```

Parser-created nodes use their exact source range. Closure transformations and
other AST rewrites preserve the originating node's span; synthetic helper
nodes inherit the most specific enclosing source span available. AST semantic
equality compares `kind` and ignores `span`, while tests that care about source
locations assert spans explicitly.

For compound forms, the node span runs from the opening `(` through the
closing `)`. For atoms, it is the token span. For EOF-related errors, use an
empty span at the source length. Byte spans are canonical; line/column are
derived only when formatting an error.

## 3. Estimated shape

One file, `src/parser.rs`: ~150 LOC lexer + ~250 LOC parser + tests. No
other production file changes except replacing `atoms` with `unescape` in
`Cargo.toml` and updating `Cargo.lock`.

## 4. Implementation and migration testing

1. Implement the lexer and parser with span-bearing AST results, initially
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
4. Add span tests inside the parser module showing that atoms, nested calls,
   special forms, and their children have the expected byte ranges. Add
   transformation tests showing that closure rewriting preserves spans.
5. Switch `read_multiple` to the new parser, run the full suite, then delete
   the old parser code and remove the `atoms` dependency.

## Verification

`cargo test` green; `atoms` absent from `Cargo.toml` and `Cargo.lock`;
`unescape` is a direct dependency; `#` comment lines work; the intentional
incompatibilities above are covered by tests; parse errors contain
line/column; every parsed AST node carries its span; and closure transformation
tests demonstrate that rewritten nodes retain useful source locations.
