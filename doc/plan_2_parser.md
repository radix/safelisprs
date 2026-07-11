# Plan 2: Replace the `atoms` parser with a hand-written lexer + parser

Rewrite `src/parser.rs` as a hand-written lexer and recursive-descent parser,
drop the `atoms` dependency, and add two capabilities the current parser
lacks: `#` line comments and source-position tracking for parse errors.
**Behavior-compatible**: every source that parses today must produce an
identical `AST`; the public API (`read_multiple(&str) -> Result<Vec<AST>,
String>`, the `AST` / `Function` / `Identifier` types) is unchanged.

Motivation: full control of the grammar for future language evolution,
position-carrying parse errors instead of `atoms`' opaque ones, comments,
and one fewer dependency.

Preconditions (verify before starting — this plan replicates this state):

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
  (New capability — the old parser has no comments.)
- A token starting with a digit, or with `-`/`+` immediately followed by a
  digit, is a number: `Float` if it contains `.`, else `Int`. Anything else
  that isn't a paren, string, or comment is a `Sym` — so `-`, `+`, `==`,
  `std.+`, `roll!` are symbols.
- Strings: double-quoted with the same escape sequences the `atoms` crate
  accepts. **Determine the exact escape set empirically from the existing
  test corpus and `atoms`' behavior** (at minimum `\"`, `\\`, `\n`); the
  differential test (§4) is the safety net.
- Numeric edge cases (`i64` overflow, `1.2.3`, bare `.`) should produce
  parse errors with positions, matching or improving on whatever `atoms`
  does today — again pinned down by the differential test.

## 2. Parser

Recursive descent over the token stream. Grammar (unchanged from current
behavior, plus comments handled in the lexer):

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

Atom / call-shape rules to replicate exactly:

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
  `let` etc. remain usable as ordinary symbols elsewhere if they are today
  (check `atoms` behavior; the differential test decides).
- An empty form `()` is an error ("Empty call"), as today.

Parse errors: `Err(String)` as today, but include line/column (computed from
the token's byte span) and what was expected. Existing tests that assert on
parse-error *text* may need their expected strings updated — behavior
compatibility is required for *accepted* programs, not for error messages.

Keep spans internal to the parser for now: `AST` does not change shape. The
lexer/parser infrastructure retaining spans is the point — consuming them
beyond parse errors is future work.

## 3. Estimated shape

One file, `src/parser.rs`: ~150 LOC lexer + ~250 LOC parser + tests. No
other production file changes except `Cargo.toml`.

## 4. Differential testing, then removal

1. Implement the new parser alongside the old one (temporarily e.g.
   `parser::new_parser` module), with `atoms` still present.
2. Add a differential test: for every SafeLisp source string in the repo's
   test suites (grep the test modules for source literals; also glob any
   `.sl`-style fixture files if present), assert
   `old_parse(src) == new_parse(src)` on the `AST`. Add targeted snippets
   for the edge cases above (negative numbers, operators as symbols, dotted
   symbols in head vs. value position, nested fns, string escapes, empty
   form errors).
3. Flip `read_multiple` to the new parser; run the full suite.
4. Delete the old parser code, the differential test, and the `atoms`
   dependency from `Cargo.toml`.
5. Add new-parser unit tests that survive: comments, spans in error
   messages, the edge-case corpus from step 2.

## Verification

`cargo test` green; `atoms` absent from `Cargo.toml` and `Cargo.lock`;
`# comment` lines usable in a test source; a parse error message contains a
line/column.
