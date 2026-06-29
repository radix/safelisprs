use std::collections::HashMap;
use std::sync::Arc;

use wasm_encoder::{
  BlockType, CodeSection, EntityType, ExportKind, ExportSection, Function, FunctionSection,
  ImportSection, Module, TypeSection, ValType,
};

use crate::parser::{self, Identifier, AST};

/// The implementation of a host function: takes a slice of i64 arguments
/// (one per parameter) and returns a single i64 result.
pub type HostFn = Arc<dyn Fn(&[i64]) -> i64 + Send + Sync>;

/// A description of one host function that the compiled WASM module can
/// import and call. The caller assembles a [`Builtins`] registry of these
/// and passes it to [`compile`].
///
/// Each `Builtin` carries both the signature (used by the compiler to emit the
/// right import/type) and the implementation (used by the embedder to register
/// the host function with the wasmtime `Linker`). This is a bit conflated
/// between compilation and runtime but if you want to compile without knowledge
/// of the implementations, just provide no-op functions.
#[derive(Clone)]
pub struct Builtin {
  /// The WASM import module name. In SafeLisp source this is the part before
  /// the `.` in a qualified call like `(std.+ 1 2)`.
  pub module: String,
  /// The WASM import function name. In SafeLisp source this is the part after
  /// the `.` (or the whole bare name, if the caller registers it that way).
  pub name: String,
  /// The parameter types, in order. All SafeLisp values are currently `i64`,
  /// so this is typically a vec of `ValType::I64`.
  pub params: Vec<ValType>,
  /// The result types. SafeLisp functions currently return a single `i64`.
  pub results: Vec<ValType>,
  /// The host implementation, stored as a type-erased closure over `&[i64]`.
  /// Used by test harnesses and embedders to register the function with a
  /// wasmtime `Linker` without re-stating the signature.
  pub func: HostFn,
}

impl Builtin {
  /// Build a `Builtin` from an explicit `&[i64] -> i64` closure.
  pub fn new(
    module: &str,
    name: &str,
    num_params: usize,
    func: impl Fn(&[i64]) -> i64 + Send + Sync + 'static,
  ) -> Self {
    Builtin {
      module: module.to_string(),
      name: name.to_string(),
      params: (0..num_params).map(|_| ValType::I64).collect(),
      results: vec![ValType::I64],
      func: Arc::new(func),
    }
  }

  /// A nullary (zero-arg) host function.
  pub fn nullary(module: &str, name: &str, func: impl Fn() -> i64 + Send + Sync + 'static) -> Self {
    Builtin::new(module, name, 0, move |_| func())
  }

  /// A unary (one-arg) host function.
  pub fn unary(
    module: &str,
    name: &str,
    func: impl Fn(i64) -> i64 + Send + Sync + 'static,
  ) -> Self {
    Builtin::new(module, name, 1, move |args| func(args[0]))
  }

  /// A binary (two-arg) host function.
  pub fn binary(
    module: &str,
    name: &str,
    func: impl Fn(i64, i64) -> i64 + Send + Sync + 'static,
  ) -> Self {
    Builtin::new(module, name, 2, move |args| func(args[0], args[1]))
  }
}

/// A registry of host functions available to the compiled module. The
/// compiler emits a WASM import for each entry that is actually referenced
/// in the source; unused entries are ignored.
#[derive(Clone, Default)]
pub struct Builtins {
  entries: Vec<Builtin>,
}

impl Builtins {
  pub fn new() -> Self {
    Builtins::default()
  }

  pub fn with_builtin(mut self, builtin: Builtin) -> Self {
    self.entries.push(builtin);
    self
  }

  pub fn add(&mut self, builtin: Builtin) {
    self.entries.push(builtin);
  }

  /// Iterate over all registered builtins. Used by embedders to register
  /// every host function with a wasmtime `Linker`.
  pub fn iter(&self) -> impl Iterator<Item = &Builtin> {
    self.entries.iter()
  }

  /// Look up a builtin by `(module, name)`. Returns the matching entry.
  fn lookup(&self, module: &str, name: &str) -> Option<&Builtin> {
    self
      .entries
      .iter()
      .find(|b| b.module == module && b.name == name)
  }

  /// Look up a builtin by bare name (no module qualifier). This is used when
  /// a `CallFixed` uses an unqualified name that doesn't resolve to a
  /// same-module user function.
  fn lookup_bare(&self, name: &str) -> Option<&Builtin> {
    self.entries.iter().find(|b| b.name == name)
  }
}

/// A function signature, used as a key for type-index deduplication.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Signature {
  params: Vec<ValType>,
  results: Vec<ValType>,
}

/// Compile a SafeLisp source string to a WebAssembly binary module. The
/// `builtins` registry describes the host functions the source may call; the
/// compiler emits a WASM import for each one that is actually referenced.
///
/// All SafeLisp values are represented as `i64` in the generated WASM: `Int`
/// is an `i64`, and `Bool` is `0` or `1` as an `i64`. This avoids any need for
/// type inference while keeping the module statically typed.
///
/// Supported subset (anything else returns a clear "not yet supported"
/// error):
/// - `Int`, `Bool`
/// - `Variable`, `Let`
/// - `If`
/// - `CallFixed` to a same-module function or a registered builtin
///
/// Not supported (returns an error): `Float`, `String`, `Call` (dynamic
/// calls), `Cell`/`DerefCell`/`SetCell`, `PartialApply`, `FunctionRef`, nested
/// `DefineFn`, `Import`, `DeclareFn` (declarations are ignored rather than
/// erroring, since they're just signatures).
pub fn compile(source: &str, builtins: &Builtins) -> Result<Vec<u8>, String> {
  let asts = parser::read_multiple(source)?;
  compile_asts(&asts, builtins)
}

/// Compile a slice of already-parsed top-level AST into a WASM binary.
pub fn compile_asts(asts: &[AST], builtins: &Builtins) -> Result<Vec<u8>, String> {
  ModuleCompiler::new(builtins).compile(asts)
}

/// A top-level function definition, collected during the first pass.
struct FuncDef {
  name: String,
  num_params: u32,
  /// Index into the defined-functions space (i.e. excluding imports). Used
  /// to build the function and export sections, and to resolve same-module
  /// calls.
  def_index: u32,
}

/// One entry in the set of imports actually used by the compiled module,
/// resolved during the discovery pass and assigned a function index.
struct UsedImport {
  /// The builtin descriptor from the registry.
  builtin: Builtin,
  /// The function index in the module's function index space (imports come
  /// first).
  func_index: u32,
  /// The type index for this builtin's signature.
  type_index: u32,
}

struct ModuleCompiler<'b> {
  builtins: &'b Builtins,
  /// All top-level function definitions, in source order.
  functions: Vec<FuncDef>,
  /// Map from function name to its position in `functions` (and thus its
  /// `def_index`), for resolving `CallFixed` to same-module functions.
  function_names: HashMap<String, u32>,
  /// Type-index allocator: maps each distinct signature to a type index,
  /// assigned in first-seen order. Shared by both imports and user functions.
  type_indices: HashMap<Signature, u32>,
  /// The imports actually referenced by the source, keyed by
  /// `(module, name)`. Assigned during the discovery pass.
  used_imports: HashMap<(String, String), UsedImport>,
}

impl<'b> ModuleCompiler<'b> {
  fn new(builtins: &'b Builtins) -> Self {
    ModuleCompiler {
      builtins,
      functions: vec![],
      function_names: HashMap::new(),
      type_indices: HashMap::new(),
      used_imports: HashMap::new(),
    }
  }

  fn compile(mut self, asts: &[AST]) -> Result<Vec<u8>, String> {
    // Pass 0: collect top-level function names so CallFixed can resolve
    // forward references (SafeLisp allows calling a function defined later
    // in the same module). Each defined function gets a `def_index` in the
    // defined-functions space (which starts after all imports).
    for ast in asts {
      if let AST::DefineFn(f) = ast {
        self
          .function_names
          .insert(f.name.clone(), self.functions.len() as u32);
        self.functions.push(FuncDef {
          name: f.name.clone(),
          num_params: f.params.len() as u32,
          def_index: self.functions.len() as u32,
        });
      }
    }

    // Pass 1 (discovery): walk every function body to find which builtins
    // are called. This doesn't emit any code; it just collects the set of
    // imports we'll need and assigns them function indices and type indices.
    for ast in asts {
      if let AST::DefineFn(f) = ast {
        self.discover(f)?;
      }
    }

    let num_imports = self.used_imports.len() as u32;

    // Pass 2 (emit): build the module sections in binary order: type,
    // import, function, export, code.
    let mut module = Module::new();
    self.emit_type_section(&mut module);
    self.emit_import_section(&mut module);
    self.emit_function_section(&mut module, num_imports);
    self.emit_export_section(&mut module, num_imports);
    self.emit_code_section(&mut module, asts, num_imports)?;
    Ok(module.finish())
  }

  /// Allocate (or reuse) a type index for the given signature. The first time
  /// a signature is seen, it gets the next available index; subsequent lookups
  /// return the same index.
  fn type_index(&mut self, sig: Signature) -> u32 {
    let next = self.type_indices.len() as u32;
    *self.type_indices.entry(sig).or_insert(next)
  }

  /// Emit the type section, one entry per distinct signature in first-seen
  /// order (matching the indices assigned by `type_index`).
  fn emit_type_section(&self, module: &mut Module) {
    let mut types = TypeSection::new();
    // Collect and sort by index so the section is emitted in index order.
    let mut entries: Vec<(&Signature, u32)> =
      self.type_indices.iter().map(|(s, i)| (s, *i)).collect();
    entries.sort_by_key(|(_, i)| *i);
    for (sig, _) in entries {
      types.ty().function(sig.params.clone(), sig.results.clone());
    }
    module.section(&types);
  }

  /// Emit the import section, one entry per used builtin, in function-index
  /// order.
  fn emit_import_section(&self, module: &mut Module) {
    let mut imports = ImportSection::new();
    let mut used: Vec<&UsedImport> = self.used_imports.values().collect();
    used.sort_by_key(|u| u.func_index);
    for u in used {
      imports.import(
        &u.builtin.module,
        &u.builtin.name,
        EntityType::Function(u.type_index),
      );
    }
    module.section(&imports);
  }

  /// Emit the function section: one entry per defined function, giving its
  /// type index. Defined functions follow imports in the function index
  /// space.
  fn emit_function_section(&self, module: &mut Module, _num_imports: u32) {
    let mut funcs = FunctionSection::new();
    for def in &self.functions {
      let sig = Signature {
        params: (0..def.num_params).map(|_| ValType::I64).collect(),
        results: vec![ValType::I64],
      };
      let ty = self.type_indices[&sig];
      funcs.function(ty);
    }
    module.section(&funcs);
  }

  /// Emit the export section: export each defined function by its source name.
  fn emit_export_section(&self, module: &mut Module, num_imports: u32) {
    let mut exports = ExportSection::new();
    for def in &self.functions {
      let func_index = num_imports + def.def_index;
      exports.export(&def.name, ExportKind::Func, func_index);
    }
    module.section(&exports);
  }

  /// Emit the code section: the bodies, in the same order as the function
  /// section.
  fn emit_code_section(
    &mut self,
    module: &mut Module,
    asts: &[AST],
    num_imports: u32,
  ) -> Result<(), String> {
    let mut codes = CodeSection::new();
    for ast in asts {
      if let AST::DefineFn(f) = ast {
        let def_index = self.function_names[&f.name];
        let body = self.compile_function(f, def_index, num_imports)?;
        codes.function(&body);
      }
    }
    module.section(&codes);
    Ok(())
  }

  /// Discovery walk over a function body: record every builtin that's called
  /// by assigning it an import function index and a type index (if not
  /// already assigned). Doesn't emit code or error on unknown names (those
  /// are caught in the emit pass).
  fn discover(&mut self, f: &parser::Function) -> Result<(), String> {
    // Ensure the function's own signature has a type index.
    let sig = Signature {
      params: (0..f.params.len() as u32).map(|_| ValType::I64).collect(),
      results: vec![ValType::I64],
    };
    self.type_index(sig);
    for expr in &f.code {
      self.discover_expr(expr)?;
    }
    Ok(())
  }

  fn discover_expr(&mut self, ast: &AST) -> Result<(), String> {
    match ast {
      AST::Int(_) | AST::Bool(_) | AST::Variable(_) => {}
      AST::Let(_, expr) => self.discover_expr(expr)?,
      AST::If(cond, then, els) => {
        self.discover_expr(cond)?;
        self.discover_expr(then)?;
        self.discover_expr(els)?;
      }
      AST::CallFixed(ident, args) => {
        self.resolve_builtin_for_discovery(ident)?;
        for arg in args {
          self.discover_expr(arg)?;
        }
      }
      x => {
        return Err(format!(
          "WASM backend does not yet support this form: {:?}",
          x
        ))
      }
    }
    Ok(())
  }

  /// During discovery, if a `CallFixed` target resolves to a registered
  /// builtin (not a same-module function), record the import. This assigns
  /// the builtin a function index (if not already assigned) and a type index.
  fn resolve_builtin_for_discovery(&mut self, ident: &Identifier) -> Result<(), String> {
    let (module_name, func_name) = match ident {
      Identifier::Bare(n) => (None, n.clone()),
      Identifier::Qualified(m, n) => (Some(m.clone()), n.clone()),
    };
    // Only resolve builtins; same-module function calls don't need imports.
    let builtin = match (module_name.as_deref(), func_name.as_str()) {
      (None, name) if self.function_names.contains_key(name) => return Ok(()),
      (None, name) => self.builtins.lookup_bare(name),
      (Some(m), name) => self.builtins.lookup(m, name),
    };
    if let Some(b) = builtin {
      let key = (b.module.clone(), b.name.clone());
      if !self.used_imports.contains_key(&key) {
        let sig = Signature {
          params: b.params.clone(),
          results: b.results.clone(),
        };
        let type_index = self.type_index(sig);
        let func_index = self.used_imports.len() as u32;
        self.used_imports.insert(
          key,
          UsedImport {
            builtin: b.clone(),
            func_index,
            type_index,
          },
        );
      }
    }
    Ok(())
  }

  /// Compile one function to a `wasm_encoder::Function` (locals + body).
  fn compile_function(
    &mut self,
    f: &parser::Function,
    def_index: u32,
    num_imports: u32,
  ) -> Result<Function, String> {
    // Locals are tracked as a stack of (name, local-index). Parameters occupy
    // indices 0..num_params; each `let` allocates a fresh local index even if
    // it shadows an existing name, which avoids any need to rename or
    // invalidate prior bindings. `Variable` resolves to the most recent
    // binding of that name.
    let num_params = f.params.len() as u32;
    let mut locals: Vec<(String, u32)> = f
      .params
      .iter()
      .enumerate()
      .map(|(i, p)| (p.clone(), i as u32))
      .collect();
    let mut local_count = num_params;

    // Pre-pass to count `let`-introduced locals. `wasm_encoder::Function::new`
    // requires the full locals declaration up front, before any instructions
    // are emitted, so we must know the count before building the body.
    for expr in &f.code {
      self.count_let_locals(expr, &mut locals, &mut local_count)?;
    }
    let num_locals = local_count - num_params;

    // Build the Function. Parameters are declared by the function type, not
    // the locals vector; we declare only the `let`-introduced slots, all i64.
    let func = Function::new([(num_locals, ValType::I64)]);
    let mut func = func;

    // Reset local tracking for the emission pass.
    locals = f
      .params
      .iter()
      .enumerate()
      .map(|(i, p)| (p.clone(), i as u32))
      .collect();
    local_count = num_params;

    let last = f.code.len().saturating_sub(1);
    for (i, expr) in f.code.iter().enumerate() {
      self.compile_expr(
        expr,
        &mut locals,
        &mut local_count,
        def_index,
        num_imports,
        &mut func,
      )?;
      // Non-final body expressions are in statement position: their value is
      // discarded. The interpreter's compiler does the same with `Pop`.
      if i != last {
        func.instructions().drop();
      }
    }
    // A function with an empty body still needs to produce a value. Push
    // `0` (which represents Int 0 / Bool false) so the body's stack depth
    // matches the declared `(result i64)`.
    if f.code.is_empty() {
      func.instructions().i64_const(0);
    }
    // Every function body must end with `end`.
    func.instructions().end();
    Ok(func)
  }

  /// Pre-pass over an expression to count how many new local slots `let`
  /// will introduce. This mirrors `compile_expr`'s `Let` handling so the
  /// local count is identical between the two passes.
  fn count_let_locals(
    &self,
    ast: &AST,
    locals: &mut Vec<(String, u32)>,
    local_count: &mut u32,
  ) -> Result<(), String> {
    match ast {
      AST::Int(_) | AST::Bool(_) | AST::Variable(_) => {}
      AST::Let(name, expr) => {
        self.count_let_locals(expr, locals, local_count)?;
        locals.push((name.clone(), *local_count));
        *local_count += 1;
      }
      AST::If(cond, then, els) => {
        self.count_let_locals(cond, locals, local_count)?;
        self.count_let_locals(then, locals, local_count)?;
        self.count_let_locals(els, locals, local_count)?;
      }
      AST::CallFixed(_, args) => {
        for arg in args {
          self.count_let_locals(arg, locals, local_count)?;
        }
      }
      x => {
        return Err(format!(
          "WASM backend does not yet support this form: {:?}",
          x
        ))
      }
    }
    Ok(())
  }

  fn compile_expr(
    &mut self,
    ast: &AST,
    locals: &mut Vec<(String, u32)>,
    local_count: &mut u32,
    def_index: u32,
    num_imports: u32,
    func: &mut Function,
  ) -> Result<(), String> {
    match ast {
      AST::Int(n) => {
        func.instructions().i64_const(*n);
      }
      AST::Bool(b) => {
        func.instructions().i64_const(if *b { 1 } else { 0 });
      }
      AST::Variable(name) => {
        let idx = self
          .resolve_local(locals, name)
          .ok_or_else(|| format!("unbound variable: {}", name))?;
        func.instructions().local_get(idx);
      }
      AST::Let(name, expr) => {
        // Evaluate the value expression first, then bind it to a fresh local.
        // `let` evaluates to the bound value (matching interpreter semantics:
        // `(let a 1)` returns 1), so we leave the value on the stack via
        // `local.tee` (set-and-keep) instead of local.set + local.get.
        self.compile_expr(expr, locals, local_count, def_index, num_imports, func)?;
        let idx = *local_count;
        *local_count += 1;
        locals.push((name.clone(), idx));
        func.instructions().local_tee(idx);
      }
      AST::If(cond, then, els) => {
        // cond; i64.const 0; i64.ne; if (result i64) then else end
        self.compile_expr(cond, locals, local_count, def_index, num_imports, func)?;
        func.instructions().i64_const(0);
        func.instructions().i64_ne();
        func.instructions().if_(BlockType::Result(ValType::I64));
        self.compile_expr(then, locals, local_count, def_index, num_imports, func)?;
        func.instructions().else_();
        self.compile_expr(els, locals, local_count, def_index, num_imports, func)?;
        func.instructions().end();
      }
      AST::CallFixed(ident, args) => self.compile_call_fixed(
        ident,
        args,
        locals,
        local_count,
        def_index,
        num_imports,
        func,
      )?,
      x => {
        return Err(format!(
          "WASM backend does not yet support this form: {:?}",
          x
        ))
      }
    }
    Ok(())
  }

  #[allow(clippy::too_many_arguments)]
  fn compile_call_fixed(
    &mut self,
    ident: &Identifier,
    args: &[AST],
    locals: &mut Vec<(String, u32)>,
    local_count: &mut u32,
    def_index: u32,
    num_imports: u32,
    func: &mut Function,
  ) -> Result<(), String> {
    let (module_name, func_name) = match ident {
      Identifier::Bare(n) => (None, n.clone()),
      Identifier::Qualified(m, n) => (Some(m.clone()), n.clone()),
    };

    // Resolve the callee to its function index in the module's function
    // index space: imports come first, then defined functions.
    let callee_index = match (module_name.as_deref(), func_name.as_str()) {
      (None, name) if self.function_names.contains_key(name) => {
        // Same-module call: num_imports + target's def_index.
        let _ = def_index;
        let target_def = self.function_names[name];
        num_imports + target_def
      }
      _ => {
        // Try to resolve as a builtin import.
        let key = match &module_name {
          Some(m) => (m.clone(), func_name.clone()),
          None => {
            // Bare name: look up the builtin to get its module.
            let b = self
              .builtins
              .lookup_bare(&func_name)
              .ok_or_else(|| format!("call to unknown function: {}", func_name))?;
            (b.module.clone(), b.name.clone())
          }
        };
        let imp = self
          .used_imports
          .get(&key)
          .ok_or_else(|| format!("call to unknown function: {}.{}", key.0, key.1))?;
        imp.func_index
      }
    };

    // Evaluate args left-to-right onto the stack, then call.
    for arg in args {
      self.compile_expr(arg, locals, local_count, def_index, num_imports, func)?;
    }
    func.instructions().call(callee_index);
    Ok(())
  }

  fn resolve_local(&self, locals: &[(String, u32)], name: &str) -> Option<u32> {
    // Most recent binding wins (inner `let` shadows outer).
    locals
      .iter()
      .rev()
      .find(|(n, _)| n == name)
      .map(|(_, idx)| *idx)
  }
}

/// A convenience function returning the standard set of SafeLisp builtins
/// (`std.+`, `std.-`, `std.==`), all with signature `(i64, i64) -> i64` and
/// the obvious integer implementations. Callers who want a different set
/// can build their own [`Builtins`].
pub fn std_builtins() -> Builtins {
  Builtins::new()
    .with_builtin(Builtin::binary("std", "+", |a, b| a.wrapping_add(b)))
    .with_builtin(Builtin::binary("std", "-", |a, b| a - b))
    .with_builtin(Builtin::binary(
      "std",
      "==",
      |a, b| if a == b { 1 } else { 0 },
    ))
}

#[cfg(test)]
mod test {
  use super::*;
  use wasmtime::{Engine, Linker, Module, Store};

  /// Register every builtin in `builtins` with the given wasmtime `Linker`,
  /// adapting the stored `&[i64] -> i64` implementation to the right typed
  /// `func_wrap` arity.
  fn register_all(linker: &mut Linker<()>, builtins: &Builtins) -> Result<(), String> {
    for b in builtins.iter() {
      let f = b.func.clone();
      let result = match b.params.len() {
        0 => linker.func_wrap(&b.module, &b.name, move || f(&[])),
        1 => linker.func_wrap(&b.module, &b.name, move |a: i64| f(&[a])),
        2 => linker.func_wrap(&b.module, &b.name, move |a: i64, b: i64| f(&[a, b])),
        3 => linker.func_wrap(&b.module, &b.name, move |a: i64, b: i64, c: i64| {
          f(&[a, b, c])
        }),
        n => {
          return Err(format!(
            "unsupported arity {} for builtin {}.{}",
            n, b.module, b.name
          ))
        }
      };
      result.map_err(|e| format!("link {}.{}: {e}", b.module, b.name))?;
    }
    Ok(())
  }

  /// Compile `source` with [`std_builtins`], instantiate it under a wasmtime
  /// `Linker` that auto-registers all builtins, run `main`, and return the
  /// i64 result.
  fn run_main(source: &str) -> Result<i64, String> {
    run_main_with(source, &std_builtins())
  }

  /// Lower-level helper: compile `source` with the given `builtins` registry,
  /// auto-register every builtin with the linker, then instantiate and run
  /// `main`.
  fn run_main_with(source: &str, builtins: &Builtins) -> Result<i64, String> {
    let wasm = compile(source, builtins)?;
    let engine = Engine::default();
    let module = Module::from_binary(&engine, &wasm).map_err(|e| format!("wasm validate: {e}"))?;
    let mut linker: Linker<()> = Linker::new(&engine);
    register_all(&mut linker, builtins)?;
    let mut store: Store<()> = Store::new(&engine, ());
    let instance = linker
      .instantiate(&mut store, &module)
      .map_err(|e| format!("instantiate: {e}"))?;
    let main = instance
      .get_typed_func::<(), i64>(&mut store, "main")
      .map_err(|e| format!("get main: {e}"))?;
    main
      .call(&mut store, ())
      .map_err(|e| format!("call main: {e}"))
  }

  /// Compile `source` and assert that running `main` returns `expected`.
  fn assert_main_eq(source: &str, expected: i64) {
    let result = run_main(source).unwrap_or_else(|e| panic!("run_main failed: {e}"));
    assert_eq!(result, expected, "source: {:?}", source);
  }

  // End-to-end tests shared with the interpreter live in
  // `tests/test_eval.rs`. The tests below are WASM-specific (error cases,
  // validation, and behaviors the interpreter doesn't support or handles
  // differently).

  #[test]
  fn if_branches_can_use_variables_from_let() {
    // This case is WASM-only because the interpreter requires `if` conditions
    // to be `Bool`, and `true` here is `Bool(true)` — but the `let` before it
    // triggers a suspected interpreter bug where `Int(10)` is seen as the
    // condition. The WASM backend treats any non-zero i64 as truthy.
    assert_main_eq("(fn main () (let a 10) (if true a 0))", 10);
  }

  #[test]
  fn empty_function_body_returns_zero() {
    assert_main_eq("(fn main () )", 0);
  }

  #[test]
  fn unsupported_float_errors_clearly() {
    let err = run_main("(fn main () 1.5)").unwrap_err();
    assert!(err.contains("does not yet support"), "got: {err}");
  }

  #[test]
  fn unsupported_string_errors_clearly() {
    let err = run_main("(fn main () \"hi\")").unwrap_err();
    assert!(err.contains("does not yet support"), "got: {err}");
  }

  #[test]
  fn unsupported_dynamic_call_errors_clearly() {
    let err = run_main("(fn main () (undefined 1))").unwrap_err();
    assert!(err.contains("unknown function"), "got: {err}");
  }

  #[test]
  fn compiled_module_validates() {
    // A basic smoke test that the emitted bytes are valid WASM: wasmtime's
    // `Module::from_binary` validates before instantiation.
    let wasm = compile("(fn id (a) a) (fn main () (id 7))", &std_builtins()).unwrap();
    let engine = Engine::default();
    Module::from_binary(&engine, &wasm).expect("emitted wasm should validate");
  }

  #[test]
  fn custom_builtin_with_different_module_name() {
    // A caller can register builtins under any module name, not just "std".
    // The implementation is stored in the Builtin itself, so the test harness
    // auto-registers it — no manual linker wiring needed.
    let builtins = Builtins::new().with_builtin(Builtin::unary("math", "double", |n| n * 2));
    let result = run_main_with("(fn main () (math.double 21))", &builtins).unwrap();
    assert_eq!(result, 42);
  }

  #[test]
  fn bare_builtin_name_resolves() {
    // A builtin can be called with a bare name if it's the only one with
    // that name in the registry. Here we register "double" under module
    // "host"; in source, `(double 21)` resolves to it.
    let builtins = Builtins::new().with_builtin(Builtin::unary("host", "double", |n| n * 2));
    let result = run_main_with("(fn main () (double 21))", &builtins).unwrap();
    assert_eq!(result, 42);
  }

  #[test]
  fn unused_builtins_are_not_emitted() {
    // Only the builtins actually referenced in the source should appear as
    // imports. We check by compiling with all three std builtins but only
    // using one.
    let wasm = compile("(fn main () (std.+ 1 2))", &std_builtins()).unwrap();
    let engine = Engine::default();
    let module = Module::from_binary(&engine, &wasm).unwrap();
    // The module should have exactly one import (std.+), not three.
    let num_imports = module.imports().filter(|i| i.module() == "std").count();
    assert_eq!(num_imports, 1);
  }
}
