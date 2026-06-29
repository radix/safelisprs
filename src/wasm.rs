use std::collections::HashMap;

use wasm_encoder::{
  BlockType, CodeSection, EntityType, ExportKind, ExportSection, Function, FunctionSection,
  ImportSection, Module, TypeSection, ValType,
};

use crate::parser::{self, Identifier, AST};

/// The name of the WASM import module exposing `std.+`, `std.-`, `std.==` to
/// compiled modules.
pub const STD_IMPORT_MODULE: &str = "std";

/// Compile a SafeLisp source string to a WebAssembly binary module. The
/// module exports one function per top-level `(fn …)` definition; builtins
/// (`std.+`, `std.-`, `std.==`) are imported from a host module named `std`
/// and must be supplied by the embedder (see tests for the wasmtime wiring).
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
/// - `CallFixed` to a same-module function or a `std.+`/`std.-`/`std.==`
///   builtin
///
/// Not supported (returns an error): `Float`, `String`, `Call` (dynamic
/// calls), `Cell`/`DerefCell`/`SetCell`, `PartialApply`, `FunctionRef`, nested
/// `DefineFn`, `Import`, `DeclareFn` (declarations are ignored rather than
/// erroring, since they're just signatures).
pub fn compile(source: &str) -> Result<Vec<u8>, String> {
  let asts = parser::read_multiple(source)?;
  compile_asts(&asts)
}

/// Compile a slice of already-parsed top-level AST into a WASM binary.
pub fn compile_asts(asts: &[AST]) -> Result<Vec<u8>, String> {
  ModuleCompiler::new().compile(asts)
}

/// The set of `std.*` builtins this backend knows how to import. Each maps a
/// SafeLisp builtin name to the name used in the WASM import; the host must
/// supply a matching `(func (param i64 i64) (result i64))` under the `std`
/// module.
fn std_builtin_import_name(name: &str) -> Option<&'static str> {
  match name {
    "+" => Some("add"),
    "-" => Some("sub"),
    "==" => Some("eq"),
    _ => None,
  }
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

struct ModuleCompiler {
  /// All top-level function definitions, in source order.
  functions: Vec<FuncDef>,
  /// Map from function name to its position in `functions` (and thus its
  /// `def_index`), for resolving `CallFixed` to same-module functions.
  function_names: HashMap<String, u32>,
  /// Type indices for `(func (param i64 * N) (result i64))` keyed by param
  /// count. The 2-param entry is shared with the builtin signature. Filled
  /// by `assign_type_indices` before the emit pass.
  func_type_indices: HashMap<u32, u32>,
  /// Map from builtin import name ("add"/"sub"/"eq") to its function index
  /// in the module's function index space (imports come first). Filled by
  /// `assign_import_indices` before the emit pass, in a deterministic order
  /// derived from `discovery`.
  builtin_import_indices: HashMap<&'static str, u32>,
  /// The total number of imported functions. Imports occupy the leading
  /// slots of the function index space; defined functions follow. Set by
  /// `assign_import_indices`.
  num_imports: u32,
}

impl ModuleCompiler {
  fn new() -> Self {
    ModuleCompiler {
      functions: vec![],
      function_names: HashMap::new(),
      func_type_indices: HashMap::new(),
      builtin_import_indices: HashMap::new(),
      num_imports: 0,
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

    // Pass 1 (discovery): walk every function body to find which `std.*`
    // builtins are called and which param counts are used by user functions.
    // This doesn't emit any code; it just collects the sets we need to size
    // the type and import sections before the emit pass.
    let mut used_builtins: Vec<&'static str> = Vec::new();
    let mut used_param_counts: Vec<u32> = Vec::new();
    for ast in asts {
      if let AST::DefineFn(f) = ast {
        self.discover(f, &mut used_builtins, &mut used_param_counts)?;
      }
    }

    // Assign type indices deterministically. The builtin 2-arg signature
    // `(param i64 i64) (result i64)`, if used, is always type 0 (so the
    // import section can reference it). User-function types follow in
    // ascending param-count order; the 2-param case reuses the builtin type.
    let mut next_type = 0u32;
    let builtin_used = used_builtins
      .iter()
      .any(|n| *n == "add" || *n == "sub" || *n == "eq");
    if builtin_used {
      self.func_type_indices.insert(2, 0);
      next_type = 1;
    }
    used_param_counts.sort_unstable();
    used_param_counts.dedup();
    for n in &used_param_counts {
      if self.func_type_indices.contains_key(n) {
        continue;
      }
      self.func_type_indices.insert(*n, next_type);
      next_type += 1;
    }

    // Assign import function indices. Imports occupy the leading slots of
    // the function index space. We emit them in a fixed, sorted order so
    // the import section and the call targets agree.
    used_builtins.sort_unstable();
    used_builtins.dedup();
    for (i, name) in used_builtins.iter().enumerate() {
      self.builtin_import_indices.insert(name, i as u32);
    }
    self.num_imports = used_builtins.len() as u32;

    // Pass 2 (emit): build the module sections in binary order: type,
    // import, function, export, code.
    let mut module = Module::new();

    // Type section. Emit in index order: the builtin 2-arg type first (if
    // used), then one type per distinct param count in ascending order.
    let mut types = TypeSection::new();
    if builtin_used {
      types
        .ty()
        .function(vec![ValType::I64, ValType::I64], vec![ValType::I64]);
    }
    for n in used_param_counts {
      if n == 2 && builtin_used {
        continue;
      }
      let params: Vec<ValType> = (0..n).map(|_| ValType::I64).collect();
      types.ty().function(params, vec![ValType::I64]);
    }
    module.section(&types);

    // Import section: one entry per used builtin, in the sorted order we
    // assigned indices above. All builtins share the 2-arg i64 type. When no
    // builtins are used, we emit an empty (but valid) import section.
    let mut imports = ImportSection::new();
    if builtin_used {
      let builtin_type = self.func_type_indices[&2];
      for name in &used_builtins {
        imports.import(STD_IMPORT_MODULE, name, EntityType::Function(builtin_type));
      }
    }
    module.section(&imports);

    // Function section: one entry per defined function, giving its type
    // index. Defined functions follow imports in the function index space.
    let mut funcs = FunctionSection::new();
    for def in &self.functions {
      let ty = self.func_type_indices[&def.num_params];
      funcs.function(ty);
    }
    module.section(&funcs);

    // Export section: export each defined function by its source name.
    let mut exports = ExportSection::new();
    for def in &self.functions {
      let func_index = self.num_imports + def.def_index;
      exports.export(&def.name, ExportKind::Func, func_index);
    }
    module.section(&exports);

    // Code section: the bodies, in the same order as the function section.
    let mut codes = CodeSection::new();
    for ast in asts {
      if let AST::DefineFn(f) = ast {
        let def_index = self.function_names[&f.name];
        let body = self.compile_function(f, def_index)?;
        codes.function(&body);
      }
    }
    module.section(&codes);

    Ok(module.finish())
  }

  /// Discovery walk over a function body: record every `std.*` builtin that's
  /// called and every param count used by called user functions. Doesn't
  /// emit code or error on unknown names (those are caught in the emit pass).
  fn discover(
    &self,
    f: &parser::Function,
    used_builtins: &mut Vec<&'static str>,
    used_param_counts: &mut Vec<u32>,
  ) -> Result<(), String> {
    used_param_counts.push(f.params.len() as u32);
    for expr in &f.code {
      self.discover_expr(expr, used_builtins, used_param_counts)?;
    }
    Ok(())
  }

  #[allow(clippy::only_used_in_recursion)]
  fn discover_expr(
    &self,
    ast: &AST,
    used_builtins: &mut Vec<&'static str>,
    used_param_counts: &mut Vec<u32>,
  ) -> Result<(), String> {
    match ast {
      AST::Int(_) | AST::Bool(_) | AST::Variable(_) => {}
      AST::Let(_, expr) => self.discover_expr(expr, used_builtins, used_param_counts)?,
      AST::If(cond, then, els) => {
        self.discover_expr(cond, used_builtins, used_param_counts)?;
        self.discover_expr(then, used_builtins, used_param_counts)?;
        self.discover_expr(els, used_builtins, used_param_counts)?;
      }
      AST::CallFixed(ident, args) => {
        // Record the callee if it's a builtin or a same-module function.
        let (module_name, func_name) = match ident {
          Identifier::Bare(n) => (None, n.clone()),
          Identifier::Qualified(m, n) => (Some(m.clone()), n.clone()),
        };
        match (module_name.as_deref(), func_name.as_str()) {
          (None, name) if self.function_names.contains_key(name) => {
            // User function call: we don't know the target's param count
            // without resolving it, but all user functions are in
            // `self.functions`, which we already recorded above. No-op.
            let _ = name;
          }
          (None, name) | (Some(STD_IMPORT_MODULE), name)
            if std_builtin_import_name(name).is_some() =>
          {
            used_builtins.push(std_builtin_import_name(name).unwrap());
          }
          _ => {}
        }
        for arg in args {
          self.discover_expr(arg, used_builtins, used_param_counts)?;
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

  /// Compile one function to a `wasm_encoder::Function` (locals + body).
  fn compile_function(&mut self, f: &parser::Function, def_index: u32) -> Result<Function, String> {
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
      self.compile_expr(expr, &mut locals, &mut local_count, def_index, &mut func)?;
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
        self.compile_expr(expr, locals, local_count, def_index, func)?;
        let idx = *local_count;
        *local_count += 1;
        locals.push((name.clone(), idx));
        func.instructions().local_tee(idx);
      }
      AST::If(cond, then, els) => {
        // cond; i64.const 0; i64.ne; if (result i64) then else end
        self.compile_expr(cond, locals, local_count, def_index, func)?;
        func.instructions().i64_const(0);
        func.instructions().i64_ne();
        func.instructions().if_(BlockType::Result(ValType::I64));
        self.compile_expr(then, locals, local_count, def_index, func)?;
        func.instructions().else_();
        self.compile_expr(els, locals, local_count, def_index, func)?;
        func.instructions().end();
      }
      AST::CallFixed(ident, args) => {
        self.compile_call_fixed(ident, args, locals, local_count, def_index, func)?
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

  fn compile_call_fixed(
    &mut self,
    ident: &Identifier,
    args: &[AST],
    locals: &mut Vec<(String, u32)>,
    local_count: &mut u32,
    def_index: u32,
    func: &mut Function,
  ) -> Result<(), String> {
    let (module_name, func_name) = match ident {
      Identifier::Bare(n) => (None, n.clone()),
      Identifier::Qualified(m, n) => (Some(m.clone()), n.clone()),
    };

    // Resolve the callee to its function index in the module's function
    // index space: imports come first, then defined functions. Both sets
    // of indices were assigned before the emit pass in `compile`.
    let callee_index = match (module_name.as_deref(), func_name.as_str()) {
      (None, name) if self.function_names.contains_key(name) => {
        // Same-module call: num_imports + target's def_index.
        let _ = def_index;
        let target_def = self.function_names[name];
        self.num_imports + target_def
      }
      (None, name) | (Some(STD_IMPORT_MODULE), name) if std_builtin_import_name(name).is_some() => {
        // Builtin call: looked up in the pre-assigned import map.
        let imp_name = std_builtin_import_name(name).unwrap();
        *self
          .builtin_import_indices
          .get(imp_name)
          .expect("discovery should have recorded this builtin")
      }
      (Some(m), name) => {
        return Err(format!(
          "WASM backend can only call same-module functions or std builtins, got {}.{}",
          m, name
        ))
      }
      (None, name) => return Err(format!("call to unknown function: {}", name)),
    };

    // Evaluate args left-to-right onto the stack, then call.
    for arg in args {
      self.compile_expr(arg, locals, local_count, def_index, func)?;
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

#[cfg(test)]
mod test {
  use super::*;
  use wasmtime::{Engine, Linker, Module, Store};

  /// Compile `source` to a WASM binary, instantiate it under a wasmtime
  /// `Linker` that supplies the three std builtins, run `main`, and return
  /// the i64 result.
  fn run_main(source: &str) -> Result<i64, String> {
    let wasm = compile(source)?;
    let engine = Engine::default();
    let module = Module::from_binary(&engine, &wasm).map_err(|e| format!("wasm validate: {e}"))?;
    let mut linker: Linker<()> = Linker::new(&engine);
    linker
      .func_wrap(STD_IMPORT_MODULE, "add", |a: i64, b: i64| a.wrapping_add(b))
      .map_err(|e| format!("link add: {e}"))?;
    linker
      .func_wrap(STD_IMPORT_MODULE, "sub", |a: i64, b: i64| a - b)
      .map_err(|e| format!("link sub: {e}"))?;
    linker
      .func_wrap(STD_IMPORT_MODULE, "eq", |a: i64, b: i64| -> i64 {
        if a == b {
          1
        } else {
          0
        }
      })
      .map_err(|e| format!("link eq: {e}"))?;
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
    let err = compile("(fn main () 1.5)").unwrap_err();
    assert!(err.contains("does not yet support"), "got: {err}");
  }

  #[test]
  fn unsupported_string_errors_clearly() {
    let err = compile("(fn main () \"hi\")").unwrap_err();
    assert!(err.contains("does not yet support"), "got: {err}");
  }

  #[test]
  fn unsupported_dynamic_call_errors_clearly() {
    let err = compile("(fn main () (undefined 1))").unwrap_err();
    assert!(err.contains("unknown function"), "got: {err}");
  }

  #[test]
  fn compiled_module_validates() {
    // A basic smoke test that the emitted bytes are valid WASM: wasmtime's
    // `Module::from_binary` validates before instantiation.
    let wasm = compile("(fn id (a) a) (fn main () (id 7))").unwrap();
    let engine = Engine::default();
    Module::from_binary(&engine, &wasm).expect("emitted wasm should validate");
  }
}
