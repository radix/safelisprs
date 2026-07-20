use std::borrow::Cow;
use std::collections::HashMap;
use std::sync::Arc;

use wasm_encoder::{
  BlockType, CodeSection, ConstExpr, ElementSection, Elements, EntityType, ExportKind,
  ExportSection, Function, FunctionSection, Ieee64, ImportSection, Module, RefType, TableSection,
  TableType, TypeSection, ValType,
};

use crate::builtins::{sig, BuiltinSignature, Trait};
use crate::parser::{self, ASTKind, BindingId, Identifier, AST};
use crate::prelude::resolve_module_names;
use crate::types::Signature as TypeSignature;

/// Tag values for the SafeLisp tagged-value representation. Every SafeLisp
/// value on the WASM stack is a `(payload: i64, tag: i32)` pair — payload
/// pushed first, tag last, so the receiver pops tag first.
pub const TAG_INT: i32 = 0;
/// Tag for a floating-point value in the WASM tagged-value representation.
pub const TAG_FLOAT: i32 = 1;
/// Tag for a boolean value in the WASM tagged-value representation.
pub const TAG_BOOL: i32 = 2;
/// Tag for the void value in the WASM tagged-value representation.
pub const TAG_VOID: i32 = 3;
/// Tag for a function reference in the WASM tagged-value representation.
pub const TAG_FUNCTION_REF: i32 = 4;

/// A SafeLisp value as seen on the Rust side (for host functions). The host
/// receives `&[SLValue]` and returns `SlValue`. The WASM-side representation
/// is always `(i32 tag, i64 payload)`, but on the Rust side we use this enum
/// for clarity.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SLValue {
  /// An integer value.
  Int(i64),
  /// A floating-point value.
  Float(f64),
  /// A boolean value.
  Bool(bool),
  /// The void value.
  Void,
  /// A function-table reference.
  FunctionRef(u32),
}

impl SLValue {
  /// The WASM tag for this value's variant.
  pub fn tag(&self) -> i32 {
    match self {
      SLValue::Int(_) => TAG_INT,
      SLValue::Float(_) => TAG_FLOAT,
      SLValue::Bool(_) => TAG_BOOL,
      SLValue::Void => TAG_VOID,
      SLValue::FunctionRef(_) => TAG_FUNCTION_REF,
    }
  }

  /// The i64 payload for this value. Floats are reinterpreted as their bits.
  pub fn payload(&self) -> i64 {
    match self {
      SLValue::Int(n) => *n,
      SLValue::Float(f) => f.to_bits() as i64,
      SLValue::Bool(b) => i64::from(*b),
      SLValue::Void => 0,
      SLValue::FunctionRef(index) => i64::from(*index),
    }
  }

  /// Reconstruct an `SlValue` from a tag and payload.
  pub fn from_parts(tag: i32, payload: i64) -> Self {
    match tag {
      TAG_INT => SLValue::Int(payload),
      TAG_FLOAT => SLValue::Float(f64::from_bits(payload as u64)),
      TAG_BOOL => SLValue::Bool(payload != 0),
      TAG_VOID => SLValue::Void,
      TAG_FUNCTION_REF => SLValue::FunctionRef(payload as u32),
      other => panic!("unknown SafeLisp value tag: {}", other),
    }
  }
}

/// The implementation of a host function: takes a slice of SafeLisp values
/// and returns one SafeLisp value.
pub type HostFn = Arc<dyn Fn(&[SLValue]) -> SLValue + Send + Sync>;

/// A description of one host function that the compiled WASM module can
/// import and call. The caller assembles a [`Builtins`] registry of these
/// and passes it to [`compile`].
#[derive(Clone)]
pub struct Builtin {
  /// The WASM import module name.
  pub module: String,
  /// The WASM import function name.
  pub name: String,
  /// Number of SafeLisp-value parameters (each is a tag+payload pair on the
  /// WASM side, so the actual WASM param count is `num_params * 2`).
  pub num_params: usize,
  /// The SafeLisp-level type checked at call sites.
  signature: BuiltinSignature,
  /// The host implementation.
  pub func: HostFn,
}

impl Builtin {
  /// A unary (one-arg) host function.
  #[cfg(test)]
  fn unary(
    module: &str,
    name: &str,
    signature: BuiltinSignature,
    func: impl Fn(SLValue) -> SLValue + Send + Sync + 'static,
  ) -> Self {
    Builtin {
      module: module.to_string(),
      name: name.to_string(),
      num_params: 1,
      signature,
      func: Arc::new(move |args| func(args[0])),
    }
  }

  /// A binary (two-arg) host function.
  fn binary(
    module: &str,
    name: &str,
    signature: BuiltinSignature,
    func: impl Fn(SLValue, SLValue) -> SLValue + Send + Sync + 'static,
  ) -> Self {
    Builtin {
      module: module.to_string(),
      name: name.to_string(),
      num_params: 2,
      signature,
      func: Arc::new(move |args| func(args[0], args[1])),
    }
  }
}

/// A registry of host functions available to the compiled module.
#[derive(Clone, Default)]
pub struct Builtins {
  entries: Vec<Builtin>,
}

impl Builtins {
  /// Create an empty registry.
  fn new() -> Self {
    Builtins::default()
  }

  /// Add a builtin to this registry (builder style).
  fn with_builtin(mut self, builtin: Builtin) -> Self {
    self.entries.push(builtin);
    self
  }

  /// Iterate over all registered builtins.
  pub fn iter(&self) -> impl Iterator<Item = &Builtin> {
    self.entries.iter()
  }

  /// Look up a builtin by `(module, name)`.
  fn lookup(&self, module: &str, name: &str) -> Option<&Builtin> {
    self
      .entries
      .iter()
      .find(|b| b.module == module && b.name == name)
  }
}

/// The WASM type pair for a SafeLisp value: `(i64 payload, i32 tag)`. Used
/// for every parameter and every result.
const SLVAL: [ValType; 2] = [ValType::I64, ValType::I32];

/// Build the WASM param list for a function with `n` SafeLisp-value params.
/// Each value is a `(i64, i32)` pair, so the total WASM param count is `n * 2`.
fn slval_param_types(n: u32) -> Vec<ValType> {
  (0..n).flat_map(|_| SLVAL.iter().copied()).collect()
}

/// A function signature, used as a key for type-index deduplication.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Signature {
  params: Vec<ValType>,
  results: Vec<ValType>,
}

/// Compile a SafeLisp source string to a WebAssembly binary module.
///
/// All SafeLisp values are represented as `(i64 payload, i32 tag)` pairs on
/// the WASM stack. The tag discriminates Int (0), Float (1), Bool (2), and
/// Void (3).
pub fn compile(
  source: &str,
  builtins: &Builtins,
  prelude: &[(&str, &str)],
) -> Result<Vec<u8>, String> {
  let asts = parser::read_multiple(source)?;
  let module_symbols = builtins
    .iter()
    .filter(|builtin| builtin.module == "main")
    .map(|builtin| builtin.name.as_str())
    .collect::<Vec<_>>();
  let asts = resolve_module_names("main", &asts, prelude, &module_symbols)?;
  let checked = check_types(asts, builtins).map_err(|error| error.render(source))?;
  ModuleCompiler::new(builtins).compile(checked.asts())
}

/// Compile a slice of already-parsed top-level AST into a WASM binary.
pub fn compile_asts(
  asts: &[AST],
  builtins: &Builtins,
  prelude: &[(&str, &str)],
) -> Result<Vec<u8>, String> {
  let module_symbols = builtins
    .iter()
    .filter(|builtin| builtin.module == "main")
    .map(|builtin| builtin.name.as_str())
    .collect::<Vec<_>>();
  let asts = resolve_module_names("main", asts, prelude, &module_symbols)?;
  let checked = check_types(asts, builtins).map_err(|error| error.to_string())?;
  ModuleCompiler::new(builtins).compile(checked.asts())
}

fn check_types(
  asts: Vec<AST>,
  builtins: &Builtins,
) -> Result<crate::typecheck::CheckedModule, crate::typecheck::TypeError> {
  crate::typecheck::typecheck_named(
    asts,
    builtins.iter().map(|builtin| {
      (
        builtin.module.as_str(),
        builtin.name.as_str(),
        &builtin.signature,
      )
    }),
  )
}

/// Metadata for a top-level function definition, collected during the first
/// compiler pass.
struct FuncDef {
  name: String,
  num_params: u32,
  def_index: u32,
}

/// A builtin import that the source actually references, with its assigned
/// function and type indices.
struct UsedImport {
  builtin: Builtin,
  func_index: u32,
  type_index: u32,
}

struct ModuleCompiler<'b> {
  builtins: &'b Builtins,
  functions: Vec<FuncDef>,
  function_names: HashMap<String, u32>,
  type_indices: HashMap<Signature, u32>,
  used_imports: HashMap<(String, String), UsedImport>,
  /// Type index for the `if` block type: `() -> (i64, i32)`, i.e. an SlValue
  /// pair result with no params. Allocated during discovery so it's present
  /// when the type section is emitted.
  if_block_type: Option<u32>,
}

impl<'b> ModuleCompiler<'b> {
  fn new(builtins: &'b Builtins) -> Self {
    ModuleCompiler {
      builtins,
      functions: vec![],
      function_names: HashMap::new(),
      type_indices: HashMap::new(),
      used_imports: HashMap::new(),
      if_block_type: None,
    }
  }

  fn compile(mut self, asts: &[AST]) -> Result<Vec<u8>, String> {
    for ast in asts {
      if let ASTKind::DefineFn(f) = &ast.kind {
        self
          .function_names
          .insert(f.name.name.clone(), self.functions.len() as u32);
        self.functions.push(FuncDef {
          name: f.name.name.clone(),
          num_params: f.params.len() as u32,
          def_index: self.functions.len() as u32,
        });
      }
    }

    for ast in asts {
      if let ASTKind::DefineFn(f) = &ast.kind {
        self.discover(f)?;
      }
    }

    let num_imports = self.used_imports.len() as u32;

    let mut module = Module::new();
    self.emit_type_section(&mut module);
    self.emit_import_section(&mut module);
    self.emit_function_section(&mut module, num_imports);
    self.emit_table_section(&mut module, num_imports);
    self.emit_export_section(&mut module, num_imports);
    self.emit_element_section(&mut module, num_imports);
    self.emit_code_section(&mut module, asts, num_imports)?;
    Ok(module.finish())
  }

  /// Allocate (or reuse) a type index for the given signature.
  fn type_index(&mut self, sig: Signature) -> u32 {
    let next = self.type_indices.len() as u32;
    *self.type_indices.entry(sig).or_insert(next)
  }

  /// Emit the type section, one entry per distinct signature in index order.
  fn emit_type_section(&self, module: &mut Module) {
    let mut types = TypeSection::new();
    let mut entries: Vec<(&Signature, u32)> =
      self.type_indices.iter().map(|(s, i)| (s, *i)).collect();
    entries.sort_by_key(|(_, i)| *i);
    for (sig, _) in entries {
      types.ty().function(sig.params.clone(), sig.results.clone());
    }
    module.section(&types);
  }

  /// Emit the import section, one entry per used builtin.
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

  /// Emit the function section: one entry per defined function.
  fn emit_function_section(&self, module: &mut Module, _num_imports: u32) {
    let mut funcs = FunctionSection::new();
    for def in &self.functions {
      let sig = Signature {
        params: slval_param_types(def.num_params),
        results: SLVAL.to_vec(),
      };
      let ty = self.type_indices[&sig];
      funcs.function(ty);
    }
    module.section(&funcs);
  }

  fn emit_table_section(&self, module: &mut Module, num_imports: u32) {
    let function_count = num_imports + self.functions.len() as u32;
    let mut tables = TableSection::new();
    tables.table(TableType {
      element_type: RefType::FUNCREF,
      table64: false,
      minimum: u64::from(function_count),
      maximum: Some(u64::from(function_count)),
      shared: false,
    });
    module.section(&tables);
  }

  fn emit_element_section(&self, module: &mut Module, num_imports: u32) {
    let function_count = num_imports + self.functions.len() as u32;
    let indices: Vec<u32> = (0..function_count).collect();
    let mut elements = ElementSection::new();
    elements.active(
      None,
      &ConstExpr::i32_const(0),
      Elements::Functions(Cow::Owned(indices)),
    );
    module.section(&elements);
  }

  /// Emit the export section: export each defined function by name.
  fn emit_export_section(&self, module: &mut Module, num_imports: u32) {
    let mut exports = ExportSection::new();
    for def in &self.functions {
      let func_index = num_imports + def.def_index;
      exports.export(&def.name, ExportKind::Func, func_index);
    }
    module.section(&exports);
  }

  /// Emit the code section: the compiled bodies, in function-section order.
  fn emit_code_section(
    &mut self,
    module: &mut Module,
    asts: &[AST],
    num_imports: u32,
  ) -> Result<(), String> {
    let mut codes = CodeSection::new();
    for ast in asts {
      if let ASTKind::DefineFn(f) = &ast.kind {
        let body = self.compile_function(f, num_imports)?;
        codes.function(&body);
      }
    }
    module.section(&codes);
    Ok(())
  }

  /// Discovery pass over a function: allocate type indices for the function's
  /// signature and any builtins it calls.
  fn discover(&mut self, f: &parser::Function) -> Result<(), String> {
    let sig = Signature {
      params: slval_param_types(f.params.len() as u32),
      results: SLVAL.to_vec(),
    };
    self.type_index(sig);
    // Ensure the `if` block type exists: `() -> (i64, i32)`.
    if self.if_block_type.is_none() {
      let sig = Signature {
        params: vec![],
        results: SLVAL.to_vec(),
      };
      self.if_block_type = Some(self.type_index(sig));
    }
    for expr in &f.code {
      self.discover_expr(expr)?;
    }
    Ok(())
  }

  /// Recursively discover imports and unsupported forms in an expression.
  fn discover_expr(&mut self, ast: &AST) -> Result<(), String> {
    match &ast.kind {
      ASTKind::Int(_) | ASTKind::Float(_) | ASTKind::Bool(_) | ASTKind::Variable(_) => {}
      ASTKind::FunctionRef(module, name) => {
        self.record_builtin_import(module, name);
      }
      ASTKind::Let(_, _, expr) => self.discover_expr(expr)?,
      ASTKind::If(cond, then, els) => {
        self.discover_expr(cond)?;
        self.discover_expr(then)?;
        self.discover_expr(els)?;
      }
      ASTKind::Block(body) => {
        for expr in body {
          self.discover_expr(expr)?;
        }
      }
      ASTKind::CallFixed(ident, args) => {
        self.resolve_builtin_for_discovery(ident)?;
        self.type_index(Signature {
          params: slval_param_types(args.len() as u32),
          results: SLVAL.to_vec(),
        });
        for arg in args {
          self.discover_expr(arg)?;
        }
      }
      ASTKind::Call(callable, args) => {
        self.type_index(Signature {
          params: slval_param_types(args.len() as u32),
          results: SLVAL.to_vec(),
        });
        self.discover_expr(callable)?;
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

  /// During discovery, record a builtin import if the call target resolves to
  /// a registered builtin (not a same-module function).
  fn resolve_builtin_for_discovery(&mut self, ident: &Identifier) -> Result<(), String> {
    let builtin = match ident {
      Identifier::Bare(_) => return Ok(()),
      Identifier::Qualified(module, name) => self.builtins.lookup(module, name),
    };
    if let Some(b) = builtin {
      let key = (b.module.clone(), b.name.clone());
      if !self.used_imports.contains_key(&key) {
        let sig = Signature {
          params: slval_param_types(b.num_params as u32),
          results: SLVAL.to_vec(),
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

  fn record_builtin_import(&mut self, module: &str, name: &str) {
    let Some(b) = self.builtins.lookup(module, name) else {
      return;
    };
    let key = (b.module.clone(), b.name.clone());
    if !self.used_imports.contains_key(&key) {
      let sig = Signature {
        params: slval_param_types(b.num_params as u32),
        results: SLVAL.to_vec(),
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

  /// Compile one function. Each SafeLisp-value local is two WASM locals:
  /// a payload (i64) and a tag (i32). Parameters are interleaved in the
  /// function signature as (payload, tag) pairs. Declared locals (from
  /// `let`) are grouped: all i64 payloads first, then all i32 tags — this
  /// is required by WASM's local declaration format.
  ///
  /// Local index layout:
  /// - Params: `0..2*num_params`, interleaved as (payload, tag) pairs.
  /// - Declared payload locals (i64): `2*num_params..2*num_params+num_lets`
  /// - Declared tag locals (i32): `2*num_params+num_lets..2*num_params+2*num_lets`
  ///
  /// We store the *pair index* in `locals` (0-based: 0 for first param, 1 for
  /// second, etc.) and convert to actual WASM local indices when emitting.
  fn compile_function(
    &mut self,
    f: &parser::Function,
    num_imports: u32,
  ) -> Result<Function, String> {
    let num_params = f.params.len() as u32;
    // `locals` stores (name, pair_index) where pair_index is the index of
    // the SafeLisp value (0-based: param 0 is pair 0, param 1 is pair 1,
    // first let is pair num_params, etc.).
    let mut locals: Vec<(BindingId, u32)> = f
      .params
      .iter()
      .enumerate()
      .map(|(i, (name, _))| (name.binding, i as u32))
      .collect();
    let mut next_pair = num_params;

    for expr in &f.code {
      self.count_let_locals(expr, &mut locals, &mut next_pair)?;
    }
    let num_lets = next_pair - num_params;

    // Declare locals: num_lets i64 payloads, then num_lets i32 tags.
    let func = Function::new([(num_lets, ValType::I64), (num_lets, ValType::I32)]);
    let mut func = func;

    // Reset for emission pass.
    locals = f
      .params
      .iter()
      .enumerate()
      .map(|(i, (name, _))| (name.binding, i as u32))
      .collect();
    next_pair = num_params;

    let last = f.code.len().saturating_sub(1);
    let returns_void = f.returns_void();
    for (i, expr) in f.code.iter().enumerate() {
      self.compile_expr(
        expr,
        &mut locals,
        &mut next_pair,
        num_params,
        num_lets,
        num_imports,
        &mut func,
      )?;
      if i != last || returns_void {
        // Drop the value pair (payload, tag) from the stack.
        func.instructions().drop();
        func.instructions().drop();
      }
    }
    if returns_void || f.code.is_empty() {
      self.emit_void(&mut func);
    }
    func.instructions().end();
    Ok(func)
  }

  /// Convert a pair index to the WASM local index for the payload (i64).
  /// Params are interleaved; declared locals are grouped.
  fn payload_local(pair_idx: u32, num_params: u32) -> u32 {
    if pair_idx < num_params {
      // Param: interleaved as (payload, tag, payload, tag, ...)
      pair_idx * 2
    } else {
      // Declared local: payloads are grouped after all params.
      2 * num_params + (pair_idx - num_params)
    }
  }

  /// Convert a pair index to the WASM local index for the tag (i32).
  fn tag_local(pair_idx: u32, num_params: u32, num_lets: u32) -> u32 {
    if pair_idx < num_params {
      // Param: interleaved
      pair_idx * 2 + 1
    } else {
      // Declared local: tags are grouped after all payloads.
      2 * num_params + num_lets + (pair_idx - num_params)
    }
  }

  /// Emit a Void value: payload 0 (i64) + tag TAG_VOID (i32).
  fn emit_void(&self, func: &mut Function) {
    func.instructions().i64_const(0);
    func.instructions().i32_const(TAG_VOID);
  }

  /// Pre-pass to count `let`-introduced locals before emitting instructions.
  fn count_let_locals(
    &self,
    ast: &AST,
    locals: &mut Vec<(BindingId, u32)>,
    next_pair: &mut u32,
  ) -> Result<(), String> {
    match &ast.kind {
      ASTKind::Int(_)
      | ASTKind::Float(_)
      | ASTKind::Bool(_)
      | ASTKind::Variable(_)
      | ASTKind::FunctionRef(_, _) => {}
      ASTKind::Let(name, _, expr) => {
        self.count_let_locals(expr, locals, next_pair)?;
        if self.resolve_local(locals, name.binding).is_none() {
          locals.push((name.binding, *next_pair));
          *next_pair += 1;
        }
      }
      ASTKind::If(cond, then, els) => {
        self.count_let_locals(cond, locals, next_pair)?;
        self.count_let_locals(then, locals, next_pair)?;
        self.count_let_locals(els, locals, next_pair)?;
      }
      ASTKind::Block(body) => {
        for expr in body {
          self.count_let_locals(expr, locals, next_pair)?;
        }
      }
      ASTKind::CallFixed(_, args) => {
        for arg in args {
          self.count_let_locals(arg, locals, next_pair)?;
        }
      }
      ASTKind::Call(callable, args) => {
        self.count_let_locals(callable, locals, next_pair)?;
        for arg in args {
          self.count_let_locals(arg, locals, next_pair)?;
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

  /// Emit instructions for one expression, leaving its `(payload, tag)` pair on
  /// the stack.
  #[allow(clippy::too_many_arguments)]
  fn compile_expr(
    &mut self,
    ast: &AST,
    locals: &mut Vec<(BindingId, u32)>,
    next_pair: &mut u32,
    num_params: u32,
    num_lets: u32,
    num_imports: u32,
    func: &mut Function,
  ) -> Result<(), String> {
    match &ast.kind {
      ASTKind::Int(n) => {
        func.instructions().i64_const(*n);
        func.instructions().i32_const(TAG_INT);
      }
      ASTKind::Float(f) => {
        func.instructions().f64_const(Ieee64::from(*f));
        func.instructions().i64_reinterpret_f64();
        func.instructions().i32_const(TAG_FLOAT);
      }
      ASTKind::Bool(b) => {
        func.instructions().i64_const(if *b { 1 } else { 0 });
        func.instructions().i32_const(TAG_BOOL);
      }
      ASTKind::Variable(name) => {
        if let Some(pair_idx) = self.resolve_local(locals, name.binding) {
          let pl = Self::payload_local(pair_idx, num_params);
          let tl = Self::tag_local(pair_idx, num_params, num_lets);
          func.instructions().local_get(pl);
          func.instructions().local_get(tl);
        } else {
          return Err(format!("unbound variable: {}", name));
        }
      }
      ASTKind::FunctionRef(module, name) => {
        let index = self.resolve_function_ref(module, name, num_imports)?;
        func.instructions().i64_const(i64::from(index));
        func.instructions().i32_const(TAG_FUNCTION_REF);
      }
      ASTKind::Let(name, _, expr) => {
        self.compile_expr(
          expr,
          locals,
          next_pair,
          num_params,
          num_lets,
          num_imports,
          func,
        )?;
        let pair_idx = match self.resolve_local(locals, name.binding) {
          Some(pair_idx) => pair_idx,
          None => {
            let pair_idx = *next_pair;
            *next_pair += 1;
            locals.push((name.binding, pair_idx));
            pair_idx
          }
        };
        let pl = Self::payload_local(pair_idx, num_params);
        let tl = Self::tag_local(pair_idx, num_params, num_lets);
        // Stack: [..., payload(i64), tag(i32)].
        // We want to store both to locals and leave the pair on the stack
        // as the let's result value. `local.set` the tag (consumes it),
        // then `local.tee` the payload (consumes and leaves it), then
        // `local.get` the tag back.
        func.instructions().local_set(tl);
        func.instructions().local_tee(pl);
        func.instructions().local_get(tl);
      }
      ASTKind::If(cond, then, els) => {
        self.compile_expr(
          cond,
          locals,
          next_pair,
          num_params,
          num_lets,
          num_imports,
          func,
        )?;
        // Drop tag, check payload != 0.
        func.instructions().drop();
        func.instructions().i64_const(0);
        func.instructions().i64_ne();
        let bt = BlockType::FunctionType(
          self
            .if_block_type
            .expect("if block type allocated in discovery"),
        );
        func.instructions().if_(bt);
        self.compile_expr(
          then,
          locals,
          next_pair,
          num_params,
          num_lets,
          num_imports,
          func,
        )?;
        func.instructions().else_();
        self.compile_expr(
          els,
          locals,
          next_pair,
          num_params,
          num_lets,
          num_imports,
          func,
        )?;
        func.instructions().end();
      }
      ASTKind::Block(body) => {
        let last_idx = body.len().saturating_sub(1);
        for (i, expr) in body.iter().enumerate() {
          self.compile_expr(
            expr,
            locals,
            next_pair,
            num_params,
            num_lets,
            num_imports,
            func,
          )?;
          if i != last_idx {
            // Discard this sub-expression's (payload, tag) pair.
            func.instructions().drop();
            func.instructions().drop();
          }
        }
      }
      ASTKind::CallFixed(ident, args) => self.compile_call_fixed(
        ident,
        args,
        locals,
        next_pair,
        num_params,
        num_lets,
        num_imports,
        func,
      )?,
      ASTKind::Call(callable, args) => self.compile_call_dynamic(
        callable,
        args,
        locals,
        next_pair,
        num_params,
        num_lets,
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

  /// Emit a call to a same-module function or a builtin import.
  #[allow(clippy::too_many_arguments)]
  fn compile_call_fixed(
    &mut self,
    ident: &Identifier,
    args: &[AST],
    locals: &mut Vec<(BindingId, u32)>,
    next_pair: &mut u32,
    num_params: u32,
    num_lets: u32,
    num_imports: u32,
    func: &mut Function,
  ) -> Result<(), String> {
    let callee_index = match ident {
      Identifier::Bare(name) => return Err(format!("call to unknown function: {name}")),
      Identifier::Qualified(module, name) if module == "main" => self
        .function_names
        .get(name)
        .map(|target_def| num_imports + target_def)
        .ok_or_else(|| format!("call to unknown function: {module}::{name}"))?,
      Identifier::Qualified(module, name) => {
        let key = (module.clone(), name.clone());
        let imp = self
          .used_imports
          .get(&key)
          .ok_or_else(|| format!("call to unknown function: {}::{}", key.0, key.1))?;
        imp.func_index
      }
    };

    for arg in args {
      self.compile_expr(
        arg,
        locals,
        next_pair,
        num_params,
        num_lets,
        num_imports,
        func,
      )?;
    }
    func.instructions().call(callee_index);
    Ok(())
  }

  #[allow(clippy::too_many_arguments)]
  fn compile_call_dynamic(
    &mut self,
    callable: &AST,
    args: &[AST],
    locals: &mut Vec<(BindingId, u32)>,
    next_pair: &mut u32,
    num_params: u32,
    num_lets: u32,
    num_imports: u32,
    func: &mut Function,
  ) -> Result<(), String> {
    for arg in args {
      self.compile_expr(
        arg,
        locals,
        next_pair,
        num_params,
        num_lets,
        num_imports,
        func,
      )?;
    }
    self.compile_expr(
      callable,
      locals,
      next_pair,
      num_params,
      num_lets,
      num_imports,
      func,
    )?;
    func.instructions().drop();
    func.instructions().i32_wrap_i64();
    let signature = Signature {
      params: slval_param_types(args.len() as u32),
      results: SLVAL.to_vec(),
    };
    let type_index = self.type_indices[&signature];
    func.instructions().call_indirect(0, type_index);
    Ok(())
  }

  fn resolve_function_ref(
    &self,
    module: &str,
    name: &str,
    num_imports: u32,
  ) -> Result<u32, String> {
    if module == "main" {
      return self
        .function_names
        .get(name)
        .map(|index| num_imports + index)
        .ok_or_else(|| format!("unknown function: {module}::{name}"));
    }
    self
      .used_imports
      .get(&(module.to_string(), name.to_string()))
      .map(|import| import.func_index)
      .ok_or_else(|| format!("unknown function: {module}::{name}"))
  }

  /// Resolve a variable name to its local pair index (most recent binding wins).
  fn resolve_local(&self, locals: &[(BindingId, u32)], binding: BindingId) -> Option<u32> {
    locals
      .iter()
      .rev()
      .find(|(candidate, _)| *candidate == binding)
      .map(|(_, idx)| *idx)
  }
}

/// A convenience function returning the standard set of SafeLisp builtins.
pub fn std_builtins() -> Builtins {
  Builtins::new()
    .with_builtin(Builtin::binary(
      "std",
      "+",
      sig(
        &[("A", &[Trait::Add])],
        vec![TypeSignature::var("A"), TypeSignature::var("A")],
        None,
        TypeSignature::var("A"),
      ),
      |a, b| match (a, b) {
        (SLValue::Int(x), SLValue::Int(y)) => SLValue::Int(x.wrapping_add(y)),
        (SLValue::Float(x), SLValue::Float(y)) => SLValue::Float(x + y),
        _ => SLValue::Void,
      },
    ))
    .with_builtin(Builtin::binary(
      "std",
      "-",
      sig(
        &[("A", &[Trait::Sub])],
        vec![TypeSignature::var("A"), TypeSignature::var("A")],
        None,
        TypeSignature::var("A"),
      ),
      |a, b| match (a, b) {
        (SLValue::Int(x), SLValue::Int(y)) => SLValue::Int(x - y),
        (SLValue::Float(x), SLValue::Float(y)) => SLValue::Float(x - y),
        _ => SLValue::Void,
      },
    ))
    .with_builtin(Builtin::binary(
      "std",
      "==",
      sig(
        &[("A", &[Trait::Eq])],
        vec![TypeSignature::var("A"), TypeSignature::var("A")],
        None,
        TypeSignature::Bool,
      ),
      |a, b| SLValue::Bool(a == b),
    ))
}

#[cfg(all(test, feature = "wasm-tests"))]
mod wasm_tests;
