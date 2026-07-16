use std::collections::HashMap;

use crate::builtins::BuiltinSpec;
use crate::closure::transform_closures_in_module;
use crate::parser::{self, ASTKind, BindingId, Identifier, AST};
use crate::prelude::resolve_module_names;

/// A Package can either represent a "program" or a "library".
/// If a `main` is provided, then it can be executed as a program directly.
#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Package {
  /// While the names here *can* be used for function lookup, they are only
  /// included for debugging purposes. In a complete "executable" package, all
  /// calls will be represented with index-based function offsets.
  pub modules: LinkedModules,
  pub main: Option<(u32, u32)>,
}

pub type LinkedCallable = Callable<(u32, u32), (u32, u32)>;
type CompiledCallable = Callable<(String, String), (String, String)>;

/// Packages contain Callables, which can either be LinkedFunctions or
/// Builtins. This is so the interpreter can know whether it should fall back to
/// the builtins when invoking a function. Builtin doesn't need a name because
/// it's already in the Package::functions data.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Callable<CallType, StructType> {
  Function(Function<CallType, StructType>),
  Builtin,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Function<CallType, StructType> {
  pub num_params: u16,
  pub num_locals: u16,
  pub instructions: Vec<Instruction<CallType, StructType>>,
}

/// Instructions are parameterized by the representation of function calls.
/// During compilation, functions are referenced by name, but they are later
/// "linked" and turned into direct offsets into the function table.
///
/// TODO: This Instruction type is BIG. I'm guessing that reducing its size down
/// to, say, 64 bits would lead to some wins?
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Instruction<CallType, StructType> {
  /// loads local variable onto the stack
  LoadLocal(u16),
  /// assigns top of the stack to local variable.
  SetLocal(u16),
  PushInt(i64),
  PushFloat(f64),
  PushString(String),
  PushBool(bool),
  PushVoid,
  /// Pop all declared fields and allocate a heap-backed struct instance.
  NewStruct(StructType),
  /// Pop a struct value and push the field at this declaration-order offset.
  GetField(u16),
  /// discards topmost stack item
  Pop,
  /// Call the function at `(module, function)` in the function table.
  /// The `u16` is the number of arguments pushed at the call site, used by the
  /// interpreter to pop the right number of args (and to arity-check). This is
  /// essential for variadic builtins, whose `BuiltinSpec::num_params` is
  /// `None`.
  Call(CallType, u16),
  /// Call a FunctionRef or a Closure at TOS. The `u16` is the number of
  /// arguments pushed at the call site (see [`Instruction::Call`]).
  CallDynamic(u16),
  /// Exit the current function, returning the TOS to the caller
  Return,
  /// Push a reference to a function
  MakeFunctionRef(CallType),
  /// Partially apply some arguments to a function.
  /// argument: how many arguments to pop from the stack and bind to the function.
  /// TOS: a function reference
  PartialApply(u16),
  /// Jump to a relative offset.
  ///
  /// The offset is relative to the instruction *following* this one (the IP
  /// has already been advanced past the `Jump` when it executes), so `Jump(0)`
  /// is a no-op.
  Jump(u32),
  /// Pop the TOS; if it is false, add `offset` to the instruction pointer.
  /// See [`Instruction::Jump`] for the relative-offset semantics.
  JumpIfFalse(u32),
}

pub type LinkedFunction = Function<(u32, u32), (u32, u32)>;
type CompiledFunction = Function<(String, String), (String, String)>;

type LinkedInstruction = Instruction<(u32, u32), (u32, u32)>;
type CompiledInstruction = Instruction<(String, String), (String, String)>;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StructDef {
  pub name: String,
  pub fields: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Module<CallType, StructType> {
  pub name: String,
  pub functions: Vec<(String, Callable<CallType, StructType>)>,
  pub structs: Vec<StructDef>,
}

type CompiledModule = Module<(String, String), (String, String)>;
type CompiledModules = Vec<CompiledModule>;
pub type LinkedModule = Module<(u32, u32), (u32, u32)>;
pub type LinkedModules = Vec<LinkedModule>;

struct ModuleIndexEntry {
  module: u32,
  functions: HashMap<String, u32>,
  structs: HashMap<String, u32>,
}

type ModuleIndex = HashMap<String, ModuleIndexEntry>;

// This is what a Module should actually be, instead of a (u32, HashMap<String, u32>)
// struct Module {
//   name: String,
//   index: u32,
//   functions: HashMap<String, u32>
// }

impl Package {
  pub fn from_modules(modules: CompiledModules, specs: &[BuiltinSpec]) -> Result<Self, String> {
    let modules = inject_builtin_specs(modules, specs)?;
    let index = index_modules(modules.iter());
    Ok(Package {
      modules: link(&index, modules)?,
      main: None,
    })
  }

  pub fn from_modules_with_main(
    compiled_modules: CompiledModules,
    path: (&str, &str),
    specs: &[BuiltinSpec],
  ) -> Result<Self, String> {
    let compiled_modules = inject_builtin_specs(compiled_modules, specs)?;
    let index = index_modules(compiled_modules.iter());
    let linked_modules = link(&index, compiled_modules)?;
    let main = find_function(&index, path.0, path.1);
    if main.is_none() {
      Err(format!("Main function {path:?} was not found!"))
    } else {
      Ok(Package {
        modules: linked_modules,
        main,
      })
    }
  }

  pub fn get_module(&self, mod_index: u32) -> Option<&LinkedModule> {
    self.modules.get(mod_index as usize)
  }

  pub fn get_function(&self, module: u32, function: u32) -> Option<&LinkedCallable> {
    self
      .modules
      .get(module as usize)
      .and_then(|m| m.functions.get(function as usize))
      .map(|(_, f)| f)
  }

  pub fn get_struct(&self, module: u32, struct_: u32) -> Option<&StructDef> {
    self
      .modules
      .get(module as usize)
      .and_then(|m| m.structs.get(struct_ as usize))
  }
}

fn index_modules<'a>(modules: impl Iterator<Item = &'a CompiledModule>) -> ModuleIndex {
  let mut module_table = hashmap! {};
  for (mod_index, module) in modules.enumerate() {
    let mut entry = ModuleIndexEntry {
      module: mod_index as u32,
      functions: hashmap! {},
      structs: hashmap! {},
    };
    for (func_index, (func_name, _)) in module.functions.iter().enumerate() {
      entry
        .functions
        .insert(func_name.to_string(), func_index as u32);
    }
    for (struct_index, struct_) in module.structs.iter().enumerate() {
      entry
        .structs
        .insert(struct_.name.to_string(), struct_index as u32);
    }
    module_table.insert(module.name.to_string(), entry);
  }
  module_table
}

fn link(module_table: &ModuleIndex, modules: CompiledModules) -> Result<LinkedModules, String> {
  //! In a set of modules, replace all String-based references to functions and modules with
  //! index-based references.
  let mut result = vec![];
  for module in modules {
    // consuming
    let new_functions = module
      .functions
      .into_iter()
      .map(|(func_name, callable)| {
        let new_callable = match callable {
          Callable::Function(function) => {
            Callable::Function(link_instructions(module_table, function)?)
          }
          Callable::Builtin => Callable::Builtin,
        };
        Ok((func_name, new_callable))
      })
      .collect::<Result<Vec<_>, String>>()?;
    result.push(LinkedModule {
      name: module.name,
      functions: new_functions,
      structs: module.structs,
    });
  }
  Ok(result)
}

fn link_instructions(
  module_table: &ModuleIndex,
  function: CompiledFunction,
) -> Result<LinkedFunction, String> {
  //! In a function, replace all string-based references to functions and modules with index-based
  //! references.
  let instructions = function
    .instructions
    .into_iter()
    .map(|i| link_instruction(module_table, i))
    .collect::<Result<Vec<_>, _>>()?;
  Ok(LinkedFunction {
    num_params: function.num_params,
    num_locals: function.num_locals,
    instructions,
  })
}

fn link_instruction(
  module_table: &ModuleIndex,
  instruction: CompiledInstruction,
) -> Result<LinkedInstruction, String> {
  Ok(match instruction {
    Instruction::Call((mod_name, func_name), arity) => {
      let (mod_idx, func_idx) = find_function(module_table, &mod_name, &func_name)
        .ok_or_else(|| format!("Call to undefined function {}::{}", mod_name, func_name))?;
      Instruction::Call((mod_idx, func_idx), arity)
    }
    Instruction::MakeFunctionRef((mod_name, func_name)) => {
      let (mod_idx, func_idx) = find_function(module_table, &mod_name, &func_name)
        .ok_or_else(|| format!("Call to undefined function {}::{}", mod_name, func_name))?;
      Instruction::MakeFunctionRef((mod_idx, func_idx))
    }
    Instruction::NewStruct((mod_name, struct_name)) => {
      let (mod_idx, struct_idx) =
        find_struct(module_table, &mod_name, &struct_name).ok_or_else(|| {
          format!(
            "Construction of undefined struct {}.{}",
            mod_name, struct_name
          )
        })?;
      Instruction::NewStruct((mod_idx, struct_idx))
    }

    // Here's what I want to say:
    // x => Ok(x),
    // but Rust isn't smart enough to allow me. So I have to list out every variant of Instruction
    // :(
    Instruction::CallDynamic(arity) => Instruction::CallDynamic(arity),
    Instruction::LoadLocal(num) => Instruction::LoadLocal(num),
    Instruction::SetLocal(num) => Instruction::SetLocal(num),
    Instruction::PushInt(num) => Instruction::PushInt(num),
    Instruction::PushFloat(f) => Instruction::PushFloat(f),
    Instruction::PushString(string) => Instruction::PushString(string),
    Instruction::PushBool(b) => Instruction::PushBool(b),
    Instruction::PushVoid => Instruction::PushVoid,
    Instruction::GetField(field) => Instruction::GetField(field),
    Instruction::Pop => Instruction::Pop,
    Instruction::Return => Instruction::Return,
    Instruction::PartialApply(size) => Instruction::PartialApply(size),
    Instruction::Jump(target) => Instruction::Jump(target),
    Instruction::JumpIfFalse(target) => Instruction::JumpIfFalse(target),
  })
}

fn find_function(
  index: &ModuleIndex,
  module_name: &str,
  function_name: &str,
) -> Option<(u32, u32)> {
  index.get(module_name).and_then(|entry| {
    entry
      .functions
      .get(function_name)
      .map(|func_index| (entry.module, *func_index))
  })
}

fn find_struct(index: &ModuleIndex, module_name: &str, struct_name: &str) -> Option<(u32, u32)> {
  index.get(module_name).and_then(|entry| {
    entry
      .structs
      .get(struct_name)
      .map(|struct_index| (entry.module, *struct_index))
  })
}

pub fn compile_module(
  module_name: &str,
  asts: &[AST],
  prelude: &[(&str, &str)],
) -> Result<CompiledModule, String> {
  let asts = resolve_module_names(module_name, asts, prelude, &[])?;
  compile_resolved_module(module_name, &asts)
}

fn compile_resolved_module(module_name: &str, asts: &[AST]) -> Result<CompiledModule, String> {
  let asts = transform_closures_in_module(module_name, asts)?;
  let structs = asts
    .iter()
    .filter_map(|ast| match &ast.kind {
      ASTKind::DefineStruct(struct_) => Some((struct_.name.clone(), struct_.clone())),
      _ => None,
    })
    .collect::<HashMap<_, _>>();
  let function_returns = asts
    .iter()
    .filter_map(|ast| match &ast.kind {
      ASTKind::DefineFn(func) => Some((
        func.name.name.clone(),
        func
          .return_type
          .as_ref()
          .and_then(|ty| struct_name_from_type(ty, &structs)),
      )),
      _ => None,
    })
    .collect();
  let ctx = CompileContext {
    module_name,
    function_returns: &function_returns,
    structs: &structs,
  };
  let struct_defs = asts
    .iter()
    .filter_map(|ast| match &ast.kind {
      ASTKind::DefineStruct(struct_) => Some(StructDef {
        name: struct_.name.clone(),
        fields: struct_
          .fields
          .iter()
          .map(|(field, _)| field.clone())
          .collect(),
      }),
      _ => None,
    })
    .collect();
  let mut functions = vec![];
  for ast in &asts {
    match &ast.kind {
      ASTKind::DefineStruct(_) => {}
      ASTKind::DefineFn(func) => functions.extend(compile_function(&ctx, func)?),
      x => return Err(format!("Unexpected form at top-level: {:?}", x)),
    };
  }
  Ok(CompiledModule {
    name: module_name.to_string(),
    functions,
    structs: struct_defs,
  })
}

struct CompileContext<'a> {
  module_name: &'a str,
  function_returns: &'a HashMap<String, Option<String>>,
  structs: &'a HashMap<String, parser::Struct>,
}

#[derive(Clone)]
struct LocalInfo {
  index: u16,
  struct_type: Option<String>,
}

/// Compile a function.
/// Returns a vec of functions in case any of them contain nested functions.
fn compile_function(
  ctx: &CompileContext<'_>,
  f: &parser::Function,
) -> Result<Vec<(String, CompiledCallable)>, String> {
  // Map of local-name to local-index
  let mut locals = HashMap::new();
  for (idx, (param, annotation)) in f.params.iter().enumerate() {
    locals.insert(
      param.binding,
      LocalInfo {
        index: idx as u16,
        struct_type: annotation
          .as_ref()
          .and_then(|ty| struct_name_from_type(ty, ctx.structs)),
      },
    );
  }
  let mut instructions = vec![];
  let last_idx = f.code.len().saturating_sub(1);
  let returns_void = f.returns_void();
  for (i, ast) in f.code.iter().enumerate() {
    instructions.extend(compile_expr(ctx, ast, &mut locals)?);
    // Non-final body expressions are in statement position: their value is
    // discarded, so pop it off the stack to keep the stack clean. It would be
    // really nice to avoid pushing things to the stack entirely if we know they
    // are not going to be used, but that will probably take some more
    // thought/refactoring.
    if i != last_idx || returns_void {
      instructions.push(Instruction::Pop);
    }
  }
  if returns_void {
    instructions.push(Instruction::PushVoid);
  }
  instructions.push(Instruction::Return);
  Ok(vec![(
    f.name.name.clone(),
    Callable::Function(Function {
      num_params: f.params.len() as u16,
      num_locals: locals.len() as u16,
      instructions,
    }),
  )])
}

/// Compile `ast` into instructions.
fn compile_expr(
  ctx: &CompileContext<'_>,
  ast: &AST,
  locals: &mut HashMap<BindingId, LocalInfo>,
) -> Result<Vec<CompiledInstruction>, String> {
  let mut instructions = vec![];
  match &ast.kind {
    ASTKind::Call(callable_expr, arg_exprs) => {
      for expr in arg_exprs {
        instructions.extend(compile_expr(ctx, expr, locals)?);
      }
      instructions.extend(compile_expr(ctx, callable_expr, locals)?);
      instructions.push(Instruction::CallDynamic(arg_exprs.len() as u16))
    }
    ASTKind::CallFixed(identifier, arg_exprs) => {
      for expr in arg_exprs {
        instructions.extend(compile_expr(ctx, expr, locals)?);
      }
      let (module_name, function_name) = match identifier {
        Identifier::Qualified(mname, fname) => (mname.to_string(), fname.to_string()),
        Identifier::Bare(fname) => return Err(format!("call to unknown function: {fname}")),
      };
      instructions.push(Instruction::Call(
        (module_name, function_name),
        arg_exprs.len() as u16,
      ))
    }
    ASTKind::FunctionRef(mname, fname) => {
      instructions.push(Instruction::MakeFunctionRef((
        mname.to_owned(),
        fname.to_owned(),
      )));
    }
    ASTKind::Let(name, annotation, expr) => {
      if !locals.contains_key(&name.binding) {
        locals.insert(
          name.binding,
          LocalInfo {
            index: locals.len() as u16,
            struct_type: None,
          },
        );
      }
      instructions.extend(compile_expr(ctx, expr, locals)?);
      let struct_type = annotation
        .as_ref()
        .and_then(|ty| struct_name_from_type(ty, ctx.structs))
        .or_else(|| infer_struct_type(ctx, expr, locals));
      let local = locals
        .get_mut(&name.binding)
        .expect("local was inserted above");
      local.struct_type = struct_type;
      let local_index = local.index;
      instructions.push(Instruction::SetLocal(local_index));
      instructions.push(Instruction::LoadLocal(local_index));
    }
    ASTKind::PartialApply(expr, args) => {
      for expr in args {
        instructions.extend(compile_expr(ctx, expr, locals)?);
      }
      instructions.extend(compile_expr(ctx, expr, locals)?);
      instructions.push(Instruction::PartialApply(args.len() as u16));
    }
    ASTKind::NewStruct(name, fields) => {
      let struct_ = ctx
        .structs
        .get(name)
        .ok_or_else(|| format!("unknown struct `{name}`"))?;
      for (field_name, _) in &struct_.fields {
        let expr = fields
          .iter()
          .find(|(field, _)| field == field_name)
          .map(|(_, expr)| expr)
          .ok_or_else(|| format!("missing initializer for field `{field_name}` of `{name}`"))?;
        instructions.extend(compile_expr(ctx, expr, locals)?);
      }
      instructions.push(Instruction::NewStruct((
        ctx.module_name.to_string(),
        name.clone(),
      )));
    }
    ASTKind::FieldAccess(receiver, field) => {
      let receiver_type = infer_struct_type(ctx, receiver, locals)
        .ok_or_else(|| format!("field access receiver has no known struct type: {receiver:?}"))?;
      let (field_index, _) = field_for_struct(ctx, &receiver_type, field)?;
      instructions.extend(compile_expr(ctx, receiver, locals)?);
      instructions.push(Instruction::GetField(field_index));
    }
    ASTKind::If(cond, then, els) => {
      // <cond>; JumpIfFalse(+else); <then>; Jump(+end); <else>; L_end:
      //
      // We generate the code with placeholder jumps, remember their indices,
      // then patch them with the real relative offsets. Because the offsets are
      // relative to the instruction *following* each jump, they depend only on
      // the lengths of the `then` and `else` sub-vectors, not on where this
      // `if` sits in the enclosing function.
      instructions.extend(compile_expr(ctx, cond, locals)?);
      let jmp_else = instructions.len();
      instructions.push(Instruction::JumpIfFalse(0));
      instructions.extend(compile_expr(ctx, then, locals)?);
      let jmp_end = instructions.len();
      instructions.push(Instruction::Jump(0));
      // L_else:
      let else_start = instructions.len();
      instructions.extend(compile_expr(ctx, els, locals)?);
      // L_end:
      let end_start = instructions.len();
      // The JumpIfFalse at `jmp_else` must skip over the `then` branch and the
      // trailing `Jump`. When it executes, IP has already been advanced past
      // the JumpIfFalse itself (to jmp_else + 1), so the offset is
      // (else_start - (jmp_else + 1)).
      let else_offset = (else_start - jmp_else - 1) as u32;
      // The Jump at `jmp_end` must skip over the `else` branch. When it
      // executes, IP has already been advanced past the Jump itself
      // (to jmp_end + 1), so the offset is (end_start - (jmp_end + 1)).
      let end_offset = (end_start - jmp_end - 1) as u32;
      instructions[jmp_else] = Instruction::JumpIfFalse(else_offset);
      instructions[jmp_end] = Instruction::Jump(end_offset);
    }
    ASTKind::Block(body) => {
      // Evaluate each sub-expression; discard all but the last by popping,
      // and leave the last on the stack as the block's value.
      let last_idx = body.len().saturating_sub(1);
      for (i, expr) in body.iter().enumerate() {
        instructions.extend(compile_expr(ctx, expr, locals)?);
        if i != last_idx {
          instructions.push(Instruction::Pop);
        }
      }
    }
    ASTKind::Variable(name) => {
      if let Some(local) = locals.get(&name.binding) {
        instructions.push(Instruction::LoadLocal(local.index));
      } else {
        return Err(format!("Function accesses unbound variable {}", name));
      }
    }

    ASTKind::Int(i) => instructions.push(Instruction::PushInt(*i)),
    ASTKind::Float(f) => instructions.push(Instruction::PushFloat(*f)),
    ASTKind::String(s) => instructions.push(Instruction::PushString(s.clone())),
    ASTKind::Bool(b) => instructions.push(Instruction::PushBool(*b)),
    ASTKind::DefineStruct(_) => {
      return Err("Unexpected struct definition in expression".to_string())
    }
    x => return Err(format!("Unexpected form at top-level: {:?}", x)),
  }
  Ok(instructions)
}

fn struct_name_from_type(
  ty: &parser::TypeAst,
  structs: &HashMap<String, parser::Struct>,
) -> Option<String> {
  match ty {
    parser::TypeAst::Named(name) if structs.contains_key(name) => Some(name.clone()),
    _ => None,
  }
}

fn infer_struct_type(
  ctx: &CompileContext<'_>,
  ast: &AST,
  locals: &HashMap<BindingId, LocalInfo>,
) -> Option<String> {
  match &ast.kind {
    ASTKind::NewStruct(name, _) if ctx.structs.contains_key(name) => Some(name.clone()),
    ASTKind::Variable(name) => locals
      .get(&name.binding)
      .and_then(|local| local.struct_type.clone()),
    ASTKind::FieldAccess(receiver, field) => {
      let receiver = infer_struct_type(ctx, receiver, locals)?;
      field_struct_type_for_struct(ctx, &receiver, field)
    }
    ASTKind::Let(_, annotation, expr) => annotation
      .as_ref()
      .and_then(|ty| struct_name_from_type(ty, ctx.structs))
      .or_else(|| infer_struct_type(ctx, expr, locals)),
    ASTKind::CallFixed(Identifier::Bare(_), _) => None,
    ASTKind::CallFixed(Identifier::Qualified(module, name), _) if module == ctx.module_name => {
      ctx.function_returns.get(name).cloned().flatten()
    }
    ASTKind::Block(body) => body
      .last()
      .and_then(|expr| infer_struct_type(ctx, expr, locals)),
    ASTKind::If(_, then, els) => {
      let then_type = infer_struct_type(ctx, then, locals)?;
      let else_type = infer_struct_type(ctx, els, locals)?;
      (then_type == else_type).then_some(then_type)
    }
    _ => None,
  }
}

fn field_for_struct(
  ctx: &CompileContext<'_>,
  struct_name: &str,
  field: &str,
) -> Result<(u16, Option<String>), String> {
  let struct_ = ctx
    .structs
    .get(struct_name)
    .ok_or_else(|| format!("unknown struct `{struct_name}`"))?;
  let (index, ty) = struct_
    .fields
    .iter()
    .enumerate()
    .find(|(_, (name, _))| name == field)
    .map(|(index, (_, ty))| (index as u16, ty))
    .ok_or_else(|| format!("struct `{struct_name}` has no field `{field}`"))?;
  Ok((index, struct_name_from_type(ty, ctx.structs)))
}

fn field_struct_type_for_struct(
  ctx: &CompileContext<'_>,
  struct_name: &str,
  field: &str,
) -> Option<String> {
  field_for_struct(ctx, struct_name, field)
    .ok()
    .and_then(|(_, struct_name)| struct_name)
}

pub fn compile_executable_from_source(
  module_source: &str,
  main: (&str, &str),
  specs: &[BuiltinSpec],
  prelude: &[(&str, &str)],
) -> Result<Package, String> {
  Package::from_modules_with_main(
    _compile_from_source(module_source, specs, prelude)?,
    main,
    specs,
  )
}

pub fn compile_from_source(
  module_source: &str,
  specs: &[BuiltinSpec],
  prelude: &[(&str, &str)],
) -> Result<Package, String> {
  Package::from_modules(_compile_from_source(module_source, specs, prelude)?, specs)
}

fn _compile_from_source(
  module_source: &str,
  specs: &[BuiltinSpec],
  prelude: &[(&str, &str)],
) -> Result<CompiledModules, String> {
  let asts = parser::read_multiple(module_source)?;
  let module_symbols = specs
    .iter()
    .filter(|spec| spec.module == "main")
    .map(|spec| spec.name)
    .collect::<Vec<_>>();
  let asts = resolve_module_names("main", &asts, prelude, &module_symbols)?;
  crate::typecheck::typecheck(&asts, specs).map_err(|error| error.render(module_source))?;
  let compiled_modules = compile_modules(&asts)?;
  Ok(compiled_modules)
}

fn compile_modules(asts: &[AST]) -> Result<CompiledModules, String> {
  println!("Compiling main module");
  let compiled_module = compile_resolved_module("main", asts)?;
  Ok(vec![compiled_module])
}

/// Inject a `Callable::Builtin` entry for each `BuiltinSpec` into the named
/// module's function table, creating the module if it doesn't already exist
/// (e.g. the `std` module, which has no source file). Must run before
/// [`index_modules`] so the slots are resolvable by `Call` instructions.
fn inject_builtin_specs(
  mut modules: CompiledModules,
  specs: &[BuiltinSpec],
) -> Result<CompiledModules, String> {
  for spec in specs {
    let module = modules
      .iter_mut()
      .find(|module| module.name == spec.module)
      .map(|module| &mut module.functions);
    let module = match module {
      Some(funcs) => funcs,
      None => {
        modules.push(CompiledModule {
          name: spec.module.to_string(),
          functions: vec![],
          structs: vec![],
        });
        &mut modules.last_mut().unwrap().functions
      }
    };
    if module.iter().any(|(name, _)| name == spec.name) {
      return Err(format!(
        "Builtin {}.{} collides with an existing function",
        spec.module, spec.name
      ));
    }
    module.push((spec.name.to_string(), Callable::Builtin));
  }
  Ok(modules)
}

#[cfg(test)]
mod compiler_tests;
