use std::collections::{HashMap, HashSet};

use crate::builtins::BuiltinSpec;
use crate::closure::transform_closures_in_module;
use crate::parser::{self, ASTKind, Identifier, AST};

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

pub type LinkedCallable = Callable<(u32, u32)>;
type CompiledCallable = Callable<(String, String)>;

/// Packages contain Callables, which can either be LinkedFunctions or
/// Builtins. This is so the interpreter can know whether it should fall back to
/// the builtins when invoking a function. Builtin doesn't need a name because
/// it's already in the Package::functions data.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Callable<CallType> {
  Function(Function<CallType>),
  Builtin,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Function<CallType> {
  pub num_params: u16,
  pub num_locals: u16,
  pub instructions: Vec<Instruction<CallType>>,
}

/// Instructions are parameterized by the representation of function calls.
/// During compilation, functions are referenced by name, but they are later
/// "linked" and turned into direct offsets into the function table.
///
/// TODO: This Instruction type is BIG. I'm guessing that reducing its size down
/// to, say, 64 bits would lead to some wins?
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Instruction<CallType> {
  /// loads local variable onto the stack
  LoadLocal(u16),
  /// assigns top of the stack to local variable.
  SetLocal(u16),
  PushInt(i64),
  PushFloat(f64),
  PushString(String),
  PushBool(bool),
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
  /// Wrap the TOS in a Cell, which is pushed.
  MakeCell,
  /// Extract the underlying SLVal from the Cell that's on TOS.
  DerefCell,
  /// Set the contents of a Cell.
  SetCell,
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

pub type LinkedFunction = Function<(u32, u32)>;
type CompiledFunction = Function<(String, String)>;

type LinkedInstruction = Instruction<(u32, u32)>;
type CompiledInstruction = Instruction<(String, String)>;

type CompiledModule = Vec<(String, CompiledCallable)>;
type CompiledModules = Vec<(String, CompiledModule)>;
pub type LinkedModule = Vec<(String, LinkedCallable)>;
pub type LinkedModules = Vec<(String, LinkedModule)>;

type ModuleIndex = HashMap<String, (u32, HashMap<String, u32>)>;

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

  pub fn get_module(&self, mod_index: u32) -> Option<&(String, LinkedModule)> {
    self.modules.get(mod_index as usize)
  }

  pub fn get_function(&self, module: u32, function: u32) -> Option<&LinkedCallable> {
    self
      .modules
      .get(module as usize)
      .and_then(|(_, m)| m.get(function as usize))
      .map(|(_, f)| f)
  }
}

fn index_modules<'a>(modules: impl Iterator<Item = &'a (String, CompiledModule)>) -> ModuleIndex {
  let mut module_table = hashmap! {};
  for (mod_index, (mod_name, functions)) in modules.enumerate() {
    module_table.insert(mod_name.to_string(), (mod_index as u32, hashmap! {}));
    for (func_index, (func_name, _)) in functions.iter().enumerate() {
      module_table
        .get_mut(mod_name)
        .unwrap()
        .1
        .insert(func_name.to_string(), func_index as u32);
    }
  }
  module_table
}

fn link(module_table: &ModuleIndex, modules: CompiledModules) -> Result<LinkedModules, String> {
  //! In a set of modules, replace all String-based references to functions and modules with
  //! index-based references.
  let mut result = vec![];
  for (mod_name, functions) in modules {
    // consuming
    let new_functions = functions
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
      .collect::<Result<LinkedModule, String>>()?;
    result.push((mod_name, new_functions));
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
        .ok_or_else(|| format!("Call to undefined function {}.{}", mod_name, func_name))?;
      Instruction::Call((mod_idx, func_idx), arity)
    }
    Instruction::MakeFunctionRef((mod_name, func_name)) => {
      let (mod_idx, func_idx) = find_function(module_table, &mod_name, &func_name)
        .ok_or_else(|| format!("Call to undefined function {}.{}", mod_name, func_name))?;
      Instruction::MakeFunctionRef((mod_idx, func_idx))
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
    Instruction::Pop => Instruction::Pop,
    Instruction::Return => Instruction::Return,
    Instruction::MakeCell => Instruction::MakeCell,
    Instruction::DerefCell => Instruction::DerefCell,
    Instruction::SetCell => Instruction::SetCell,
    Instruction::PartialApply(size) => Instruction::PartialApply(size),
    Instruction::Jump(target) => Instruction::Jump(target),
    Instruction::JumpIfFalse(target) => Instruction::JumpIfFalse(target),
  })
}

pub fn find_function(
  index: &ModuleIndex,
  module_name: &str,
  function_name: &str,
) -> Option<(u32, u32)> {
  index.get(module_name).and_then(|(mod_index, functions)| {
    functions
      .get(function_name)
      .map(|func_index| (*mod_index, *func_index))
  })
}

pub fn compile_module(module_name: &str, asts: &[AST]) -> Result<CompiledModule, String> {
  let asts = transform_closures_in_module(module_name, asts)?;
  let module_functions: HashSet<String> = asts
    .iter()
    .filter_map(|ast| match &ast.kind {
      ASTKind::DefineFn(func) => Some(func.name.clone()),
      _ => None,
    })
    .collect();
  let mut functions = vec![];
  for ast in &asts {
    match &ast.kind {
      ASTKind::DefineFn(func) => {
        functions.extend(compile_function(module_name, &module_functions, func)?)
      }
      x => return Err(format!("Unexpected form at top-level: {:?}", x)),
    };
  }
  Ok(functions)
}

/// Compile a function.
/// Returns a vec of functions in case any of them contain nested functions.
fn compile_function(
  module_name: &str,
  module_functions: &HashSet<String>,
  f: &parser::Function,
) -> Result<CompiledModule, String> {
  // Map of local-name to local-index
  let mut locals = HashMap::new();
  for (idx, (param, _)) in f.params.iter().enumerate() {
    locals.insert(param.clone(), idx as u16);
  }
  let mut instructions = vec![];
  let last_idx = f.code.len().saturating_sub(1);
  for (i, ast) in f.code.iter().enumerate() {
    instructions.extend(compile_expr(
      module_name,
      module_functions,
      ast,
      &mut locals,
    )?);
    // Non-final body expressions are in statement position: their value is
    // discarded, so pop it off the stack to keep the stack clean. It would be
    // really nice to avoid pushing things to the stack entirely if we know they
    // are not going to be used, but that will probably take some more
    // thought/refactoring.
    if i != last_idx {
      instructions.push(Instruction::Pop);
    }
  }
  instructions.push(Instruction::Return);
  Ok(vec![(
    f.name.to_string(),
    Callable::Function(Function {
      num_params: f.params.len() as u16,
      num_locals: locals.len() as u16,
      instructions,
    }),
  )])
}

/// Compile `ast` into instructions.
fn compile_expr(
  module_name: &str,
  module_functions: &HashSet<String>,
  ast: &AST,
  locals: &mut HashMap<String, u16>,
) -> Result<Vec<CompiledInstruction>, String> {
  let mut instructions = vec![];
  match &ast.kind {
    ASTKind::Call(callable_expr, arg_exprs) => {
      for expr in arg_exprs {
        instructions.extend(compile_expr(module_name, module_functions, expr, locals)?);
      }
      instructions.extend(compile_expr(
        module_name,
        module_functions,
        callable_expr,
        locals,
      )?);
      instructions.push(Instruction::CallDynamic(arg_exprs.len() as u16))
    }
    ASTKind::CallFixed(identifier, arg_exprs) => {
      for expr in arg_exprs {
        instructions.extend(compile_expr(module_name, module_functions, expr, locals)?);
      }
      if let Identifier::Bare(fname) = identifier {
        if let Some(local_index) = locals.get(fname) {
          instructions.push(Instruction::LoadLocal(*local_index));
          instructions.push(Instruction::CallDynamic(arg_exprs.len() as u16));
          return Ok(instructions);
        }
      }
      let (module_name, function_name) = match identifier {
        Identifier::Bare(fname) => (module_name.to_string(), fname.to_string()),
        Identifier::Qualified(mname, fname) => (mname.to_string(), fname.to_string()),
      };
      instructions.push(Instruction::Call(
        (module_name, function_name),
        arg_exprs.len() as u16,
      ))
    }
    ASTKind::Cell(expr) => {
      instructions.extend(compile_expr(module_name, module_functions, expr, locals)?);
      instructions.push(Instruction::MakeCell);
    }
    ASTKind::DerefCell(expr) => {
      instructions.extend(compile_expr(module_name, module_functions, expr, locals)?);
      instructions.push(Instruction::DerefCell);
    }
    ASTKind::SetCell(target, value) => {
      // Evaluate the value, then the target (which must resolve to a Cell).
      // Stack order at SetCell time: [value, cell] (cell on TOS).
      instructions.extend(compile_expr(module_name, module_functions, value, locals)?);
      instructions.extend(compile_expr(module_name, module_functions, target, locals)?);
      instructions.push(Instruction::SetCell);
    }
    ASTKind::FunctionRef(mname, fname) => {
      instructions.push(Instruction::MakeFunctionRef((
        mname.to_owned(),
        fname.to_owned(),
      )));
    }
    ASTKind::Let(name, _, expr) => {
      if !locals.contains_key(name) {
        locals.insert(name.clone(), locals.len() as u16);
      }
      instructions.extend(compile_expr(module_name, module_functions, expr, locals)?);
      let local_index = locals[name];
      instructions.push(Instruction::SetLocal(local_index));
      instructions.push(Instruction::LoadLocal(local_index));
    }
    ASTKind::PartialApply(expr, args) => {
      for expr in args {
        instructions.extend(compile_expr(module_name, module_functions, expr, locals)?);
      }
      instructions.extend(compile_expr(module_name, module_functions, expr, locals)?);
      instructions.push(Instruction::PartialApply(args.len() as u16));
    }
    ASTKind::If(cond, then, els) => {
      // <cond>; JumpIfFalse(+else); <then>; Jump(+end); <else>; L_end:
      //
      // We generate the code with placeholder jumps, remember their indices,
      // then patch them with the real relative offsets. Because the offsets are
      // relative to the instruction *following* each jump, they depend only on
      // the lengths of the `then` and `else` sub-vectors, not on where this
      // `if` sits in the enclosing function.
      instructions.extend(compile_expr(module_name, module_functions, cond, locals)?);
      let jmp_else = instructions.len();
      instructions.push(Instruction::JumpIfFalse(0));
      instructions.extend(compile_expr(module_name, module_functions, then, locals)?);
      let jmp_end = instructions.len();
      instructions.push(Instruction::Jump(0));
      // L_else:
      let else_start = instructions.len();
      instructions.extend(compile_expr(module_name, module_functions, els, locals)?);
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
        instructions.extend(compile_expr(module_name, module_functions, expr, locals)?);
        if i != last_idx {
          instructions.push(Instruction::Pop);
        }
      }
    }
    ASTKind::Variable(name) => {
      if let Some(local) = locals.get(name) {
        instructions.push(Instruction::LoadLocal(*local));
      } else if module_functions.contains(name) {
        instructions.push(Instruction::MakeFunctionRef((
          module_name.to_string(),
          name.clone(),
        )));
      } else {
        return Err(format!("Function accesses unbound variable {}", name));
      }
    }

    ASTKind::Int(i) => instructions.push(Instruction::PushInt(*i)),
    ASTKind::Float(f) => instructions.push(Instruction::PushFloat(*f)),
    ASTKind::String(s) => instructions.push(Instruction::PushString(s.clone())),
    ASTKind::Bool(b) => instructions.push(Instruction::PushBool(*b)),
    x => return Err(format!("Unexpected form at top-level: {:?}", x)),
  }
  Ok(instructions)
}

pub fn compile_executable_from_source(
  module_source: &str,
  main: (&str, &str),
  specs: &[BuiltinSpec],
) -> Result<Package, String> {
  Package::from_modules_with_main(_compile_from_source(module_source, specs)?, main, specs)
}

pub fn compile_from_source(module_source: &str, specs: &[BuiltinSpec]) -> Result<Package, String> {
  Package::from_modules(_compile_from_source(module_source, specs)?, specs)
}

fn _compile_from_source(
  module_source: &str,
  specs: &[BuiltinSpec],
) -> Result<CompiledModules, String> {
  let asts = parser::read_multiple(module_source)?;
  crate::typecheck::typecheck(&asts, specs).map_err(|error| error.to_string())?;
  let compiled_modules = compile_modules(&asts)?;
  Ok(compiled_modules)
}

fn compile_modules(asts: &[AST]) -> Result<CompiledModules, String> {
  println!("Compiling main module");
  let compiled_module = compile_module("main", asts)?;
  Ok(vec![("main".to_string(), compiled_module)])
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
      .find(|(name, _)| name == spec.module)
      .map(|(_, funcs)| funcs);
    let module = match module {
      Some(funcs) => funcs,
      None => {
        modules.push((spec.module.to_string(), vec![]));
        &mut modules.last_mut().unwrap().1
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
mod test {
  use super::*;

  #[test]
  fn compile_id() {
    let func = parser::Function {
      name: "id".to_string(),
      params: vec![("a".to_string(), None)],
      return_type: None,
      bounds: vec![],
      code: vec![AST::Variable("a".to_string())],
    };
    let code = compile_function("main", &HashSet::new(), &func).unwrap();
    assert_eq!(
      code,
      vec![(
        "id".to_string(),
        Callable::Function(Function {
          num_params: 1,
          num_locals: 1,
          instructions: vec![Instruction::LoadLocal(0), Instruction::Return],
        }),
      )]
    );
  }

  #[test]
  fn compile_call_to_local_function_ref() {
    let func = parser::Function {
      name: "main".to_string(),
      params: vec![],
      return_type: None,
      bounds: vec![],
      code: vec![
        AST::Let(
          "alias".to_string(),
          Box::new(AST::FunctionRef("main".to_string(), "x".to_string())),
        ),
        AST::CallFixed(Identifier::Bare("alias".to_string()), vec![]),
      ],
    };
    let code = compile_function("main", &HashSet::new(), &func).unwrap();
    assert_eq!(
      code,
      vec![(
        "main".to_string(),
        Callable::Function(Function {
          num_params: 0,
          num_locals: 1,
          instructions: vec![
            Instruction::MakeFunctionRef(("main".to_string(), "x".to_string())),
            Instruction::SetLocal(0),
            Instruction::LoadLocal(0),
            Instruction::Pop,
            Instruction::LoadLocal(0),
            Instruction::CallDynamic(0),
            Instruction::Return,
          ],
        }),
      )]
    );
  }

  #[test]
  fn compile_let_returns_bound_value() {
    let func = parser::Function {
      name: "main".to_string(),
      params: vec![],
      return_type: None,
      bounds: vec![],
      code: vec![AST::Let("a".to_string(), Box::new(AST::Int(1)))],
    };
    let code = compile_function("main", &HashSet::new(), &func).unwrap();
    assert_eq!(
      code,
      vec![(
        "main".to_string(),
        Callable::Function(Function {
          num_params: 0,
          num_locals: 1,
          instructions: vec![
            Instruction::PushInt(1),
            Instruction::SetLocal(0),
            Instruction::LoadLocal(0),
            Instruction::Return,
          ],
        }),
      )]
    );
  }

  #[test]
  fn compile_unbound_variable_still_errors() {
    let func = parser::Function {
      name: "main".to_string(),
      params: vec![],
      return_type: None,
      bounds: vec![],
      code: vec![AST::Variable("missing".to_string())],
    };
    let err = compile_function("main", &HashSet::new(), &func).unwrap_err();
    assert_eq!(err, "Function accesses unbound variable missing");
  }

  #[test]
  fn source_compilation_rejects_type_errors_before_codegen() {
    let builtins = crate::builtins::default_builtins();
    let error = compile_executable_from_source(
      "(fn main () ->Int (std.+ 1 \"not-an-int\"))",
      ("main", "main"),
      &builtins.specs(),
    )
    .unwrap_err();
    assert!(
      error.contains("type `String` does not satisfy trait `Add`")
        || error.contains("expected `Int`, got `String`"),
      "{error}"
    );
  }
}
