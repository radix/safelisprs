use std::collections::{HashMap, VecDeque};

use crate::closure::transform_closures_in_module;
use crate::parser::{self, Identifier, AST};

/// A Package can either represent a "program" or a "library".
/// If a `main` is provided, then it can be executed as a program directly.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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
  /// discards topmost stack item
  Pop,
  Call(CallType),
  /// Call a FunctionRef or a Closure at TOS.
  CallDynamic,
  /// Exit the current function, returning the TOS to the caller
  Return,
  /// Wrap the TOS in a Cell, which is pushed.
  MakeCell,
  /// Extract the underlying SLVal from the Cell that's on TOS.
  DerefCell,
  /// Push a reference to a function
  MakeFunctionRef(CallType),
  /// Partially apply some arguments to a function.
  /// argument: how many arguments to pop from the stack and bind to the function.
  /// TOS: a function reference
  PartialApply(u16),
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
  pub fn from_modules(modules: CompiledModules) -> Result<Self, String> {
    let index = index_modules(modules.iter());
    Ok(Package {
      modules: link(&index, modules)?,
      main: None,
    })
  }

  pub fn from_modules_with_main(
    compiled_modules: CompiledModules,
    main: (&str, &str),
  ) -> Result<Self, String> {
    let index = index_modules(compiled_modules.iter());
    let linked_modules = link(&index, compiled_modules)?;
    let main = find_function(&index, main.0, main.1);
    if main.is_none() {
      Err(format!("Main function {:?} was not found!", main))
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
    Instruction::Call((mod_name, func_name)) => {
      let (mod_idx, func_idx) = find_function(module_table, &mod_name, &func_name)
        .ok_or_else(|| format!("Call to undefined function {}.{}", mod_name, func_name))?;
      Instruction::Call((mod_idx, func_idx))
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
    Instruction::CallDynamic => Instruction::CallDynamic,
    Instruction::LoadLocal(num) => Instruction::LoadLocal(num),
    Instruction::SetLocal(num) => Instruction::SetLocal(num),
    Instruction::PushInt(num) => Instruction::PushInt(num),
    Instruction::PushFloat(f) => Instruction::PushFloat(f),
    Instruction::PushString(string) => Instruction::PushString(string),
    Instruction::Pop => Instruction::Pop,
    Instruction::Return => Instruction::Return,
    Instruction::MakeCell => Instruction::MakeCell,
    Instruction::DerefCell => Instruction::DerefCell,
    Instruction::PartialApply(size) => Instruction::PartialApply(size),
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

pub fn compile_module(asts: &[AST]) -> Result<(CompiledModule, Vec<String>), String> {
  let asts = transform_closures_in_module(asts)?;
  let mut functions = vec![];
  let mut imports = vec![];
  for ast in &asts {
    match ast {
      AST::Import(filename) => {
        imports.push(filename.to_owned());
      }
      AST::DefineFn(func) => functions.extend(compile_function("main", func)?),
      AST::DeclareFn(decl) => functions.push((decl.name.clone(), Callable::Builtin)),
      x => return Err(format!("Unexpected form at top-level: {:?}", x)),
    };
  }
  Ok((functions, imports))
}

/// Compile a function.
/// Returns a vec of functions in case any of them contain nested functions.
fn compile_function(
  module_name: &str,
  f: &parser::Function,
) -> Result<CompiledModule, String> {
  // Map of local-name to local-index
  let mut locals = HashMap::new();
  for (idx, param) in f.params.iter().enumerate() {
    locals.insert(param.clone(), idx as u16);
  }
  let mut instructions = vec![];
  for ast in &f.code {
    instructions.extend(compile_expr(
      module_name,
      ast,
      &mut locals,
    )?);
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

fn compile_expr(
  module_name: &str,
  ast: &AST,
  locals: &mut HashMap<String, u16>,
) -> Result<Vec<CompiledInstruction>, String> {
  let mut instructions = vec![];
  match ast {
    AST::Call(callable_expr, _arg_exprs) => {
      return Err(format!(
        "NYI: non-constant functions: {callable_expr:?} -- {_arg_exprs:?}"
      ))
    }
    AST::CallFixed(identifier, arg_exprs) => {
      for expr in arg_exprs {
        instructions.extend(compile_expr(
          module_name,
          expr,
          locals,
        )?);
      }
      let (module_name, function_name) = match identifier {
        Identifier::Bare(fname) => (module_name.to_string(), fname.to_string()),
        Identifier::Qualified(mname, fname) => (mname.to_string(), fname.to_string()),
      };
      instructions.push(Instruction::Call((module_name, function_name)))
    }
    AST::Cell(expr) => {
      instructions.extend(compile_expr(
        module_name,
        expr,
        locals,
      )?);
      instructions.push(Instruction::MakeCell);
    }
    AST::DeclareFn(decl) => {
      return Err(format!(
        "[BUG] Cannot declare functions inside other forms: {}",
        decl.name
      ))
    }
    AST::DefineFn(func) => return Err(format!("[BUG] DefineFn in an expression: {}", func.name)),
    AST::DerefCell(expr) => {
      instructions.extend(compile_expr(
        module_name,
        expr,
        locals,
      )?);
      instructions.push(Instruction::DerefCell);
    }
    AST::FunctionRef(mname, fname) => {
      instructions.push(Instruction::MakeFunctionRef((
        mname.to_owned(),
        fname.to_owned(),
      )));
    }
    AST::Let(name, expr) => {
      if !locals.contains_key(name) {
        locals.insert(name.clone(), locals.len() as u16);
      }
      instructions.extend(compile_expr(
        module_name,
        expr,
        locals,
      )?);
      instructions.push(Instruction::SetLocal(locals[name]))
    }
    AST::PartialApply(expr, args) => {
      for expr in args {
        instructions.extend(compile_expr(
          module_name,
          expr,
          locals,
        )?);
      }
      instructions.extend(compile_expr(
        module_name,
        expr,
        locals,
      )?);
      instructions.push(Instruction::PartialApply(args.len() as u16));
    }
    AST::Variable(name) => {
      if !locals.contains_key(name) {
        return Err(format!("Function accesses unbound variable {}", name));
      }
      instructions.push(Instruction::LoadLocal(locals[name]));
    }

    AST::Int(i) => instructions.push(Instruction::PushInt(*i)),
    AST::Float(f) => instructions.push(Instruction::PushFloat(*f)),
    AST::String(s) => instructions.push(Instruction::PushString(s.clone())),
    x => return Err(format!("Unexpected form at top-level: {:?}", x)),
  }
  Ok(instructions)
}

pub fn compile_executable_from_source(
  module_source: &str,
  main: (&str, &str),
) -> Result<Package, String> {
  Package::from_modules_with_main(_compile_from_source(module_source)?, main)
}

pub fn compile_from_source(module_source: &str) -> Result<Package, String> {
  Package::from_modules(_compile_from_source(module_source)?)
}

fn _compile_from_source(module_source: &str) -> Result<CompiledModules, String> {
  let asts = parser::read_multiple(module_source)?;
  let compiled_modules = compile_modules(&asts)?;
  Ok(compiled_modules)
}

fn compile_modules(asts: &[AST]) -> Result<CompiledModules, String> {
  // TODO: move file IO out of this function and into a trait implementation
  use std::fs::File;
  use std::io::prelude::*;
  use std::path::PathBuf;

  let mut compiled_modules = vec![];
  let mut imports: VecDeque<String> = VecDeque::new();
  println!("Compiling main module");
  let (compiled_module, initial_imports) = compile_module(asts)?;
  imports.extend(initial_imports);
  compiled_modules.push(("main".to_string(), compiled_module));
  while !imports.is_empty() {
    let import = imports.pop_front().unwrap();
    let mut import_filepath = PathBuf::from(import);
    import_filepath.set_extension("sl");
    let module_name = import_filepath
      .file_stem()
      .ok_or_else(|| format!("Couldn't figure out module name for {import_filepath:?}"))?
      .to_str()
      .ok_or_else(|| format!("Not UTF-8: {import_filepath:?}"))?;
    let source = {
      let mut buffer = String::new();
      let mut f = File::open(&import_filepath)
        .map_err(|e| format!("Error opening {import_filepath:?}: {e:?}"))?;
      f.read_to_string(&mut buffer).map_err(|e| e.to_string())?;
      buffer
    };
    let asts = parser::read_multiple(&source)?;
    println!("Compiling additional file {import_filepath:?} as module {module_name:?}");
    let (compiled_module, more_imports) = compile_module(&asts)?;
    compiled_modules.push((module_name.to_owned(), compiled_module));
    imports.extend(more_imports);
  }
  println!("Done compiling.");
  Ok(compiled_modules)
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn compile_id() {
    let func = parser::Function {
      name: "id".to_string(),
      params: vec!["a".to_string()],
      code: vec![AST::Variable("a".to_string())],
    };
    let code = compile_function("main", &func).unwrap();
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
}
