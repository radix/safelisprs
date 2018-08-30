use std::collections::HashMap;

use parser::{self, AST};

/// A Package can either represent a "program" or a "library".
/// If a `main` is provided, then it can be executed as a program directly.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Package {
  /// While the names here *can* be used for function lookup, they are only
  /// included for debugging purposes. In a complete "executable" package, all
  /// calls will be represented with index-based function offsets.
  pub functions: Vec<(String, Vec<(String, CompiledCallable)>)>,
  pub main: Option<(usize, usize)>,
}

pub type CompiledCallable = Callable<(usize, usize)>;
type CompilingCallable = Callable<(String, String)>;

/// Packages contain Callables, which can either be CompiledFunctions or
/// Builtins. This is so the interpreter can know whether it should fall back to
/// the builtins when invoking a function. Builtin doesn't need a name because
/// it's already in the Package::functions data.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Callable<CallType> {
  Function(Function<CallType>),
  Builtin,
}

pub type CompiledFunction = Function<(usize, usize)>;
type CompilingFunction = Function<(String, String)>;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Function<CallType> {
  pub num_params: u16,
  pub num_locals: u16,
  pub instructions: Vec<Instruction<CallType>>,
}

type CompiledInstruction = Instruction<(usize, usize)>;
type CompilingInstruction = Instruction<(String, String)>;

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
  /// Exit the current function, returning the TOS to the caller
  Return,
}

type CompilingModules = Vec<(String, Vec<(String, CompilingCallable)>)>;
pub type CompiledModules = Vec<(String, Vec<(String, CompiledCallable)>)>;

type ModuleIndex = HashMap<String, (usize, HashMap<String, usize>)>;

impl Package {
  pub fn from_modules(modules: CompilingModules) -> Result<Self, String> {
    let index = index_modules(&modules);
    Ok(Package {
      functions: link(&index, modules)?,
      main: None,
    })
  }

  pub fn from_modules_with_main(
    modules: CompilingModules,
    main: (String, String),
  ) -> Result<Self, String> {
    let index = index_modules(&modules);
    let functions = link(&index, modules)?;
    let main = find_function(&index, &main.0, &main.1);
    if main.is_none() {
      Err(format!("Main function {:?} was not found!", main))
    } else {
      Ok(Package { functions, main })
    }
  }

  pub fn get_module(&self, mod_index: usize) -> Option<&(String, Vec<(String, CompiledCallable)>)> {
    self.functions.get(mod_index)
  }

  pub fn get_function(&self, module: usize, function: usize) -> Option<&CompiledCallable> {
    self
      .functions
      .get(module)
      .and_then(|(_, m)| m.get(function))
      .map(|(_, f)| f)
  }
}

fn index_modules(modules: &CompilingModules) -> ModuleIndex {
  let mut module_table = hashmap!{};
  for (mod_index, (mod_name, functions)) in modules.iter().enumerate() {
    module_table.insert(mod_name.to_string(), (mod_index, hashmap!{}));
    for (func_index, (func_name, _)) in functions.iter().enumerate() {
      module_table
        .get_mut(mod_name)
        .unwrap()
        .1
        .insert(func_name.to_string(), func_index);
    }
  }
  module_table
}

fn link(module_table: &ModuleIndex, modules: CompilingModules) -> Result<CompiledModules, String> {
  let mut result = vec![];
  for (mod_name, functions) in modules {
    // consuming
    let new_functions = functions.into_iter().map(|(func_name, callable)| {
      let new_callable = match callable {
        Callable::Function(function) => Callable::Function(link_instructions(module_table, function)?),
        Callable::Builtin => Callable::Builtin,
      };
      Ok((func_name, new_callable))
    }).collect::<Result<Vec<(String, CompiledCallable)>, String>>()?;
    result.push((mod_name, new_functions));
  }
  Ok(result)
}

fn link_instructions(
  module_table: &ModuleIndex,
  function: CompilingFunction,
) -> Result<CompiledFunction, String> {
  let instructions = function
    .instructions
    .into_iter()
    .map(|i| link_instruction(module_table, i))
    .collect::<Result<Vec<_>, _>> ()?;
  Ok(CompiledFunction {
    num_params: function.num_params,
    num_locals: function.num_locals,
    instructions,
  })
}

fn link_instruction(
  module_table: &ModuleIndex,
  instruction: CompilingInstruction,
) -> Result<CompiledInstruction, String> {
  Ok(match instruction {
    Instruction::Call((mod_name, func_name)) => {
      let (mod_idx, func_idx) = find_function(module_table, &mod_name, &func_name)
        .ok_or_else(|| format!("Call to undefined function {}.{}", mod_name, func_name))?;
      Instruction::Call((mod_idx, func_idx))
    }
    // Here's what I want to say:
    // x => Ok(x),
    // but Rust isn't smart enough to allow me. So I have to list out every variant of Instruction
    // :(
    Instruction::LoadLocal(num) => Instruction::LoadLocal(num),
    Instruction::SetLocal(num) => Instruction::SetLocal(num),
    Instruction::PushInt(num) => Instruction::PushInt(num),
    Instruction::PushFloat(f) => Instruction::PushFloat(f),
    Instruction::PushString(string) => Instruction::PushString(string),
    Instruction::Pop => Instruction::Pop,
    Instruction::Return => Instruction::Return,
  })
}

pub fn find_function(
  index: &ModuleIndex,
  module_name: &str,
  function_name: &str,
) -> Option<(usize, usize)> {
  index.get(module_name).and_then(|(mod_index, m)| {
    m.get(function_name)
      .map(|func_index| (*mod_index, *func_index))
  })
}

pub fn compile_module(
  name: &str,
  asts: &[AST],
) -> Result<Vec<(String, CompilingCallable)>, String> {
  let mut functions = vec![];
  for ast in asts {
    match ast {
      AST::DefineFn(func) => functions.push((
        func.name.clone(),
        Callable::Function(compile_function(name, func)?),
      )),
      AST::DeclareFn(decl) => functions.push((decl.name.clone(), Callable::Builtin)),
      x => return Err(format!("Unexpected form at top-level: {:?}", x)),
    };
  }
  Ok(functions)
}

fn compile_function(module_name: &str, f: &parser::Function) -> Result<CompilingFunction, String> {
  let mut num_locals = f.params.len() as u16;
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
      &mut num_locals,
      &mut locals,
    )?);
  }
  instructions.push(Instruction::Return);
  Ok(Function {
    num_params: f.params.len() as u16,
    num_locals: num_locals as u16,
    instructions: instructions,
  })
}

fn compile_expr(
  module_name: &str,
  ast: &AST,
  num_locals: &mut u16,
  locals: &mut HashMap<String, u16>,
) -> Result<Vec<CompilingInstruction>, String> {
  let mut instructions = vec![];
  match ast {
    AST::Let(name, box_expr) => {
      if !locals.contains_key(name) {
        locals.insert(name.clone(), *num_locals);
        *num_locals += 1;
      }
      instructions.extend(compile_expr(module_name, &box_expr, num_locals, locals)?);
      instructions.push(Instruction::SetLocal(locals[name]))
    }
    AST::DefineFn(func) => return Err(format!("NYI: Can't define inner functions: {}", func.name)),
    AST::DeclareFn(decl) => {
      return Err(format!(
        "Cannot declare functions inside other forms: {}",
        decl.name
      ))
    }
    AST::Call(box_expr, arg_exprs) => {
      for expr in arg_exprs {
        instructions.extend(compile_expr(module_name, &expr, num_locals, locals)?);
      }
      match &**box_expr {
        AST::Variable(name) => instructions.push(Instruction::Call((
          // FIXME TODO XXX: this is wrong, module_name is the *current*
          // module's name, not the name of the module containing this function!
          module_name.to_string(),
          name.to_string(),
        ))),
        x => return Err(format!("NYI: non-constant functions: {:?}", x)),
      }
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
  }
  Ok(instructions)
}


pub fn compile_from_sources(module_sources: &[(String, String)]) -> Result<Package, String> {
  use parser;
  let mut compiling_modules: CompilingModules = vec![];
  for (mod_name, mod_data) in module_sources {
    let asts = parser::read_multiple(&mod_data)?;
    let compiling_module = compile_module(&mod_name, &asts)?;
    compiling_modules.push((mod_name.to_string(), compiling_module));
  }
  Ok(Package::from_modules(compiling_modules)?)
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
    let code = compile_function(&func).unwrap();
    assert_eq!(
      code,
      Function {
        num_params: 1,
        num_locals: 1,
        instructions: vec![Instruction::LoadLocal(0), Instruction::Return],
      }
    );
  }
}
