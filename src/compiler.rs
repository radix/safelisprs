use std::collections::HashMap;

use parser::{self, AST};

/// A Package can either represent a "program" or a "library".
/// If a `main` is provided, then it can be executed as a program directly.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Package {
  pub functions: Vec<(String, Vec<(String, Function)>)>,
  pub main: Option<(usize, usize)>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Function {
  pub num_params: u16,
  pub num_locals: u16,
  pub instructions: Vec<Instruction>,
}

type CompiledInstruction = PrivInstruction<(usize, usize)>;
type CompilingInstruction = PrivInstruction<(String, String)>

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum PrivInstruction<CallType> {
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

struct Compilation {
  pub functions: HashMap<String, HashMap<String, Function>>,
}

impl Compilation {
  /// "Link" the functions.
  pub fn to_package(&self) -> Package {
    Package::new()
  }
}

impl Package {
  pub fn new() -> Self {
    Package {
      functions: vec![],
      main: None,
    }
  }

  pub fn set_main(&mut self, main: Option<(usize, usize)>) {
    self.main = main
  }

  // Is this really necessary tho
  pub fn find_function(&self, module_name: &str, function_name: &str) -> Option<&Function> {
    for (modname, module) in &self.functions {
      if modname == module_name {}
        for (funcname, func) in module {
          if funcname == function_name {
            return Some(func)
          }
        }
    }
    None
  }

  pub fn get_function(&self, module: usize, function: usize) -> Option<&Function> {
    self.functions.get(module).and_then(|(_,m)| m.get(function)).map(|(_, f)| f)
  }

  pub fn add_module(&mut self, name: String, module: Vec<(String, Function)>) {
    self.functions.push((name, module))
  }
}

pub fn compile_module(asts: &[AST]) -> Result<Vec<(String, Function)>, String> {
  let mut functions = vec![];
  for ast in asts {
    match ast {
      AST::DefineFn(func) => functions.push((func.name.clone(), compile_function(func)?)),
      x => return Err(format!("Unexpected form at top-level: {:?}", x)),
    };
  }
  Ok(functions)
}

fn compile_function(f: &parser::Function) -> Result<Function, String> {
  let mut num_locals = f.params.len() as u16;
  // Map of local-name to local-index
  let mut locals = HashMap::new();
  for (idx, param) in f.params.iter().enumerate() {
    locals.insert(param.clone(), idx as u16);
  }
  let mut instructions = vec![];
  for ast in &f.code {
    instructions.extend(compile_expr(ast, &mut num_locals, &mut locals)?);
  }
  instructions.push(Instruction::Return);
  Ok(Function {
    num_params: f.params.len() as u16,
    num_locals: num_locals as u16,
    instructions: instructions,
  })
}

fn compile_expr(
  ast: &AST,
  num_locals: &mut u16,
  locals: &mut HashMap<String, u16>,
) -> Result<Vec<Instruction>, String> {
  let mut instructions = vec![];
  match ast {
    AST::Let(name, box_expr) => {
      if !locals.contains_key(name) {
        locals.insert(name.clone(), *num_locals);
        *num_locals += 1;
      }
      instructions.extend(compile_expr(&box_expr, num_locals, locals)?);
      instructions.push(Instruction::SetLocal(locals[name]))
    }
    AST::DefineFn(func) => return Err(format!("NYI: Can't define inner functions: {}", func.name)),
    AST::Call(box_expr, arg_exprs) => {
      for expr in arg_exprs {
        instructions.extend(compile_expr(&expr, num_locals, locals)?);
      }
      match &**box_expr {
        AST::Variable(name) => instructions.push(Instruction::Call(name.to_string())),
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
