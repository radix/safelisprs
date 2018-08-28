use std::collections::HashMap;

use parser::{self, read_multiple, AST};

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
  pub functions: HashMap<String, Function>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
  pub num_params: u16,
  pub num_locals: u16,
  pub instructions: Vec<Instruction>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
  /// loads local variable onto the stack
  LoadLocal(u16),
  /// assigns top of the stack to local variable.
  SetLocal(u16),
  PushInt(i64),
  PushFloat(f64),
  PushString(String),
  /// discards topmost stack item
  Pop,
  Call(String),
  /// Exit the current function, returning the TOS to the caller
  Return,
}

impl Module {
  pub fn get_function(&self, name: &str) -> Option<&Function> {
    self.functions.get(name)
  }
}

pub fn compile_module(asts: &[AST]) -> Result<Module, String> {
  let mut functions = hashmap!{};
  for ast in asts {
    match ast {
      AST::DefineFn(func) => functions.insert(func.name.clone(), compile_function(func)?),
      x => return Err(format!("Unexpected form at module-level: {:?}", x)),
    };
  }
  let module = Module {
    functions: functions,
  };
  Ok(module)
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
    AST::DefineFn(func) => {}
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
    AST::Float(f) => {}
    AST::String(s) => {}
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
