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
  Return,
  Add,
}

impl Module {
  pub fn get_function(&self, name: &str) -> Result<&Function, String> {
    self
      .functions
      .get(name)
      .ok_or_else(|| format!("No function named {}", name))
  }
}

pub fn compile_module(asts: &[AST]) -> Result<Module, String> {
  let mut module = Module {
    functions: HashMap::new(),
  };
  Ok(module)
}

pub fn compile_from_source(s: &str) -> Result<Module, String> {
  let asts = read_multiple(s)?;
  compile_module(&asts)
}

fn compile_function(f: &parser::Function) -> Result<Function, String> {
  Ok(Function {
    num_params: 0,
    num_locals: 0,
    instructions: vec![],
  })
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn compile_id() {
    let func = parser::Function {
      name: "id".to_string(),
      params: vec!["a".to_string()],
      code: Box::new(AST::Variable("a".to_string())),
    };
    let code = compile_function(&func).unwrap();
    assert_eq!(
      code,
      Function {
        num_params: 0,
        num_locals: 0,
        instructions: vec![Instruction::LoadLocal(0), Instruction::Return],
      }
    );
  }

  #[test]
  fn compile_basic_module() {
    let m = compile_from_source("(fn id (a) a)").unwrap();
    assert_eq!(
      m,
      Module {
        functions: hashmap!{
        "id".to_string() =>
            Function { num_params: 1, num_locals: 1, instructions: vec![Instruction::Return]}},
      }
    )
  }

}
