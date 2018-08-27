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
  let mut num_locals = f.params.len() as u16;
  /// Map of local-name to local-index
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

fn compile_expr(ast: &AST, num_locals: &mut u16, locals: &mut HashMap<String, u16>) -> Result<Vec<Instruction>, String> {
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
    AST::Call(box_expr, arg_exprs) => {}
    AST::Variable(name) => {
      if !locals.contains_key(name) {
        return Err(format!("Function accesses unbound variable {}", name))
      }
      instructions.push(Instruction::LoadLocal(locals[name]));
    }
    AST::Int(i) => {}
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

  #[test]
  fn compile_basic_module() {
    let m = compile_from_source("(fn id (a) a)").unwrap();
    assert_eq!(
      m,
      Module {
        functions: hashmap!{
        "id".to_string() =>
            Function { num_params: 1, num_locals: 1, instructions: vec![]}},
      }
    )
  }

}
