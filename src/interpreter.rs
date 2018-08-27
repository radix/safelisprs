use std::rc::Rc; // TODO: use Manishearth/rust-gc

use compiler::{Code, Instruction, Module};

struct Stack {
  items: Vec<Rc<SLVal>>,
}

#[derive(Debug, PartialEq)]
pub enum SLVal {
  Int(i64),
  Float(f64),
  String(String),
  Symbol(String),
  List(Vec<SLVal>),
  Void,
  // Function(Function),
}

impl Stack {
  fn new() -> Self {
    Stack { items: vec![] }
  }
  fn pop(&mut self) -> Result<Rc<SLVal>, String> {
    self
      .items
      .pop()
      .ok_or_else(|| format!("POP on an empty stack"))
  }
  fn push(&mut self, item: Rc<SLVal>) {
    self.items.push(item);
  }
  fn peek(&self) -> Result<Rc<SLVal>, String> {
    self
      .items
      .last()
      .map(|x| x.clone())
      .ok_or_else(|| format!("PEEK on an empty stack"))
  }
}

fn call_in_module(module: &Module, main: &str) -> Result<Rc<SLVal>, String> {
  let code = module
    .functions
    .get(main)
    .ok_or_else(|| format!("Couldn't find function {}", main))?;
  let locals = alloc_locals(code);
  eval_code(module, code, locals)
}

fn alloc_locals(code: &Code) -> Vec<Rc<SLVal>> {
  vec![Rc::new(SLVal::Void); usize::from(code.num_locals)]
}

fn eval_code(module: &Module, code: &Code, mut locals: Vec<Rc<SLVal>>) -> Result<Rc<SLVal>, String> {
  // wait, is this right? Shouldn't there be ONE stack, instead of one stack per frame?
  let mut stack: Stack = Stack::new();
  for inst in &code.instructions {
    match inst {
      Instruction::PushInt(i) => stack.push(Rc::new(SLVal::Int(*i))),
      Instruction::PushFloat(f) => stack.push(Rc::new(SLVal::Float(*f))),
      Instruction::PushString(s) => stack.push(Rc::new(SLVal::String(s.clone()))),
      Instruction::Pop => {
        stack.pop()?;
      }
      Instruction::Return => return stack.pop(),
      Instruction::SetLocal(i) => locals[usize::from(*i)] = stack.peek()?,
      Instruction::LoadLocal(i) => stack.push(locals[usize::from(*i)].clone()),
      Instruction::Call(name) => prim_call(module, &mut stack, &name)?,
      Instruction::Add => prim_add(&mut stack)?,
    }
  }
  Ok(Rc::new(SLVal::Void))
}

fn prim_call(module: &Module, stack: &mut Stack, name: &str) -> Result<(), String> {
  let func_code = module.get_function(name)?;
  let mut locals = alloc_locals(func_code);
  for param_idx in 0..func_code.num_params {
    locals[usize::from(param_idx)] = stack.pop()?;
  }
  stack.push(eval_code(module, func_code, locals)?);
  Ok(())
}

fn prim_add(stack: &mut Stack) -> Result<(), String> {
  let one = stack.pop()?;
  let two = stack.pop()?;
  let result = match (&*one, &*two) {
    (SLVal::Int(one), SLVal::Int(two)) => Rc::new(SLVal::Int(one + two)),
    (SLVal::Float(one), SLVal::Float(two)) => Rc::new(SLVal::Float(one + two)),
    _ => return Err(format!("Couldn't add {:?} and {:?}", one, two)),
  };
  stack.push(result);
  Ok(())
}


#[cfg(test)]
mod test {
  use super::*;
  use compiler::*;

  #[test]
  fn test_id() {
    let empty_mod = Module { functions: hashmap!{}};
    let code = Code { num_locals: 1, num_params: 1, instructions: vec![Instruction::LoadLocal(0), Instruction::Return]};
    let locals = vec![Rc::new(SLVal::Int(42))];
    let result = eval_code(&empty_mod, &code, locals).unwrap();
    assert_eq!(result, Rc::new(SLVal::Int(42)));
  }
}
