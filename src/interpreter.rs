use std::collections::HashMap;
use std::rc::Rc; // TODO: use Manishearth/rust-gc

use builtins::builtin_builtins;
use compiler::{Function, Instruction, Module};

pub struct Stack {
  items: Vec<Rc<SLVal>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum SLVal {
  Int(i64),
  Float(f64),
  String(String),
  Symbol(String),
  List(Vec<SLVal>),
  Void,
}

pub type BuiltinResult = Option<Result<(), String>>;

impl Stack {
  pub fn new() -> Self {
    Stack { items: vec![] }
  }
  pub fn pop(&mut self) -> Result<Rc<SLVal>, String> {
    self
      .items
      .pop()
      .ok_or_else(|| format!("POP on an empty stack"))
  }
  pub fn push(&mut self, item: Rc<SLVal>) {
    self.items.push(item);
  }
  pub fn peek(&self) -> Result<Rc<SLVal>, String> {
    self
      .items
      .last()
      .map(|x| x.clone())
      .ok_or_else(|| format!("PEEK on an empty stack"))
  }
}

pub struct Interpreter<B> {
  modules: HashMap<String, Module>,
  builtins: B,
}

impl<B> Interpreter<B> {
  pub fn with_builtins(builtins: B) -> Self {
    Interpreter {
      modules: hashmap!{},
      builtins: builtins,
    }
  }
}

impl Interpreter<fn(&str, &mut Stack) -> BuiltinResult> {
  pub fn new() -> Self {
    Interpreter {
      modules: hashmap!{},
      builtins: builtin_builtins,
    }
  }
}

impl<B> Interpreter<B>
where
  B: for<'r, 's> FnMut(&str, &mut Stack) -> BuiltinResult,
{
  pub fn add_module(&mut self, name: String, module: Module) {
    self.modules.insert(name, module);
  }

  pub fn call_in_module(
    &mut self,
    module_name: &str,
    function_name: &str,
  ) -> Result<Rc<SLVal>, String> {
    let module = self
      .modules
      .get(module_name)
      .ok_or_else(|| format!("Couldn't find module {}", module_name))?;
    call_in_module(&module, function_name, &mut self.builtins)
  }
}

pub fn call_in_module<B>(module: &Module, main: &str, builtins: &mut B) -> Result<Rc<SLVal>, String>
where
  B: for<'r, 's> FnMut(&'r str, &'s mut Stack) -> BuiltinResult,
{
  let code = module
    .functions
    .get(main)
    .ok_or_else(|| format!("Couldn't find function {}", main))?;
  let locals = alloc_locals(code);
  eval_code(module, code, locals, builtins)
}

fn alloc_locals(code: &Function) -> Vec<Rc<SLVal>> {
  vec![Rc::new(SLVal::Void); usize::from(code.num_locals)]
}

fn eval_code<B>(
  module: &Module,
  code: &Function,
  mut locals: Vec<Rc<SLVal>>,
  builtins: &mut B,
) -> Result<Rc<SLVal>, String>
where
  B: for<'r, 's> FnMut(&'r str, &'s mut Stack) -> BuiltinResult,
{
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
      Instruction::Call(name) => prim_call(module, &mut stack, &name, builtins)?,
    }
  }
  Ok(Rc::new(SLVal::Void))
}

fn prim_call<B>(
  module: &Module,
  stack: &mut Stack,
  name: &str,
  builtins: &mut B,
) -> Result<(), String>
where
  B: for<'r, 's> FnMut(&'r str, &'s mut Stack) -> BuiltinResult,
{
  let func = module.get_function(name);
  if let Some(func) = func {
    let mut locals = alloc_locals(func);
    // The parameters are "in order" on the stack, so popping will give them to us in reverse order.
    for param_idx in (0..func.num_params).rev() {
      locals[usize::from(param_idx)] = stack.pop()?;
    }
    stack.push(eval_code(module, func, locals, builtins)?);
  } else {
    (builtins)(name, stack).ok_or_else(|| format!("No function named {}", name))??;
  }
  Ok(())
}

#[cfg(test)]
mod test {
  use super::super::compile_from_source;
  use super::*;
  use compiler::*;

  #[test]
  fn test_id() {
    let empty_mod = Module {
      functions: hashmap!{},
    };
    let code = Function {
      num_locals: 1,
      num_params: 1,
      instructions: vec![Instruction::LoadLocal(0), Instruction::Return],
    };
    let locals = vec![Rc::new(SLVal::Int(42))];
    let result = eval_code(&empty_mod, &code, locals, &mut builtin_builtins).unwrap();
    assert_eq!(result, Rc::new(SLVal::Int(42)));
  }

  #[test]
  fn extending_builtins() {
    fn mybuiltins(name: &str, stack: &mut Stack) -> BuiltinResult {
      match name {
        "add2" => {
          if let Ok(SLVal::Int(n)) = stack.pop().map(|x| (&*x).clone()) {
            stack.push(Rc::new(SLVal::Int(n + 2)));
            Some(Ok(()))
          } else {
            Some(Err("nope".to_string()))
          }
        }
        _ => None,
      }
    }

    let source = "(fn main () (add2 3))";
    let module = compile_from_source(source).unwrap();

    let mut interpreter = Interpreter::with_builtins(mybuiltins);
    interpreter.add_module("mymod".to_string(), module);
    assert_eq!(
      interpreter.call_in_module("mymod", "main").unwrap(),
      Rc::new(SLVal::Int(5))
    );
  }
}
