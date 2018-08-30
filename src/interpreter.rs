use std::rc::Rc; // TODO: use Manishearth/rust-gc

use builtins::builtin_builtins;
use compiler::{Callable, CompiledFunction as Function, Instruction, Package};

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
  package: Package,
  builtins: B,
}

impl<B> Interpreter<B> {
  pub fn with_builtins(package: Package, builtins: B) -> Self {
    Interpreter {
      package,
      builtins: builtins,
    }
  }
}

impl Interpreter<fn(&str, &str, &mut Stack) -> BuiltinResult> {
  pub fn new(package: Package) -> Self {
    Interpreter {
      package,
      builtins: builtin_builtins,
    }
  }
}

impl<B> Interpreter<B>
where
  B: for<'r, 's> FnMut(&'r str, &'r str, &'s mut Stack) -> BuiltinResult,
{
  pub fn call_main(&mut self) -> Result<Rc<SLVal>, String> {
    if let Some((module, function)) = self.package.main {
      let callable = self
        .package
        .get_function(module, function)
        .ok_or_else(|| format!("Couldn't find module {} function {}", module, function))?;
      if let Callable::Function(function) = callable {
        eval_code(
          &self.package,
          function,
          alloc_locals(function),
          &mut self.builtins,
        )
      } else {
        Err(format!(
          "{}.{} is a builtin. We can only call regular functions.",
          module, function
        ))
      }
    } else {
      Err(format!("This package has no main function."))
    }
  }
}

fn alloc_locals(code: &Function) -> Vec<Rc<SLVal>> {
  vec![Rc::new(SLVal::Void); usize::from(code.num_locals)]
}

fn eval_code<B>(
  package: &Package,
  code: &Function,
  mut locals: Vec<Rc<SLVal>>,
  builtins: &mut B,
) -> Result<Rc<SLVal>, String>
where
  B: for<'r, 's> FnMut(&'r str, &'r str, &'s mut Stack) -> BuiltinResult,
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
      Instruction::Call((mod_index, func_index)) => {
        prim_call(package, &mut stack, *mod_index, *func_index, builtins)?
      }
    }
  }
  Ok(Rc::new(SLVal::Void))
}

fn prim_call<B>(
  package: &Package,
  stack: &mut Stack,
  mod_index: usize,
  func_index: usize,
  builtins: &mut B,
) -> Result<(), String>
where
  B: for<'r, 's> FnMut(&'r str, &'r str, &'s mut Stack) -> BuiltinResult,
{
  let (module_name, functions) = package
    .get_module(mod_index)
    .ok_or_else(|| format!("Module not found: {}", mod_index))?;
  let (func_name, callable) = functions
    .get(func_index)
    .ok_or_else(|| format!("Function not found: {}/{}", mod_index, func_index))?;
  match callable {
    Callable::Function(func) => {
      let mut locals = alloc_locals(func);
      // The parameters are "in order" on the stack, so popping will give them to us
      // in reverse order.
      for param_idx in (0..func.num_params).rev() {
        locals[usize::from(param_idx)] = stack.pop()?;
      }
      stack.push(eval_code(package, func, locals, builtins)?);
    }
    Callable::Builtin => {
      (builtins)(module_name, func_name, stack)
        .ok_or_else(|| format!("No function named {}", func_name))??;
    }
  }
  Ok(())
}

#[cfg(test)]
mod test {
  use super::*;
  use compiler::{self, *};

  #[test]
  fn test_interpret_id() {
    let empty_mod = Package {
      functions: vec![],
      main: None,
    };
    let code = compiler::Function {
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
    fn mybuiltins(mod_name: &str, name: &str, stack: &mut Stack) -> BuiltinResult {
      match (mod_name, name) {
        ("main", "add2") => {
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

    let source = "
      (decl add2 (n))
      (fn main () (add2 3))
    "
      .to_string();
    let package =
      compile_executable_from_sources(&[("main".to_string(), source)], ("main", "main")).unwrap();

    let mut interpreter = Interpreter::with_builtins(package, mybuiltins);
    assert_eq!(interpreter.call_main().unwrap(), Rc::new(SLVal::Int(5)));
  }
}
