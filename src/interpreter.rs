use std::default::Default;
use std::rc::Rc; // TODO: use Manishearth/rust-gc

use crate::builtins::DefaultBuiltins;
use crate::compiler::{Callable, Instruction, LinkedFunction as Function, Package};

#[derive(Debug, Default)]
pub struct Stack {
  items: Vec<Rc<SLVal>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum SLVal {
  Int(i64),
  Float(f64),
  String(String),
  Void,
  FunctionRef(u32, u32),
  Partial(Partial),
  Cell(Rc<SLVal>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Partial {
  function: (u32, u32),
  args: Vec<Rc<SLVal>>,
}

pub type BuiltinResult = Option<Result<(), String>>;

pub trait Builtins {
  fn call(&self, mod_name: &str, func_name: &str, stack: &mut Stack) -> BuiltinResult;
}

impl Stack {
  pub fn new() -> Self {
    Stack { items: vec![] }
  }
  pub fn pop(&mut self) -> Result<Rc<SLVal>, String> {
    self
      .items
      .pop()
      .ok_or_else(|| "POP on an empty stack".to_string())
  }
  pub fn push(&mut self, item: Rc<SLVal>) {
    self.items.push(item);
  }
  pub fn peek(&self) -> Result<Rc<SLVal>, String> {
    self
      .items
      .last()
      .cloned()
      .ok_or_else(|| "PEEK on an empty stack".to_string())
  }
}

pub struct Interpreter<B> {
  package: Package,
  builtins: B,
}

impl<B> Interpreter<B> {
  pub fn with_builtins(package: Package, builtins: B) -> Self {
    Interpreter { package, builtins }
  }
}

impl Interpreter<DefaultBuiltins> {
  pub fn new(package: Package) -> Self {
    Interpreter {
      package,
      builtins: DefaultBuiltins,
    }
  }
}

impl<B> Interpreter<B>
where
  B: Builtins,
{
  pub fn call_main(&mut self) -> Result<Rc<SLVal>, String> {
    if let Some((module, function)) = self.package.main {
      let callable = self
        .package
        .get_function(module, function)
        .ok_or_else(|| format!("Couldn't find module {} function {}", module, function))?;
      if let Callable::Function(function) = callable {
        let mut exec = Execution::new(&self.package, &self.builtins);
        exec.eval_code(function, alloc_locals(function))
      } else {
        Err(format!(
          "{}.{} is a builtin. We can only call regular functions.",
          module, function
        ))
      }
    } else {
      Err("This package has no main function.".to_string())
    }
  }

  pub fn call_slval(&mut self, slval: Rc<SLVal>) -> Result<Rc<SLVal>, String> {
    //! Call a FunctionRef or a Partial in an SLVal, returning the result.
    let mut exec = Execution::new(&self.package, &self.builtins);
    exec.stack.push(slval);
    exec.call_dynamic()?;
    exec.stack.pop()
  }
}

pub struct Execution<'p, 'b, B: Builtins + ?Sized> {
  pub package: &'p Package,
  pub builtins: &'b B,
  pub stack: Stack,
}

impl<'p, 'b, B: Builtins + ?Sized> Execution<'p, 'b, B> {
  pub fn new(package: &'p Package, builtins: &'b B) -> Self {
    Execution {
      package,
      builtins,
      stack: Stack::new(),
    }
  }
}

impl<'p, 'b, B> Execution<'p, 'b, B>
where
  B: Builtins + ?Sized,
{
  pub fn eval_code(
    &mut self,
    code: &Function,
    mut locals: Vec<Rc<SLVal>>,
  ) -> Result<Rc<SLVal>, String> {
    for inst in &code.instructions {
      match inst {
        Instruction::PushInt(i) => self.stack.push(Rc::new(SLVal::Int(*i))),
        Instruction::PushFloat(f) => self.stack.push(Rc::new(SLVal::Float(*f))),
        Instruction::PushString(s) => self.stack.push(Rc::new(SLVal::String(s.clone()))),
        Instruction::Pop => {
          self.stack.pop()?;
        }
        Instruction::Return => return self.stack.pop(),
        Instruction::SetLocal(i) => locals[usize::from(*i)] = self.stack.pop()?,
        Instruction::LoadLocal(i) => self.stack.push(locals[usize::from(*i)].clone()),
        Instruction::Call((mod_index, func_index)) => self.call_fixed(*mod_index, *func_index)?,
        Instruction::CallDynamic => self.call_dynamic()?,
        Instruction::MakeFunctionRef((mod_index, func_index)) => self
          .stack
          .push(Rc::new(SLVal::FunctionRef(*mod_index, *func_index))),
        Instruction::MakeCell => {
          let val = self.stack.pop()?;
          self.stack.push(Rc::new(SLVal::Cell(val)));
        }
        Instruction::DerefCell => {
          let val = self.stack.pop()?;
          match &*val {
            SLVal::Cell(r) => self.stack.push(r.clone()),
            other => return Err(format!("Not a cell: {:?}", other)),
          }
        }
        Instruction::PartialApply(num_args) => {
          self.partial_apply(*num_args)?;
        }
      }
    }
    Ok(Rc::new(SLVal::Void))
  }

  pub fn partial_apply(&mut self, num_args: u16) -> Result<(), String> {
    let func = self.stack.pop()?;
    let mut args = vec![];
    for _ in 0..num_args {
      args.push(self.stack.pop()?)
    }
    args.reverse();
    let closure = match &*func {
      SLVal::FunctionRef(mod_index, func_index) => Ok(Rc::new(SLVal::Partial(Partial {
        function: (*mod_index, *func_index),
        args,
      }))),
      _ => Err("make_closure needs a function at TOS".to_string()),
    }?;
    self.stack.push(closure);
    Ok(())
  }

  pub fn call_dynamic(&mut self) -> Result<(), String> {
    //! Call the function that's on the top of stack.
    let callable = self.stack.pop()?;
    match &*callable {
      SLVal::FunctionRef(mod_index, func_index) => self.call_fixed(*mod_index, *func_index),
      SLVal::Partial(Partial {
        function: (mod_index, func_index),
        args,
      }) => {
        let args_len = args.len();
        let (_, functions) = self
          .package
          .get_module(*mod_index)
          .ok_or_else(|| format!("Module not found: {}", mod_index))?;
        let (_, callable) = functions
          .get(*func_index as usize)
          .ok_or_else(|| format!("Function not found: {}/{}", mod_index, func_index))?;
        match callable {
          Callable::Function(func) => {
            let mut locals = args.clone();
            locals.extend(alloc_locals(func));
            self.place_locals(&mut locals, args_len, func)?;
            let result = self.eval_code(func, locals)?;
            self.stack.push(result);
            Ok(())
          }
          Callable::Builtin => Err("Can't invoke a builtin as a closure".to_string()),
        }
      }
      x => Err(format!("Can't call a non-callable! {:?}", x)),
    }
  }

  pub fn call_fixed(&mut self, mod_index: u32, func_index: u32) -> Result<(), String> {
    let (module_name, functions) = self
      .package
      .get_module(mod_index)
      .ok_or_else(|| format!("Module not found: {}", mod_index))?;
    let (func_name, callable) = functions
      .get(func_index as usize)
      .ok_or_else(|| format!("Function not found: {}/{}", mod_index, func_index))?;
    match callable {
      Callable::Function(func) => {
        let mut locals = alloc_locals(func);
        self.place_locals(&mut locals, 0, func)?;
        let result = self.eval_code(func, locals)?;
        self.stack.push(result);
      }
      Callable::Builtin => {
        self
          .builtins
          .call(module_name, func_name, &mut self.stack)
          .ok_or_else(|| format!("No function named {}", func_name))??;
      }
    }
    Ok(())
  }

  pub fn place_locals(
    &mut self,
    locals: &mut [Rc<SLVal>],
    start: usize,
    func: &Function,
  ) -> Result<(), String> {
    // The parameters are "in order" on the stack, so popping will give them to us
    // in reverse order.
    for param_idx in (start..usize::from(func.num_params)).rev() {
      locals[param_idx] = self.stack.pop()?;
    }
    Ok(())
  }
}

fn alloc_locals(code: &Function) -> Vec<Rc<SLVal>> {
  vec![Rc::new(SLVal::Void); usize::from(code.num_locals)]
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::builtins::DefaultBuiltins;
  use crate::compiler::{self, *};

  fn eval_main(source: &str) -> Rc<SLVal> {
    let pkg = compile_executable_from_source(&source.to_string(), ("main", "main")).unwrap();
    let mut interp = Interpreter::new(pkg);
    interp.call_main().unwrap()
  }

  #[test]
  fn test_interpret_id() {
    let empty_mod = Package {
      modules: vec![],
      main: None,
    };
    let code = compiler::Function {
      num_locals: 1,
      num_params: 1,
      instructions: vec![Instruction::LoadLocal(0), Instruction::Return],
    };
    let locals = vec![Rc::new(SLVal::Int(42))];
    let mut exec = Execution::new(&empty_mod, &DefaultBuiltins);
    let result = exec.eval_code(&code, locals).unwrap();
    assert_eq!(result, Rc::new(SLVal::Int(42)));
  }

  #[test]
  fn extending_builtins() {
    struct MyBuiltins;
    impl Builtins for MyBuiltins {
      fn call(&self, mod_name: &str, name: &str, stack: &mut Stack) -> BuiltinResult {
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
    }

    let source = "
      (decl add2 (n))
      (fn main () (add2 3))
    "
    .to_string();
    let package = compile_executable_from_source(&source, ("main", "main")).unwrap();

    let mut interpreter = Interpreter::with_builtins(package, MyBuiltins);
    assert_eq!(interpreter.call_main().unwrap(), Rc::new(SLVal::Int(5)));
  }

  #[test]
  fn closure_bytecode() {
    let main = compiler::Function {
      num_locals: 1,
      num_params: 0,
      instructions: vec![
        Instruction::PushInt(42),
        Instruction::MakeCell,
        Instruction::MakeFunctionRef((0, 0)),
        Instruction::PartialApply(1),
        Instruction::Return,
      ],
    };

    let inner = compiler::Function {
      num_locals: 1,
      num_params: 0,
      instructions: vec![
        Instruction::LoadLocal(0),
        Instruction::DerefCell,
        Instruction::Return,
      ],
    };

    let pkg = Package {
      modules: vec![(
        "main".to_string(),
        vec![
          ("inner".to_string(), Callable::Function(inner)),
          ("main".to_string(), Callable::Function(main)),
        ],
      )],
      main: Some((0, 1)),
    };

    let mut interp = Interpreter::new(pkg);
    let result = interp.call_main().unwrap();
    let result = interp.call_slval(result).unwrap();
    assert_eq!(result, Rc::new(SLVal::Int(42)));
  }

  #[test]
  fn closure_end_to_end() {
    let source = "
      (fn outer ()
        (let a 1)
        (fn inner () a)
        inner
      )
      (fn main () ((outer)))
    "
    .to_string();
    let pkg = compile_executable_from_source(&source, ("main", "main")).unwrap();
    let mut interp = Interpreter::new(pkg);
    assert_eq!(interp.call_main().unwrap(), Rc::new(SLVal::Int(1)));
  }

  #[test]
  fn nested_closure_end_to_end() {
    let source = "
      (fn outer ()
        (let a 1)
        (fn middle ()
          (fn inner () a)
          inner)
        middle)
      (fn main () (((outer))))
    ";
    assert_eq!(eval_main(source), Rc::new(SLVal::Int(1)));
  }

  #[test]
  fn closure_captures_outer_parameter() {
    let source = "
      (fn outer (a)
        (fn inner () a)
        inner)
      (fn main () ((outer 7)))
    ";
    assert_eq!(eval_main(source), Rc::new(SLVal::Int(7)));
  }

  #[test]
  fn let_expression_returns_bound_value() {
    let source = "
      (fn main () (let a 1))
    ";
    assert_eq!(eval_main(source), Rc::new(SLVal::Int(1)));
  }

  #[test]
  fn later_let_expression_is_returned() {
    let source = "
      (fn main ()
        (let a 1)
        (let b 2))
    ";
    assert_eq!(eval_main(source), Rc::new(SLVal::Int(2)));
  }

  #[test]
  fn let_return_value_does_not_shadow_later_variable_result() {
    let source = "
      (fn main ()
        (let a 1)
        a)
    ";
    assert_eq!(eval_main(source), Rc::new(SLVal::Int(1)));
  }

  #[test]
  fn closure_capture_order() {
    let source = "
      (fn outer ()
        (let a 1)
        (let b 2)
        (fn inner ()
          (let ignore b)
          a)
        inner)
      (fn main () ((outer)))
    ";
    assert_eq!(eval_main(source), Rc::new(SLVal::Int(1)));
  }

  #[test]
  fn non_capturing_closure_end_to_end() {
    let source = "
      (fn outer ()
        (fn inner () 5)
        inner)
      (fn main () ((outer)))
    ";
    assert_eq!(eval_main(source), Rc::new(SLVal::Int(5)));
  }

  #[test]
  fn local_nested_function_can_be_called() {
    let source = "
      (fn outer ()
        (fn inner () 5)
        (inner))
      (fn main () (outer))
    ";
    assert_eq!(eval_main(source), Rc::new(SLVal::Int(5)));
  }

  #[test]
  fn captured_local_nested_function_can_be_called() {
    let source = "
      (fn outer ()
        (fn inner () 5)
        (fn caller () (inner))
        caller)
      (fn main () ((outer)))
    ";
    assert_eq!(eval_main(source), Rc::new(SLVal::Int(5)));
  }

  #[test]
  fn function_definition_expression_returns_function() {
    let source = "
      (fn outer ()
        (fn inner () 5))
      (fn main () ((outer)))
    ";
    assert_eq!(eval_main(source), Rc::new(SLVal::Int(5)));
  }
}
