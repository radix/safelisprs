use std::default::Default;
use std::rc::Rc; // TODO: use Manishearth/rust-gc
use std::time::{Duration, Instant};

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
  Bool(bool),
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
  B: Builtins + Clone,
{
  /// Set up an `Execution` ready to run `main`.
  pub fn call_main(&self) -> Result<Execution<'_, B>, String> {
    if let Some((module, function)) = self.package.main {
      let callable = self
        .package
        .get_function(module, function)
        .ok_or_else(|| format!("Couldn't find module {} function {}", module, function))?;
      if let Callable::Function(function) = callable {
        let mut exec = Execution::new(&self.package, self.builtins.clone());
        exec.enter_function(function, vec![])?;
        Ok(exec)
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

  /// Set up an `Execution` ready to call `slval`.
  pub fn call_slval(&self, slval: Rc<SLVal>) -> Result<Execution<'_, B>, String> {
    let mut exec = Execution::new(&self.package, self.builtins.clone());
    exec.stack.push(slval);
    exec.call_dynamic()?;
    Ok(exec)
  }
}

#[derive(Debug, PartialEq)]
pub enum Status {
  /// Execution paused after exhausting the budget for this `run` call.
  /// The execution can be resumed by calling `run` again. All state
  /// (frames, stack, IP, counter) is preserved.
  Paused,
  /// Execution completed: the frame stack is empty and the final result
  /// is carried here. The `Execution` should not be resumed after this.
  Done(Rc<SLVal>),
}

struct Frame<'p> {
  function: &'p Function,
  locals: Vec<Rc<SLVal>>,
  ip: usize,
}

pub struct Execution<'p, B: Builtins> {
  pub package: &'p Package,
  pub builtins: B,
  pub stack: Stack,
  frames: Vec<Frame<'p>>,
  pub executed: u64,
}

impl<'p, B: Builtins> Execution<'p, B> {
  pub fn new(package: &'p Package, builtins: B) -> Self {
    Execution {
      package,
      builtins,
      stack: Stack::new(),
      frames: vec![],
      executed: 0,
    }
  }

  /// Returns true if the frame stack is empty (execution is complete).
  pub fn is_done(&self) -> bool {
    self.frames.is_empty()
  }
}

impl<'p, B> Execution<'p, B>
where
  B: Builtins,
{
  /// Run up to `n` bytecodes. Returns `Paused` if the budget exhausted before
  /// completion, or `Done(v)` if the program completed. Errors propagate via
  /// `Err`. The cumulative count of executed bytecodes is available on
  /// `Execution::executed` after the call returns.
  pub fn run(&mut self, n: u64) -> Result<Status, String> {
    let start = self.executed;
    while !self.is_done() && self.executed - start < n {
      self.step()?;
    }
    if self.is_done() {
      Ok(Status::Done(self.stack.pop()?))
    } else {
      Ok(Status::Paused)
    }
  }

  /// Convenience: run to completion with no instruction limit. Returns the
  /// final value, or errors on a runtime error.
  pub fn run_until_done(&mut self) -> Result<Rc<SLVal>, String> {
    while !self.is_done() {
      self.step()?;
    }
    self.stack.pop()
  }

  /// Run for up to `duration`, stepping until the deadline is reached or
  /// execution completes. Returns `Paused` if the deadline expired before
  /// completion, or `Done(v)` if the program completed. Errors propagate via
  /// `Err`. The cumulative count of executed bytecodes is available on
  /// `Execution::executed` after the call returns.
  pub fn run_for_duration(&mut self, duration: Duration) -> Result<Status, String> {
    let deadline = Instant::now() + duration;
    while !self.is_done() && Instant::now() < deadline {
      self.step()?;
    }
    if self.is_done() {
      Ok(Status::Done(self.stack.pop()?))
    } else {
      Ok(Status::Paused)
    }
  }

  /// Execute one bytecode instruction from the current top frame.
  pub fn step(&mut self) -> Result<(), String> {
    self.executed = self.executed.saturating_add(1);
    let inst = {
      let frame = self
        .frames
        .last_mut()
        .ok_or_else(|| "step with no frames".to_string())?;
      if frame.ip >= frame.function.instructions.len() {
        return Err("ran past end of function without Return".to_string());
      }
      let inst = &frame.function.instructions[frame.ip];
      frame.ip += 1;
      inst
    };

    match inst {
      Instruction::PushInt(i) => self.stack.push(Rc::new(SLVal::Int(*i))),
      Instruction::PushFloat(f) => self.stack.push(Rc::new(SLVal::Float(*f))),
      Instruction::PushString(s) => self.stack.push(Rc::new(SLVal::String(s.clone()))),
      Instruction::PushBool(b) => self.stack.push(Rc::new(SLVal::Bool(*b))),
      Instruction::Pop => {
        self.stack.pop()?;
      }
      Instruction::Return => {
        // Pop the top frame; the return value stays on self.stack.
        self.frames.pop();
      }
      Instruction::SetLocal(i) => {
        let val = self.stack.pop()?;
        let frame = self
          .frames
          .last_mut()
          .ok_or_else(|| "SetLocal with no frame".to_string())?;
        frame.locals[usize::from(*i)] = val;
      }
      Instruction::LoadLocal(i) => {
        let frame = self
          .frames
          .last()
          .ok_or_else(|| "LoadLocal with no frame".to_string())?;
        self.stack.push(frame.locals[usize::from(*i)].clone());
      }
      Instruction::Call((mod_index, func_index)) => {
        self.call_fixed(*mod_index, *func_index)?;
      }
      Instruction::CallDynamic => {
        self.call_dynamic()?;
      }
      Instruction::MakeFunctionRef((mod_index, func_index)) => {
        self
          .stack
          .push(Rc::new(SLVal::FunctionRef(*mod_index, *func_index)));
      }
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
      Instruction::Jump(target) => {
        let frame = self
          .frames
          .last_mut()
          .ok_or_else(|| "Jump with no frame".to_string())?;
        frame.ip = *target as usize;
      }
      Instruction::JumpIfFalse(target) => {
        let val = self.stack.pop()?;
        match &*val {
          SLVal::Bool(false) => {
            let frame = self
              .frames
              .last_mut()
              .ok_or_else(|| "JumpIfFalse with no frame".to_string())?;
            frame.ip = *target as usize;
          }
          SLVal::Bool(true) => {}
          other => return Err(format!("`if` condition must be a bool, got {:?}", other)),
        }
      }
    }
    Ok(())
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

  /// Either pushes a frame for the function if it's defined in SafeLisp, or
  /// just call it immediately if it's a builtin.
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
        self.enter_function(func, vec![])?;
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

  /// Prepare the callable that's on the top of stack to be called. This pushes
  /// a new frame for function callables. This is both the implementation of the
  /// `CallDynamic` instruction and the entry point used by
  /// `Interpreter::call_slval`.
  pub fn call_dynamic(&mut self) -> Result<(), String> {
    let callable = self.stack.pop()?;
    match &*callable {
      SLVal::FunctionRef(mod_index, func_index) => self.call_fixed(*mod_index, *func_index),
      SLVal::Partial(Partial {
        function: (mod_index, func_index),
        args,
      }) => {
        let (_, functions) = self
          .package
          .get_module(*mod_index)
          .ok_or_else(|| format!("Module not found: {}", mod_index))?;
        let (_, callable) = functions
          .get(*func_index as usize)
          .ok_or_else(|| format!("Function not found: {}/{}", mod_index, func_index))?;
        match callable {
          Callable::Function(func) => self.enter_function(func, args.clone()),
          Callable::Builtin => Err("Can't invoke a builtin as a closure".to_string()),
        }
      }
      x => Err(format!("Can't call a non-callable! {:?}", x)),
    }
  }

  /// Push a new frame for a function, with optional pre-bound values. The
  /// remaining params (after the pre-bound ones) are popped from the stack.
  fn enter_function(
    &mut self,
    function: &'p Function,
    pre_bound: Vec<Rc<SLVal>>,
  ) -> Result<(), String> {
    let start = pre_bound.len();
    let mut locals = pre_bound;
    locals.extend(alloc_locals(function));
    // The parameters are "in order" on the stack, so popping will give them to
    // us in reverse order.
    for param_idx in (start..usize::from(function.num_params)).rev() {
      locals[param_idx] = self.stack.pop()?;
    }
    self.frames.push(Frame {
      function,
      locals,
      ip: 0,
    });
    Ok(())
  }
}

/// Generate locals, initialized to Void, for a given function.
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
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    exec.run_until_done().unwrap()
  }

  /// Test for a simple "identity" function that returns its argument
  #[test]
  fn test_interpret_identity() {
    // id takes one argument and returns it. main calls id with 42.
    let id = compiler::Function {
      num_locals: 1,
      num_params: 1,
      instructions: vec![Instruction::LoadLocal(0), Instruction::Return],
    };
    let main = compiler::Function {
      num_locals: 0,
      num_params: 0,
      instructions: vec![
        Instruction::PushInt(42),
        Instruction::Call((0, 0)),
        Instruction::Return,
      ],
    };
    let pkg = Package {
      modules: vec![(
        "main".to_string(),
        vec![
          ("id".to_string(), Callable::Function(id)),
          ("main".to_string(), Callable::Function(main)),
        ],
      )],
      main: Some((0, 1)),
    };
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    assert_eq!(exec.run_until_done().unwrap(), Rc::new(SLVal::Int(42)));
  }

  #[test]
  fn extending_builtins() {
    #[derive(Clone)]
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

    let interpreter = Interpreter::with_builtins(package, MyBuiltins);
    let mut exec = interpreter.call_main().unwrap();
    assert_eq!(exec.run_until_done().unwrap(), Rc::new(SLVal::Int(5)));
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

    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let result = exec.run_until_done().unwrap();
    let mut exec2 = interp.call_slval(result).unwrap();
    let result2 = exec2.run_until_done().unwrap();
    assert_eq!(result2, Rc::new(SLVal::Int(42)));
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
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    assert_eq!(exec.run_until_done().unwrap(), Rc::new(SLVal::Int(1)));
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

  #[test]
  fn step_advances_ip_and_leaves_value_on_stack() {
    let pkg = Package::default();
    let code = compiler::Function {
      num_locals: 0,
      num_params: 0,
      instructions: vec![Instruction::PushInt(7), Instruction::Return],
    };
    let mut exec = Execution::new(&pkg, DefaultBuiltins);
    exec.enter_function(&code, vec![]).unwrap();
    // After pushing the initial frame, there's one frame at ip 0.
    assert_eq!(exec.frames.len(), 1);
    exec.step().unwrap();
    // PushInt executed: stack has the value, frame still on the stack.
    assert_eq!(exec.stack.peek().unwrap(), Rc::new(SLVal::Int(7)));
    assert_eq!(exec.frames.len(), 1);
    exec.step().unwrap();
    // Return popped the frame; the value remains on the stack.
    assert_eq!(exec.frames.len(), 0);
    assert_eq!(exec.stack.peek().unwrap(), Rc::new(SLVal::Int(7)));
    let result = exec.run_until_done().unwrap();
    assert_eq!(result, Rc::new(SLVal::Int(7)));
  }

  #[test]
  fn run_until_done_pops_final_value() {
    let pkg = Package::default();
    let code = compiler::Function {
      num_locals: 0,
      num_params: 0,
      instructions: vec![Instruction::PushInt(99), Instruction::Return],
    };
    let mut exec = Execution::new(&pkg, DefaultBuiltins);
    exec.enter_function(&code, vec![]).unwrap();
    let result = exec.run_until_done().unwrap();
    assert_eq!(result, Rc::new(SLVal::Int(99)));
    // The stack should be empty after run_until_done pops the final value.
    assert!(exec.stack.peek().is_err());
  }

  #[test]
  fn running_past_end_without_return_errors() {
    let pkg = Package::default();
    let code = compiler::Function {
      num_locals: 0,
      num_params: 0,
      instructions: vec![Instruction::PushInt(1)],
    };
    let mut exec = Execution::new(&pkg, DefaultBuiltins);
    exec.enter_function(&code, vec![]).unwrap();
    exec.step().unwrap(); // PushInt
    let err = exec.step().unwrap_err();
    assert!(err.contains("ran past end"), "unexpected error: {}", err);
  }

  #[test]
  fn step_with_no_frames_errors() {
    let pkg = Package::default();
    let mut exec = Execution::new(&pkg, DefaultBuiltins);
    let err = exec.step().unwrap_err();
    assert!(err.contains("no frames"), "unexpected error: {}", err);
  }

  #[test]
  fn call_dynamic_on_non_callable_errors() {
    let pkg = Package::default();
    let mut exec = Execution::new(&pkg, DefaultBuiltins);
    exec.stack.push(Rc::new(SLVal::Int(3)));
    let err = exec.call_dynamic().unwrap_err();
    assert!(err.contains("non-callable"), "unexpected error: {}", err);
  }

  #[test]
  fn deref_cell_on_non_cell_errors() {
    let pkg = Package::default();
    let code = compiler::Function {
      num_locals: 0,
      num_params: 0,
      instructions: vec![Instruction::PushInt(1), Instruction::DerefCell],
    };
    let mut exec = Execution::new(&pkg, DefaultBuiltins);
    exec.enter_function(&code, vec![]).unwrap();
    exec.step().unwrap(); // PushInt
    let err = exec.step().unwrap_err(); // DerefCell
    assert!(err.contains("Not a cell"), "unexpected error: {}", err);
  }

  #[test]
  fn call_to_missing_module_errors() {
    let pkg = Package::default();
    let code = compiler::Function {
      num_locals: 0,
      num_params: 0,
      instructions: vec![Instruction::Call((0, 0))],
    };
    let mut exec = Execution::new(&pkg, DefaultBuiltins);
    exec.enter_function(&code, vec![]).unwrap();
    let err = exec.step().unwrap_err();
    assert!(
      err.contains("Module not found"),
      "unexpected error: {}",
      err
    );
  }

  #[test]
  fn partial_apply_then_call_dynamic_binds_pre_bound_args() {
    // inner takes one param and returns it. main builds a Partial with the
    // arg pre-bound, then CallDynamic's it with no extra args on the stack.
    let inner = compiler::Function {
      num_locals: 1,
      num_params: 1,
      instructions: vec![Instruction::LoadLocal(0), Instruction::Return],
    };
    let main = compiler::Function {
      num_locals: 0,
      num_params: 0,
      instructions: vec![
        Instruction::PushInt(42),
        Instruction::MakeFunctionRef((0, 0)),
        Instruction::PartialApply(1),
        Instruction::CallDynamic,
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
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    assert_eq!(exec.run_until_done().unwrap(), Rc::new(SLVal::Int(42)));
  }

  #[test]
  fn deep_recursion_does_not_overflow() {
    // Under the old recursive eval_code each Safelisp call was a Rust stack
    // frame, so this would have overflowed. This ensures we can handle very
    // large stack sizes.
    let source = "
      (use \"src/std\")
      (fn triangle (n)
        (if (std.== n 0)
          0
          (std.+ n (triangle (std.- n 1)))))
      (fn main () (triangle 50000))
    ";
    assert_eq!(eval_main(source), Rc::new(SLVal::Int(1250025000)));
  }

  #[test]
  fn function_call_leaves_no_stack_garbage() {
    // After main returns, the execution's value stack must contain exactly
    // one value: main's return value. Intermediate let-bindings and function
    // call results should not accumulate as garbage below it.
    let source = "
      (fn id (a) a)
      (fn waste ()
        (let a 1)
        (id 1)
        (let b 2)
        b)
      (fn main () (waste) (waste) (waste))
    ";
    let pkg = compile_executable_from_source(&source.to_string(), ("main", "main")).unwrap();
    let (mod_idx, fn_idx) = pkg.main.unwrap();
    let function = pkg.get_function(mod_idx, fn_idx).unwrap();
    if let Callable::Function(function) = function {
      let mut exec = Execution::new(&pkg, DefaultBuiltins);
      exec.enter_function(function, vec![]).unwrap();
      let result = exec.run_until_done().unwrap();
      assert_eq!(result, Rc::new(SLVal::Int(2)));
      // run_until_done pops the final value, so the stack should be empty.
      assert_eq!(exec.stack.items, vec![]);
    }
  }

  #[test]
  fn if_selects_then_branch_when_true() {
    let source = "
      (use \"src/std\")
      (fn main () (if (std.== 1 1) 42 0))
    ";
    assert_eq!(eval_main(source), Rc::new(SLVal::Int(42)));
  }

  #[test]
  fn if_selects_else_branch_when_false() {
    let source = "
      (use \"src/std\")
      (fn main () (if (std.== 1 2) 42 0))
    ";
    assert_eq!(eval_main(source), Rc::new(SLVal::Int(0)));
  }

  #[test]
  fn if_does_not_evaluate_unselected_branch() {
    // If the else branch were evaluated, calling the declared-but-unimplemented
    // `boom` builtin would error at runtime. Since the then branch is selected,
    // `boom` is never called and the program returns 42 cleanly.
    let source = "
      (use \"src/std\")
      (decl boom (n))
      (fn main () (if (std.== 1 1) 42 (boom 0)))
    ";
    assert_eq!(eval_main(source), Rc::new(SLVal::Int(42)));
  }

  #[test]
  fn bool_literals_evaluate() {
    let source = "(fn main () true)";
    assert_eq!(eval_main(source), Rc::new(SLVal::Bool(true)));
    let source = "(fn main () false)";
    assert_eq!(eval_main(source), Rc::new(SLVal::Bool(false)));
  }

  #[test]
  fn equality_returns_bool() {
    let source = "
      (use \"src/std\")
      (fn main () (std.== 3 3))
    ";
    assert_eq!(eval_main(source), Rc::new(SLVal::Bool(true)));
    let source = "
      (use \"src/std\")
      (fn main () (std.== 3 4))
    ";
    assert_eq!(eval_main(source), Rc::new(SLVal::Bool(false)));
  }

  #[test]
  fn if_rejects_non_bool_condition() {
    let source = "(fn main () (if 1 42 0))";
    let pkg = compile_executable_from_source(&source.to_string(), ("main", "main")).unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let err = exec.run_until_done().unwrap_err();
    assert_eq!(err, "`if` condition must be a bool, got Int(1)");
  }

  #[test]
  fn run_completes_in_one_call() {
    let source = "(fn main () 5)";
    let pkg = compile_executable_from_source(&source.to_string(), ("main", "main")).unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let status = exec.run(1_000).unwrap();
    assert_eq!(status, Status::Done(Rc::new(SLVal::Int(5))));
  }

  #[test]
  fn run_pauses_and_resumes() {
    let source = "
      (fn main ()
        (let a 1) (let b 2) (let c 3) (let d 4) 5)";
    let pkg = compile_executable_from_source(&source.to_string(), ("main", "main")).unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let status = exec.run(3).unwrap();
    assert_eq!(status, Status::Paused);
    assert_eq!(exec.executed, 3);
    assert!(!exec.is_done());
    let status = exec.run(1_000).unwrap();
    assert_eq!(status, Status::Done(Rc::new(SLVal::Int(5))));
  }

  #[test]
  fn run_hits_limit_exactly() {
    // (std.+ 1 2) is 4 bytecodes: PushInt(1), PushInt(2), Call(std.+), Return.
    let source = "(use \"src/std\") (fn main () (std.+ 1 2))";
    let pkg = compile_executable_from_source(&source.to_string(), ("main", "main")).unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let status = exec.run(4).unwrap();
    assert_eq!(status, Status::Done(Rc::new(SLVal::Int(3))));
    assert_eq!(exec.executed, 4);
  }

  #[test]
  fn run_limited_mid_program_pauses() {
    let source = "(use \"src/std\") (fn main () (std.+ 1 2))";
    let pkg = compile_executable_from_source(&source.to_string(), ("main", "main")).unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let status = exec.run(2).unwrap();
    assert_eq!(status, Status::Paused);
    let status = exec.run(1_000).unwrap();
    assert_eq!(status, Status::Done(Rc::new(SLVal::Int(3))));
    assert_eq!(exec.executed, 4);
  }

  #[test]
  fn infinite_recursion_is_bounded_by_run_budget() {
    let source = "(fn loop (n) (loop n)) (fn main () (loop 1))";
    let pkg = compile_executable_from_source(&source.to_string(), ("main", "main")).unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let status = exec.run(10_000).unwrap();
    assert_eq!(status, Status::Paused);
    assert!(!exec.is_done());
  }

  #[test]
  fn executed_count_is_cumulative() {
    let source = "(fn main () (let a 1) (let b 2) (let c 3) 4)";
    let pkg = compile_executable_from_source(&source.to_string(), ("main", "main")).unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    exec.run(2).unwrap();
    assert_eq!(exec.executed, 2);
    exec.run(2).unwrap();
    assert_eq!(exec.executed, 4);
    let s3 = exec.run(1_000).unwrap();
    assert!(matches!(s3, Status::Done(_)));
    assert_eq!(exec.executed, 14);
  }

  #[test]
  fn two_executions_run_in_parallel() {
    // Two `Execution`s from one `Interpreter` should be independent: stepping
    // one does not affect the other's stack, frames, or counter. We interleave
    // `run` calls (cooperative scheduling) and confirm each produces its own
    // result. Each `main` returns its distinct constant so we can tell them
    // apart.
    let source = "(fn main () (let a 1) (let b 2) 3)";
    let pkg = compile_executable_from_source(&source.to_string(), ("main", "main")).unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec_a = interp.call_main().unwrap();
    let mut exec_b = interp.call_main().unwrap();

    // Run a couple of bytecodes on each, alternating, before either is done.
    assert_eq!(exec_a.run(1).unwrap(), Status::Paused);
    assert_eq!(exec_b.run(1).unwrap(), Status::Paused);
    assert_eq!(exec_a.executed, 1);
    assert_eq!(exec_b.executed, 1);

    // Run both to completion in the same interleaved fashion.
    assert_eq!(exec_a.run(1_000).unwrap(), Status::Done(Rc::new(SLVal::Int(3))));
    assert_eq!(exec_b.run(1_000).unwrap(), Status::Done(Rc::new(SLVal::Int(3))));
    assert!(exec_a.is_done());
    assert!(exec_b.is_done());
  }

  #[test]
  fn run_for_duration_completes() {
    // A trivial program completes well within a generous duration.
    let source = "(fn main () 5)";
    let pkg = compile_executable_from_source(&source.to_string(), ("main", "main")).unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let status = exec
      .run_for_duration(Duration::from_secs(1))
      .unwrap();
    assert_eq!(status, Status::Done(Rc::new(SLVal::Int(5))));
  }

  #[test]
  fn run_for_duration_pauses_on_infinite_recursion() {
    // A non-terminating program is bounded by the time budget and pauses.
    let source = "(fn loop (n) (loop n)) (fn main () (loop 1))";
    let pkg = compile_executable_from_source(&source.to_string(), ("main", "main")).unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let status = exec
      .run_for_duration(Duration::from_millis(50))
      .unwrap();
    assert_eq!(status, Status::Paused);
    assert!(!exec.is_done());
    // It should have made progress.
    assert!(exec.executed > 0);
  }
}
