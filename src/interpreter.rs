use std::time::{Duration, Instant};

use gc_arena::{collect::Collect, Arena, Gc, Mutation, RefLock, Rootable};

use crate::builtins::{default_builtins, Builtins};
use crate::compiler::{Callable, Instruction, LinkedFunction as Function, Package};

/// Per-execution state held inside an `Execution`'s own arena. Each `Execution`
/// owns a private `Arena` whose root is this type. All interpreter logic lives
/// as methods on `ExecRoot`, so that stepping a single bytecode or running to
/// completion can call `self.step(mc, …)`, `self.pop()`, `self.is_done()`, etc.
/// — the same shape the original `Rc`-based `Execution` had, just branded with
/// the gc-arena invariant `'gc` lifetime so that `Gc` pointers never escape a
/// single `mutate_root` callback.
#[derive(Collect, Default)]
#[collect(no_drop)]
struct ExecRoot<'gc> {
  stack: Vec<Gc<'gc, SLVal<'gc>>>,
  frames: Vec<Frame<'gc>>,
}

/// A stack frame. The function is stored by value (cloned on entry) so that
/// the frame is self-contained and can live inside the arena root without
/// borrowing the `Package`.
#[derive(Collect, Clone)]
#[collect(no_drop)]
struct Frame<'gc> {
  function: Function,
  locals: Vec<Gc<'gc, SLVal<'gc>>>,
  ip: usize,
}

// `Function` (a.k.a. `LinkedFunction`) is `'static` and holds no `Gc` pointers,
// so it gets a trivial, empty `Collect` impl.
gc_arena::static_collect!(Function);

/// A SafeLisp value. This is the in-arena representation: anything that needs
/// sharing or cycle-detection is held behind a `Gc` pointer.
#[derive(Debug, PartialEq, Clone, Collect)]
#[collect(no_drop)]
pub enum SLVal<'gc> {
  Int(i64),
  Float(f64),
  String(String),
  Bool(bool),
  Void,
  FunctionRef(u32, u32),
  Partial(Partial<'gc>),
  Cell(Gc<'gc, RefLock<SLVal<'gc>>>),
  List(Vec<Gc<'gc, SLVal<'gc>>>),
}

#[derive(Debug, PartialEq, Clone, Collect)]
#[collect(no_drop)]
pub struct Partial<'gc> {
  function: (u32, u32),
  args: Vec<Gc<'gc, SLVal<'gc>>>,
}

/// An owned, arena-agnostic value that can escape the arena and be fed back
/// into any other (or the same) execution. Non-scalar values are represented
/// recursively with owned `SLValue` sub-values, so converting across the arena
/// boundary performs a deep copy: `Gc` pointers in one arena are unboxed into
/// owned values and re-boxed into fresh `Gc` pointers in the destination arena.
#[derive(Debug, PartialEq, Clone)]
pub enum SLValue {
  Int(i64),
  Float(f64),
  String(String),
  Bool(bool),
  Void,
  FunctionRef(u32, u32),
  Partial {
    function: (u32, u32),
    args: Vec<SLValue>,
  },
  Cell(Box<SLValue>),
  List(Vec<SLValue>),
}

impl<'gc> SLVal<'gc> {
  /// Convert an `SLVal` into an owned `SLValue`, deep-copying any `Gc`-held
  /// sub-values out of the arena.
  fn to_value(&self) -> SLValue {
    match self {
      SLVal::Int(i) => SLValue::Int(*i),
      SLVal::Float(x) => SLValue::Float(*x),
      SLVal::String(s) => SLValue::String(s.clone()),
      SLVal::Bool(b) => SLValue::Bool(*b),
      SLVal::Void => SLValue::Void,
      SLVal::FunctionRef(m, n) => SLValue::FunctionRef(*m, *n),
      SLVal::Partial(p) => SLValue::Partial {
        function: p.function,
        args: p.args.iter().map(|a| a.to_value()).collect(),
      },
      SLVal::Cell(r) => SLValue::Cell(Box::new(r.borrow().to_value())),
      SLVal::List(items) => SLValue::List(items.iter().map(|i| i.to_value()).collect()),
    }
  }

  /// Reconstruct a `Gc<SLVal>` inside the arena from an owned `SLValue`,
  /// allocating fresh `Gc` pointers for any non-scalar sub-values.
  fn from_value(mc: &Mutation<'gc>, value: &SLValue) -> Gc<'gc, SLVal<'gc>> {
    Gc::new(
      mc,
      match value {
        SLValue::Int(i) => SLVal::Int(*i),
        SLValue::Float(x) => SLVal::Float(*x),
        SLValue::String(s) => SLVal::String(s.clone()),
        SLValue::Bool(b) => SLVal::Bool(*b),
        SLValue::Void => SLVal::Void,
        SLValue::FunctionRef(m, n) => SLVal::FunctionRef(*m, *n),
        SLValue::Partial { function, args } => SLVal::Partial(Partial {
          function: *function,
          args: args.iter().map(|a| SLVal::from_value(mc, a)).collect(),
        }),
        SLValue::Cell(inner) => SLVal::Cell(Gc::new(
          mc,
          RefLock::new(SLVal::from_value(mc, inner).as_ref().clone()),
        )),
        SLValue::List(items) => {
          SLVal::List(items.iter().map(|i| SLVal::from_value(mc, i)).collect())
        }
      },
    )
  }

  /// Produce a `Gc<SLVal>` for this value. Used when deref-ing a cell: the
  /// contents come out as a borrowed `SLVal`, and we re-box it onto the GC
  /// heap so it can go back on the stack.
  fn clone_value(&self, mc: &Mutation<'gc>) -> Gc<'gc, SLVal<'gc>> {
    Gc::new(mc, self.clone())
  }
}

pub struct Interpreter {
  package: Package,
  builtins: Builtins,
}

impl Interpreter {
  pub fn with_builtins(package: Package, builtins: Builtins) -> Self {
    Interpreter { package, builtins }
  }

  /// Construct an interpreter that will run with [`default_builtins`].
  pub fn new(package: Package) -> Self {
    Interpreter::with_builtins(package, default_builtins())
  }
}

impl Interpreter {
  /// Set up an `Execution` ready to run `main`.
  pub fn call_main(&self) -> Result<Execution, String> {
    if let Some((module, function)) = self.package.main {
      let callable = self
        .package
        .get_function(module, function)
        .ok_or_else(|| format!("Couldn't find module {} function {}", module, function))?;
      if let Callable::Function(function) = callable {
        let mut exec = Execution::new(self.package.clone(), self.builtins.clone());
        exec.enter_function(function.clone(), vec![])?;
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

  /// Set up an `Execution` ready to call `slval`. The `SLValue` is deep-copied
  /// into the new execution's arena, so a value produced by one execution can
  /// be fed into any other execution (or the same one).
  pub fn call_slval(&self, slval: SLValue) -> Result<Execution, String> {
    let mut exec = Execution::new(self.package.clone(), self.builtins.clone());
    exec.push_and_call_dynamic(slval)?;
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
  Done(SLValue),
}

/// A single, independent SafeLisp execution. Each `Execution` owns its own
/// garbage-collected `Arena`, so multiple executions are fully independent
/// and may be run in parallel (cooperatively interleaved by the caller). The
/// `Arena` holds the value stack and call frames branded with an invariant
/// `'gc` lifetime; values that must cross the arena boundary (such as the
/// result of `run`) are deep-copied to the arena-agnostic `SLValue`.
pub struct Execution {
  arena: Arena<Rootable![ExecRoot<'_>]>,
  package: Package,
  builtins: Builtins,
  pub executed: u64,
}

impl Execution {
  pub fn new(package: Package, builtins: Builtins) -> Self {
    let arena = Arena::new(|_mc| ExecRoot {
      stack: Vec::new(),
      frames: Vec::new(),
    });
    Execution {
      arena,
      package,
      builtins,
      executed: 0,
    }
  }

  /// Returns true if the frame stack is empty (execution is complete).
  pub fn is_done(&self) -> bool {
    self.arena.mutate(|_, root| root.is_done())
  }

  /// The number of items currently on the value stack.
  pub fn stack_len(&self) -> usize {
    self.arena.mutate(|_, root| root.stack.len())
  }

  /// Peek the top of the value stack as an owned `SLValue`. Returns an error if
  /// the stack is empty.
  pub fn peek_value(&self) -> Result<SLValue, String> {
    let mut result = Err("PEEK on an empty stack".to_string());
    self.arena.mutate(|_, root| {
      if let Some(top) = root.stack.last().copied() {
        result = Ok(top.to_value());
      }
    });
    result
  }

  /// The current number of live `Gc` allocations in this execution's arena.
  /// Useful for tests that want to observe garbage collection.
  pub fn gc_count(&self) -> usize {
    self.arena.metrics().total_gc_count()
  }

  /// The total bytes currently allocated by live `Gc` pointers in this
  /// execution's arena.
  pub fn gc_allocation_bytes(&self) -> usize {
    self.arena.metrics().total_gc_allocation()
  }

  /// Force a full garbage-collection cycle, freeing all unreachable `Gc`
  /// pointers. Useful for tests that want to verify that collection reclaims
  /// garbage.
  pub fn collect_all(&mut self) {
    self.arena.finish_cycle();
  }

  /// Run up to `n` bytecodes. Returns `Paused` if the budget exhausted before
  /// completion, or `Done(v)` if the program completed. Errors propagate via
  /// `Err`. The cumulative count of executed bytecodes is available on
  /// `Execution::executed` after the call returns.
  pub fn run(&mut self, n: u64) -> Result<Status, String> {
    let start = self.executed;
    let mut executed = self.executed;
    let outcome = self
      .arena
      .mutate_root(|mc, root| root.run(mc, &self.package, &self.builtins, start, n, &mut executed));
    self.executed = executed;
    // Run a bit of incremental collection, paced by allocation debt.
    self.arena.collect_debt();
    outcome
  }

  /// Convenience: run to completion with no instruction limit. Returns the
  /// final value, or errors on a runtime error.
  pub fn run_until_done(&mut self) -> Result<SLValue, String> {
    let mut executed = self.executed;
    let result = self.arena.mutate_root(|mc, root| {
      root.run_until_done(mc, &self.package, &self.builtins, &mut executed)
    });
    self.executed = executed;
    self.arena.collect_debt();
    result
  }

  /// Run for up to `duration`, stepping until the deadline is reached or
  /// execution completes. Returns `Paused` if the deadline expired before
  /// completion, or `Done(v)` if the program completed. Errors propagate via
  /// `Err`. The cumulative count of executed bytecodes is available on
  /// `Execution::executed` after the call returns.
  pub fn run_for_duration(&mut self, duration: Duration) -> Result<Status, String> {
    let deadline = Instant::now() + duration;
    let mut executed = self.executed;
    let outcome = self.arena.mutate_root(|mc, root| {
      root.run_for_duration(mc, &self.package, &self.builtins, deadline, &mut executed)
    });
    self.executed = executed;
    self.arena.collect_debt();
    outcome
  }

  /// Execute one bytecode instruction from the current top frame.
  pub fn step(&mut self) -> Result<(), String> {
    let mut executed = self.executed;
    let result = self
      .arena
      .mutate_root(|mc, root| root.step(mc, &self.package, &self.builtins, &mut executed));
    self.executed = executed;
    self.arena.collect_debt();
    result
  }

  /// Push a frame for a function, with optional pre-bound values given as owned
  /// `SLValue`s (used by external/test entry points). The remaining params
  /// (after the pre-bound ones) are popped from the stack.
  pub fn enter_function(
    &mut self,
    function: Function,
    pre_bound: Vec<SLValue>,
  ) -> Result<(), String> {
    let result = self.arena.mutate_root(|mc, root| {
      let pre_bound_gc: Vec<Gc<'_, SLVal<'_>>> =
        pre_bound.iter().map(|v| SLVal::from_value(mc, v)).collect();
      root.enter_function(mc, function.clone(), pre_bound_gc)
    });
    result
  }

  /// Push a value onto the stack and invoke `call_dynamic` (used to set up an
  /// execution from an external `SLValue`).
  fn push_and_call_dynamic(&mut self, value: SLValue) -> Result<(), String> {
    let package = self.package.clone();
    let builtins = self.builtins.clone();
    let result = self.arena.mutate_root(|mc, root| {
      let gc = SLVal::from_value(mc, &value);
      root.stack.push(gc);
      root.call_dynamic(mc, &package, &builtins, 0)
    });
    result
  }
}

impl<'gc> ExecRoot<'gc> {
  /// Returns true if the frame stack is empty (execution is complete).
  fn is_done(&self) -> bool {
    self.frames.is_empty()
  }

  /// Pop a value off the execution's value stack.
  fn pop(&mut self) -> Result<Gc<'gc, SLVal<'gc>>, String> {
    self
      .stack
      .pop()
      .ok_or_else(|| "POP on an empty stack".to_string())
  }

  /// Run up to `n` bytecodes starting from `start` executed count. Returns
  /// `Paused` if the budget exhausted before completion, or `Done(v)` if the
  /// program completed. Errors propagate via `Err`.
  fn run(
    &mut self,
    mc: &'gc Mutation<'gc>,
    package: &Package,
    builtins: &Builtins,
    start: u64,
    n: u64,
    executed: &mut u64,
  ) -> Result<Status, String> {
    while !self.is_done() && *executed - start < n {
      self.step(mc, package, builtins, executed)?;
    }
    if self.is_done() {
      let top = self.pop()?;
      Ok(Status::Done(top.to_value()))
    } else {
      Ok(Status::Paused)
    }
  }

  /// Run to completion with no instruction limit. Returns the final value, or
  /// errors on a runtime error.
  fn run_until_done(
    &mut self,
    mc: &'gc Mutation<'gc>,
    package: &Package,
    builtins: &Builtins,
    executed: &mut u64,
  ) -> Result<SLValue, String> {
    while !self.is_done() {
      self.step(mc, package, builtins, executed)?;
    }
    let top = self.pop()?;
    Ok(top.to_value())
  }

  /// Run until `deadline` is reached or execution completes. Returns `Paused`
  /// if the deadline expired before completion, or `Done(v)` if the program
  /// completed. Errors propagate via `Err`.
  fn run_for_duration(
    &mut self,
    mc: &'gc Mutation<'gc>,
    package: &Package,
    builtins: &Builtins,
    deadline: Instant,
    executed: &mut u64,
  ) -> Result<Status, String> {
    while !self.is_done() && Instant::now() < deadline {
      self.step(mc, package, builtins, executed)?;
    }
    if self.is_done() {
      let top = self.pop()?;
      Ok(Status::Done(top.to_value()))
    } else {
      Ok(Status::Paused)
    }
  }

  /// Execute one bytecode instruction from the current top frame.
  fn step(
    &mut self,
    mc: &'gc Mutation<'gc>,
    package: &Package,
    builtins: &Builtins,
    executed: &mut u64,
  ) -> Result<(), String> {
    *executed = executed.saturating_add(1);
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
      inst.clone()
    };

    match inst {
      Instruction::PushInt(i) => self.stack.push(Gc::new(mc, SLVal::Int(i))),
      Instruction::PushFloat(f) => self.stack.push(Gc::new(mc, SLVal::Float(f))),
      Instruction::PushString(s) => self.stack.push(Gc::new(mc, SLVal::String(s))),
      Instruction::PushBool(b) => self.stack.push(Gc::new(mc, SLVal::Bool(b))),
      Instruction::Pop => {
        self.pop()?;
      }
      Instruction::Return => {
        // Pop the top frame; the return value stays on self.stack.
        self.frames.pop();
      }
      Instruction::SetLocal(i) => {
        let val = self.pop()?;
        let frame = self
          .frames
          .last_mut()
          .ok_or_else(|| "SetLocal with no frame".to_string())?;
        frame.locals[usize::from(i)] = val;
      }
      Instruction::LoadLocal(i) => {
        let frame = self
          .frames
          .last()
          .ok_or_else(|| "LoadLocal with no frame".to_string())?;
        self.stack.push(frame.locals[usize::from(i)]);
      }
      Instruction::Call((mod_index, func_index), arity) => {
        self.call_fixed(mc, package, builtins, mod_index, func_index, arity)?;
      }
      Instruction::CallDynamic(arity) => {
        self.call_dynamic(mc, package, builtins, arity)?;
      }
      Instruction::MakeFunctionRef((mod_index, func_index)) => {
        self
          .stack
          .push(Gc::new(mc, SLVal::FunctionRef(mod_index, func_index)));
      }
      Instruction::MakeCell => {
        let val = self.pop()?;
        self.stack.push(Gc::new(
          mc,
          SLVal::Cell(Gc::new(mc, RefLock::new((*val).clone()))),
        ));
      }
      Instruction::DerefCell => {
        let val = self.pop()?;
        match &*val {
          SLVal::Cell(r) => self.stack.push(r.borrow().clone_value(mc)),
          other => return Err(format!("Not a cell: {:?}", other)),
        }
      }
      Instruction::SetCell => {
        // Pop the Cell (TOS) and the new value (below it), write the value
        // into the Cell, and push the value back as the result of `set!`.
        let cell_gc = self.pop()?;
        let new_val = self.pop()?;
        match &*cell_gc {
          SLVal::Cell(cell_ref) => {
            // Mutate the Cell's contents in a GC-safe way.
            *Gc::write(mc, *cell_ref).unlock().borrow_mut() = (*new_val).clone();
          }
          other => return Err(format!("Not a cell: {:?}", other)),
        }
        self.stack.push(new_val);
      }
      Instruction::PartialApply(num_args) => {
        self.partial_apply(mc, num_args)?;
      }
      Instruction::Jump(offset) => {
        let frame = self
          .frames
          .last_mut()
          .ok_or_else(|| "Jump with no frame".to_string())?;
        // `offset` is relative to the instruction following the `Jump` (the IP
        // has already been advanced past it), so `Jump(0)` is a no-op.
        frame.ip = frame.ip.wrapping_add(offset as usize);
      }
      Instruction::JumpIfFalse(offset) => {
        let val = self.pop()?;
        match &*val {
          SLVal::Bool(false) => {
            let frame = self
              .frames
              .last_mut()
              .ok_or_else(|| "JumpIfFalse with no frame".to_string())?;
            frame.ip = frame.ip.wrapping_add(offset as usize);
          }
          SLVal::Bool(true) => {}
          other => return Err(format!("`if` condition must be a bool, got {:?}", other)),
        }
      }
    }
    Ok(())
  }

  fn partial_apply(&mut self, mc: &'gc Mutation<'gc>, num_args: u16) -> Result<(), String> {
    let func = self.pop()?;
    let mut args = vec![];
    for _ in 0..num_args {
      args.push(self.pop()?);
    }
    args.reverse();
    let closure = match &*func {
      SLVal::FunctionRef(mod_index, func_index) => Gc::new(
        mc,
        SLVal::Partial(Partial {
          function: (*mod_index, *func_index),
          args,
        }),
      ),
      _ => return Err("make_closure needs a function at TOS".to_string()),
    };
    self.stack.push(closure);
    Ok(())
  }

  /// Either pushes a frame for the function if it's defined in SafeLisp, or
  /// just call it immediately if it's a builtin. `arity` is the number of args
  /// pushed at the call site (carried on [`Instruction::Call`]); it is used to
  /// pop the right number of args for the callee and to arity-check at the call
  /// site. For variadic builtins (whose `BuiltinSpec::num_params` is `None`) the
  /// arity is the only source of the arg count.
  fn call_fixed(
    &mut self,
    mc: &'gc Mutation<'gc>,
    package: &Package,
    builtins: &Builtins,
    mod_index: u32,
    func_index: u32,
    arity: u16,
  ) -> Result<(), String> {
    let (module_name, functions) = package
      .get_module(mod_index)
      .ok_or_else(|| format!("Module not found: {}", mod_index))?;
    let (func_name, callable) = functions
      .get(func_index as usize)
      .ok_or_else(|| format!("Function not found: {}/{}", mod_index, func_index))?;
    match callable {
      Callable::Function(func) => {
        if arity != func.num_params {
          return Err(format!(
            "{}.{} expects {} arg(s) but was called with {}",
            module_name, func_name, func.num_params, arity
          ));
        }
        self.enter_function(mc, func.clone(), vec![])?;
      }
      Callable::Builtin => {
        let builtin = builtins
          .lookup(module_name, func_name)
          .ok_or_else(|| format!("No builtin {}.{}", module_name, func_name))?;
        // For fixed-arity builtins, enforce the declared arity at the call site.
        if let Some(expected) = builtin.spec().num_params {
          if arity != expected {
            return Err(format!(
              "{}.{} expects {} arg(s) but was called with {}",
              module_name, func_name, expected, arity
            ));
          }
        }
        // For variadic builtins (`num_params == None`) the call-site arity is
        // the only source of the arg count.
        let n = arity as usize;
        // Args were pushed left-to-right; popping gives reverse order.
        let mut args = Vec::with_capacity(n);
        for _ in 0..n {
          args.push(self.pop()?);
        }
        args.reverse();
        let result = builtin.call(&args)?;
        self.stack.push(Gc::new(mc, result));
      }
    }
    Ok(())
  }

  /// Prepare the callable that's on the top of stack to be called. This pushes
  /// a new frame for function callables. This is both the implementation of the
  /// `CallDynamic` instruction and the entry point used by
  /// `Interpreter::call_slval`. `arity` is the number of args pushed at the
  /// call site (carried on [`Instruction::CallDynamic`]); see [`Self::call_fixed`].
  fn call_dynamic(
    &mut self,
    mc: &'gc Mutation<'gc>,
    package: &Package,
    builtins: &Builtins,
    arity: u16,
  ) -> Result<(), String> {
    let callable = self.pop()?;
    match &*callable {
      SLVal::FunctionRef(mod_index, func_index) => {
        self.call_fixed(mc, package, builtins, *mod_index, *func_index, arity)
      }
      SLVal::Partial(Partial {
        function: (mod_index, func_index),
        args,
      }) => {
        let (_, functions) = package
          .get_module(*mod_index)
          .ok_or_else(|| format!("Module not found: {}", mod_index))?;
        let (func_name, callable) = functions
          .get(*func_index as usize)
          .ok_or_else(|| format!("Function not found: {}/{}", mod_index, func_index))?;
        match callable {
          Callable::Function(func) => {
            // The remaining params (after the pre-bound ones) must be supplied
            // by the call site; arity-check that they line up.
            let expected = func.num_params.saturating_sub(args.len() as u16);
            if arity != expected {
              return Err(format!(
                "{}.{} expects {} more arg(s) but was called with {}",
                mod_index, func_name, expected, arity
              ));
            }
            self.enter_function(mc, func.clone(), args.clone())
          }
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
    mc: &Mutation<'gc>,
    function: Function,
    pre_bound: Vec<Gc<'gc, SLVal<'gc>>>,
  ) -> Result<(), String> {
    let start = pre_bound.len();
    let mut locals = pre_bound;
    // Initialize all remaining local slots to `Void`. `SLVal::Void` holds no
    // `Gc` pointers, so allocating it on the GC heap is cheap and safe.
    let void = Gc::new(mc, SLVal::Void);
    for _ in locals.len()..usize::from(function.num_locals) {
      locals.push(void);
    }
    // The parameters are "in order" on the stack, so popping will give them to
    // us in reverse order.
    for param_idx in (start..usize::from(function.num_params)).rev() {
      locals[param_idx] = self.pop()?;
    }
    self.frames.push(Frame {
      function,
      locals,
      ip: 0,
    });
    Ok(())
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::builtins::{default_builtins, Builtin, Builtins};
  use crate::compiler::{self, *};
  use std::time::Duration;

  fn eval_main(source: &str) -> SLValue {
    eval_main_with(source, default_builtins())
  }

  /// Like `eval_main`, but with a custom builtin registry.
  fn eval_main_with(source: &str, builtins: Builtins) -> SLValue {
    let pkg =
      compile_executable_from_source(&source.to_string(), ("main", "main"), &builtins.specs())
        .unwrap();
    let interp = Interpreter::with_builtins(pkg, builtins);
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
        Instruction::Call((0, 0), 1),
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
    assert_eq!(exec.run_until_done().unwrap(), SLValue::Int(42));
  }

  #[test]
  fn std_eq_strings() {
    assert_eq!(
      eval_main("(fn main () (std.== \"abc\" \"abc\"))"),
      SLValue::Bool(true)
    );
    assert_eq!(
      eval_main("(fn main () (std.== \"abc\" \"abd\"))"),
      SLValue::Bool(false)
    );
  }

  #[test]
  fn std_concat_strings() {
    assert_eq!(
      eval_main("(fn main () (std.concat \"foo\" \"bar\"))"),
      SLValue::String("foobar".to_string())
    );
    assert_eq!(
      eval_main("(fn main () (std.concat \"ab\" \"CDE\"))"),
      SLValue::String("abCDE".to_string())
    );
    assert_eq!(
      eval_main("(fn main () (std.concat \"\" \"x\"))"),
      SLValue::String("x".to_string())
    );
  }

  #[test]
  fn extending_builtins() {
    let builtins = Builtins::new().with_builtin(Builtin::unary("main", "add2", |a| match &*a {
      SLVal::Int(n) => Ok(SLVal::Int(n + 2)),
      other => Err(format!("nope: {:?}", other)),
    }));
    let source = "(fn main () (add2 3))".to_string();
    let package =
      compile_executable_from_source(&source, ("main", "main"), &builtins.specs()).unwrap();

    let interpreter = Interpreter::with_builtins(package, builtins);
    let mut exec = interpreter.call_main().unwrap();
    assert_eq!(exec.run_until_done().unwrap(), SLValue::Int(5));
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
    assert_eq!(result2, SLValue::Int(42));
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
    let pkg =
      compile_executable_from_source(&source, ("main", "main"), &default_builtins().specs())
        .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    assert_eq!(exec.run_until_done().unwrap(), SLValue::Int(1));
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
    assert_eq!(eval_main(source), SLValue::Int(1));
  }

  #[test]
  fn closure_captures_outer_parameter() {
    let source = "
      (fn outer (a)
        (fn inner () a)
        inner)
      (fn main () ((outer 7)))
    ";
    assert_eq!(eval_main(source), SLValue::Int(7));
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
    assert_eq!(eval_main(source), SLValue::Int(1));
  }

  #[test]
  fn non_capturing_closure_end_to_end() {
    let source = "
      (fn outer ()
        (fn inner () 5)
        inner)
      (fn main () ((outer)))
    ";
    assert_eq!(eval_main(source), SLValue::Int(5));
  }

  #[test]
  fn local_nested_function_can_be_called() {
    let source = "
      (fn outer ()
        (fn inner () 5)
        (inner))
      (fn main () (outer))
    ";
    assert_eq!(eval_main(source), SLValue::Int(5));
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
    assert_eq!(eval_main(source), SLValue::Int(5));
  }

  #[test]
  fn function_definition_expression_returns_function() {
    let source = "
      (fn outer ()
        (fn inner () 5))
      (fn main () ((outer)))
    ";
    assert_eq!(eval_main(source), SLValue::Int(5));
  }

  #[test]
  fn step_advances_ip_and_leaves_value_on_stack() {
    let pkg = Package::default();
    let code = compiler::Function {
      num_locals: 0,
      num_params: 0,
      instructions: vec![Instruction::PushInt(7), Instruction::Return],
    };
    let mut exec = Execution::new(pkg, default_builtins());
    exec.enter_function(code, vec![]).unwrap();
    // After pushing the initial frame, there's one frame at ip 0.
    assert_eq!(exec.stack_len(), 0);
    exec.step().unwrap();
    // PushInt executed: stack has the value, frame still on the stack.
    assert_eq!(exec.peek_value().unwrap(), SLValue::Int(7));
    exec.step().unwrap();
    // Return popped the frame; the value remains on the stack.
    assert!(exec.is_done());
    assert_eq!(exec.peek_value().unwrap(), SLValue::Int(7));
    let result = exec.run_until_done().unwrap();
    assert_eq!(result, SLValue::Int(7));
  }

  #[test]
  fn run_until_done_pops_final_value() {
    let pkg = Package::default();
    let code = compiler::Function {
      num_locals: 0,
      num_params: 0,
      instructions: vec![Instruction::PushInt(99), Instruction::Return],
    };
    let mut exec = Execution::new(pkg, default_builtins());
    exec.enter_function(code, vec![]).unwrap();
    let result = exec.run_until_done().unwrap();
    assert_eq!(result, SLValue::Int(99));
    // The stack should be empty after run_until_done pops the final value.
    assert_eq!(exec.stack_len(), 0);
  }

  #[test]
  fn running_past_end_without_return_errors() {
    let pkg = Package::default();
    let code = compiler::Function {
      num_locals: 0,
      num_params: 0,
      instructions: vec![Instruction::PushInt(1)],
    };
    let mut exec = Execution::new(pkg, default_builtins());
    exec.enter_function(code, vec![]).unwrap();
    exec.step().unwrap(); // PushInt
    let err = exec.step().unwrap_err();
    assert!(err.contains("ran past end"), "unexpected error: {}", err);
  }

  #[test]
  fn step_with_no_frames_errors() {
    let pkg = Package::default();
    let mut exec = Execution::new(pkg, default_builtins());
    let err = exec.step().unwrap_err();
    assert!(err.contains("no frames"), "unexpected error: {}", err);
  }

  #[test]
  fn call_dynamic_on_non_callable_errors() {
    let pkg = Package::default();
    let mut exec = Execution::new(pkg, default_builtins());
    let err = exec
      .push_and_call_dynamic(SLValue::Int(3))
      .expect_err("expected an error calling a non-callable");
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
    let mut exec = Execution::new(pkg, default_builtins());
    exec.enter_function(code, vec![]).unwrap();
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
      instructions: vec![Instruction::Call((0, 0), 0)],
    };
    let mut exec = Execution::new(pkg, default_builtins());
    exec.enter_function(code, vec![]).unwrap();
    let err = exec.step().unwrap_err();
    assert!(
      err.contains("Module not found"),
      "unexpected error: {}",
      err
    );
  }

  /// A dummy variadic builtin used to exercise the variadic-call mechanism:
  /// it returns the number of arguments it received as an `Int`.
  fn varargs_builtins() -> Builtins {
    default_builtins().with_builtin(Builtin::variadic("std", "varargs", |args| {
      Ok(SLVal::Int(args.len() as i64))
    }))
  }

  #[test]
  fn variadic_builtin_called_with_zero_args() {
    let builtins = varargs_builtins();
    assert_eq!(
      eval_main_with("(fn main () (std.varargs))", builtins),
      SLValue::Int(0)
    );
  }

  #[test]
  fn variadic_builtin_called_with_multiple_args() {
    let builtins = varargs_builtins();
    assert_eq!(
      eval_main_with("(fn main () (std.varargs 1 2 3))", builtins),
      SLValue::Int(3)
    );
  }

  #[test]
  fn variadic_builtin_called_with_one_arg() {
    let builtins = varargs_builtins();
    assert_eq!(
      eval_main_with("(fn main () (std.varargs 7))", builtins),
      SLValue::Int(1)
    );
  }

  #[test]
  fn fixed_arity_builtin_rejects_wrong_arg_count() {
    // `+` is a fixed binary builtin; calling it with one arg should now be
    // caught at the call site via the carried arity.
    let src = "(fn main () (std.+ 1))";
    let pkg = compile_executable_from_source(
      &src.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let err = exec.run_until_done().unwrap_err();
    assert!(
      err.contains("expects 2 arg(s) but was called with 1"),
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
        Instruction::CallDynamic(0),
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
    assert_eq!(exec.run_until_done().unwrap(), SLValue::Int(42));
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
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let (mod_idx, fn_idx) = pkg.main.unwrap();
    let function = pkg.get_function(mod_idx, fn_idx).unwrap().clone();
    if let Callable::Function(function) = function {
      let mut exec = Execution::new(pkg, default_builtins());
      exec.enter_function(function, vec![]).unwrap();
      let result = exec.run_until_done().unwrap();
      assert_eq!(result, SLValue::Int(2));
      // run_until_done pops the final value, so the stack should be empty.
      assert_eq!(exec.stack_len(), 0);
    }
  }

  #[test]
  fn if_does_not_evaluate_unselected_branch() {
    // If the else branch were evaluated, calling the unimplemented
    // `boom` builtin would error at runtime. Since the then branch is selected,
    // `boom` is never called and the program returns 42 cleanly. `boom` is
    // registered as a builtin (with an erroring handler) so the call links,
    // but its behavior is never invoked.
    let source = "(fn main () (if (std.== 1 1) 42 (boom 0)))";
    let builtins =
      default_builtins().with_builtin(Builtin::unary("main", "boom", |_| Err("boom".into())));
    let pkg = compile_executable_from_source(source, ("main", "main"), &builtins.specs()).unwrap();
    let interp = Interpreter::with_builtins(pkg, builtins);
    let mut exec = interp.call_main().unwrap();
    assert_eq!(exec.run_until_done().unwrap(), SLValue::Int(42));
  }

  #[test]
  fn if_rejects_non_bool_condition() {
    let source = "(fn main () (if 1 42 0))";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let err = exec.run_until_done().unwrap_err();
    assert_eq!(err, "`if` condition must be a bool, got Int(1)");
  }

  #[test]
  fn run_completes_in_one_call() {
    let source = "(fn main () 5)";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let status = exec.run(1_000).unwrap();
    assert_eq!(status, Status::Done(SLValue::Int(5)));
  }

  #[test]
  fn run_pauses_and_resumes() {
    let source = "
      (fn main ()
        (let a 1) (let b 2) (let c 3) (let d 4) 5)";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let status = exec.run(3).unwrap();
    assert_eq!(status, Status::Paused);
    assert_eq!(exec.executed, 3);
    assert!(!exec.is_done());
    let status = exec.run(1_000).unwrap();
    assert_eq!(status, Status::Done(SLValue::Int(5)));
  }

  #[test]
  fn run_hits_limit_exactly() {
    // (std.+ 1 2) is 4 bytecodes: PushInt(1), PushInt(2), Call(std.+), Return.
    let source = "(fn main () (std.+ 1 2))";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let status = exec.run(4).unwrap();
    assert_eq!(status, Status::Done(SLValue::Int(3)));
    assert_eq!(exec.executed, 4);
  }

  #[test]
  fn run_limited_mid_program_pauses() {
    let source = "(fn main () (std.+ 1 2))";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let status = exec.run(2).unwrap();
    assert_eq!(status, Status::Paused);
    let status = exec.run(1_000).unwrap();
    assert_eq!(status, Status::Done(SLValue::Int(3)));
    assert_eq!(exec.executed, 4);
  }

  #[test]
  fn infinite_recursion_is_bounded_by_run_budget() {
    let source = "(fn loop (n) (loop n)) (fn main () (loop 1))";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let status = exec.run(10_000).unwrap();
    assert_eq!(status, Status::Paused);
    assert!(!exec.is_done());
  }

  #[test]
  fn executed_count_is_cumulative() {
    let source = "(fn main () (let a 1) (let b 2) (let c 3) 4)";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
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
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec_a = interp.call_main().unwrap();
    let mut exec_b = interp.call_main().unwrap();

    // Run a couple of bytecodes on each, alternating, before either is done.
    assert_eq!(exec_a.run(1).unwrap(), Status::Paused);
    assert_eq!(exec_b.run(1).unwrap(), Status::Paused);
    assert_eq!(exec_a.executed, 1);
    assert_eq!(exec_b.executed, 1);

    // Run both to completion in the same interleaved fashion.
    assert_eq!(exec_a.run(1_000).unwrap(), Status::Done(SLValue::Int(3)));
    assert_eq!(exec_b.run(1_000).unwrap(), Status::Done(SLValue::Int(3)));
    assert!(exec_a.is_done());
    assert!(exec_b.is_done());
  }

  #[test]
  fn run_for_duration_completes() {
    // A trivial program completes well within a generous duration.
    let source = "(fn main () 5)";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let status = exec.run_for_duration(Duration::from_secs(1)).unwrap();
    assert_eq!(status, Status::Done(SLValue::Int(5)));
  }

  #[test]
  fn run_for_duration_pauses_on_infinite_recursion() {
    // A non-terminating program is bounded by the time budget and pauses.
    let source = "(fn loop (n) (loop n)) (fn main () (loop 1))";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let status = exec.run_for_duration(Duration::from_millis(50)).unwrap();
    assert_eq!(status, Status::Paused);
    assert!(!exec.is_done());
    // It should have made progress.
    assert!(exec.executed > 0);
  }

  #[test]
  fn collect_all_frees_unreachable_values() {
    // A program that creates garbage: `main` builds a Cell holding 42, then
    // discards it by returning `99`. After execution, the Cell (and its
    // contents) should be unreachable from the root. Forcing a full
    // collection cycle should reclaim those allocations.
    //
    // We construct this with bytecode directly because there's no surface
    // syntax for `cell` outside the closure transform.
    let main = compiler::Function {
      num_locals: 1,
      num_params: 0,
      instructions: vec![
        Instruction::PushInt(42),
        Instruction::MakeCell,    // allocate a Cell wrapping 42
        Instruction::SetLocal(0), // bind it to local 0 ("garbage")
        Instruction::PushInt(99), // push the real return value
        Instruction::Return,      // return 99; the Cell in local 0 is now dead
      ],
    };
    let pkg = Package {
      modules: vec![(
        "main".to_string(),
        vec![("main".to_string(), Callable::Function(main))],
      )],
      main: Some((0, 0)),
    };
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();

    // `call_main` enters the function, which allocates locals in the GC. Record
    // the baseline allocation count before running.
    let baseline = exec.gc_count();
    assert_eq!(baseline, 1); // the single Void local for `main`

    // Step through the program manually so we can observe allocations before
    // the final value is popped (which would make it unreachable). The program
    // is 5 instructions; stepping all 5 leaves the int `99` on the stack and
    // the Cell dead in local 0.
    for _ in 0..5 {
      exec.step().unwrap();
    }
    assert!(exec.is_done());
    assert_eq!(exec.peek_value().unwrap(), SLValue::Int(99));

    // After running, the Cell is unreachable but may not yet have been
    // collected (incremental collection difficult to predict because of the
    // "debt" system). There should be at least one live `Gc` allocation right
    // now (the dangling Cell + contents), plus the int `99` on the stack.
    let after_run = exec.gc_count();
    assert!(
      after_run >= 3,
      "expected at least 3 live Gc allocations (cell + inner value + result) after run, got {}",
      after_run,
    );

    // Forcing a full collection cycle must reclaim the unreachable Cell. The
    // int `99` on the stack is still reachable and survives.
    exec.collect_all();
    let after_collect = exec.gc_count();
    assert_eq!(
      after_collect, 1,
      "expected 1 live Gc allocation (the result 99) after collect_all, got {}",
      after_collect,
    );
  }

  #[test]
  fn gc_reclaims_intermediate_values_during_long_run() {
    // A program that produces a lot of garbage: it loops `n` times, each
    // iteration building a Cell and immediately discarding it. If the GC is
    // working, the live allocation count after the run should stay bounded
    // (proportional to the final stack, not to the number of iterations),
    // rather than growing linearly with `n`.
    //
    // loop(n): if n == 0 return 0; else let g = cell(n); loop(n-1)
    //   locals: n (0), g (1)
    //   0: LoadLocal(0)        # n
    //   1: PushInt(0)
    //   2: Call((std_mod, std_eq))  # n == 0
    //   3: JumpIfFalse(+2)      # if n != 0, skip the then-branch (offset 2)
    //   4: PushInt(0)
    //   5: Return
    //   6: LoadLocal(0)        # n
    //   7: MakeCell             # cell(n)  -- this is the garbage
    //   8: SetLocal(1)          # g = cell(n)
    //   9: LoadLocal(0)         # n
    //  10: PushInt(1)
    //  11: Call((std_mod, std_sub)) # n - 1
    //  12: Call((0, 0))         # loop(n-1)  -- tail-ish recursion
    //  13: Return
    let std_mod = 1u32; // "std" is the second module after "main"
    let std_eq = 2u32;
    let std_sub = 1u32;
    let waste = compiler::Function {
      num_locals: 2,
      num_params: 1,
      instructions: vec![
        Instruction::LoadLocal(0),
        Instruction::PushInt(0),
        Instruction::Call((std_mod, std_eq), 2),
        Instruction::JumpIfFalse(2),
        Instruction::PushInt(0),
        Instruction::Return,
        Instruction::LoadLocal(0),
        Instruction::MakeCell,
        Instruction::SetLocal(1),
        Instruction::LoadLocal(0),
        Instruction::PushInt(1),
        Instruction::Call((std_mod, std_sub), 2),
        Instruction::Call((0, 0), 1),
        Instruction::Return,
      ],
    };
    let main = compiler::Function {
      num_locals: 0,
      num_params: 0,
      instructions: vec![
        Instruction::PushInt(1000),
        Instruction::Call((0, 0), 1), // waste(1000)
        Instruction::Return,
      ],
    };
    // Build a package with std builtins and the two functions above.
    let pkg = Package {
      modules: vec![
        (
          "main".to_string(),
          vec![
            ("waste".to_string(), Callable::Function(waste)),
            ("main".to_string(), Callable::Function(main)),
          ],
        ),
        (
          "std".to_string(),
          vec![
            ("+".to_string(), Callable::Builtin),
            ("-".to_string(), Callable::Builtin),
            ("==".to_string(), Callable::Builtin),
          ],
        ),
      ],
      main: Some((0, 1)),
    };
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();

    let result = exec.run_until_done().unwrap();
    assert_eq!(result, SLValue::Int(0));

    // After 1000 iterations each allocating a Cell, if GC never ran we'd
    // expect ~1000+ live allocations. Because `run_until_done` calls
    // `collect_debt` and the program allocates enough to trigger incremental
    // collection along the way, the count should be far smaller than 1000.
    // Forcing a final collection reclaims whatever transient garbage remains,
    // leaving only the final stack value.
    exec.collect_all();
    let live = exec.gc_count();
    assert!(
      live < 100,
      "expected bounded live Gc count after collecting garbage from 1000 iterations, got {}",
      live,
    );
  }

  #[test]
  fn live_values_survive_collection() {
    // A program that returns a Cell: the Cell must survive a forced
    // collection, proving that reachable `Gc` pointers are not reclaimed.
    let main = compiler::Function {
      num_locals: 0,
      num_params: 0,
      instructions: vec![
        Instruction::PushInt(3),
        Instruction::MakeCell, // cell(3)
        Instruction::Return,
      ],
    };
    let pkg = Package {
      modules: vec![(
        "main".to_string(),
        vec![("main".to_string(), Callable::Function(main))],
      )],
      main: Some((0, 0)),
    };
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();

    // Step through all 3 instructions. The Cell ends up on the stack as the
    // result (is_done() is true, but we haven't popped it).
    for _ in 0..3 {
      exec.step().unwrap();
    }
    assert!(exec.is_done());
    assert!(matches!(exec.peek_value().unwrap(), SLValue::Cell(_)));

    // The Cell on the stack is reachable and must survive a full collection.
    // (There may also be a now-dead Void local from `enter_function`; that one
    // is expected to be reclaimed.)
    exec.collect_all();
    let after = exec.gc_count();
    assert!(
      after >= 2,
      "expected the Cell + its inner value to survive collection, got {} live allocations",
      after,
    );

    // And the value must still be readable after collection.
    assert!(matches!(exec.peek_value().unwrap(), SLValue::Cell(_)));
  }

  #[test]
  fn set_cell_mutates_captured_variable() {
    // The classic counter: `counter` returns an `inc` closure that mutates
    // and returns a captured `count` variable. Each call to `inc` should
    // increment the shared counter, proving that `set!` writes through the
    // Cell that the closure and its parent share.
    let source = "
      (fn counter ()
        (let count 0)
        (fn inc ()
          (set! count (std.+ 1 count))
          count)
        inc)
      (fn main ()
        (let c (counter))
        (c)
        (c)
        (c))
    ";
    assert_eq!(eval_main(source), SLValue::Int(3));
  }

  #[test]
  fn set_cell_returns_new_value() {
    // `set!` should evaluate to the new value, not the old one. We use a
    // closure to get a Cell (captured variables are wrapped in Cells by the
    // closure transform).
    let source = "
      (fn make ()
        (let x 1)
        (fn inc ()
          (set! x (std.+ x 10))))
      (fn main () ((make)))
    ";
    assert_eq!(eval_main(source), SLValue::Int(11));
  }

  #[test]
  fn set_cell_on_non_cell_errors() {
    // `set!` on a local that isn't a Cell (not captured by any closure, so
    // the closure transform doesn't wrap it in a Cell) should error at runtime.
    let source = "
      (fn main ()
        (let x 1)
        (set! x 2))
    ";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    let err = exec.run_until_done().unwrap_err();
    assert!(err.contains("Not a cell"), "unexpected error: {}", err);
  }

  #[test]
  fn cycle_is_collected() {
    // Build a cycle using only source-level constructs: a closure that
    // captures a variable, then `set!` that variable to the closure itself.
    // After the closure transform, `c` is a Cell holding the Partial, and the
    // Partial holds the Cell → Cell↔Partial cycle. Once the result is
    // discarded, the cycle is unreachable and must be collected. This is the
    // key advantage of gc-arena over `Rc` (which would leak cycles).
    //
    // (fn make-cycle ()
    //   (let c 0)          ; c becomes a Cell (captured by self)
    //   (fn self () c)     ; self captures c's Cell
    //   (set! c self)      ; write self into c's Cell → cycle
    //   self)              ; return the cyclic closure
    // (fn main () (make-cycle) 99)  ; discard the cycle, return 99
    let source = "
      (fn make-cycle ()
        (let c 0)
        (fn self () c)
        (set! c self)
        self)
      (fn main () (make-cycle) 99)
    ";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();

    // Run to completion: main calls make-cycle (which builds the cycle and
    // returns it on the stack), then main discards it by returning 99.
    let result = exec.run_until_done().unwrap();
    assert_eq!(result, SLValue::Int(99));

    // At this point the stack is empty (run_until_done popped 99), so the
    // cycle is unreachable. Forcing a full collection must reclaim it.
    exec.collect_all();
    let after = exec.gc_count();
    assert_eq!(
      after, 0,
      "expected 0 live Gc allocations after collecting the cycle, got {}",
      after,
    );
  }

  #[test]
  fn list_self_reference_cycle_is_collected() {
    // A list that contains a cell which in turn refers back to the same list
    // forms a cycle (List ↔ Cell). This is the list analogue of
    // `cycle_is_collected` (which builds a Partial↔Cell cycle via closures).
    let source = "
      (fn make-cycle ()
        (let l (std.list))
        (fn mkcycle () (set! l (std.list l)))
        mkcycle)
      (fn main () ((make-cycle)))
    ";
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();

    let result = exec.run_until_done().unwrap();
    // `main` returns the cyclic list (a 1-element list containing itself),
    // which `run_until_done` pops and hands back to us. The cycle is now only
    // reachable through this returned `SLValue`-side copy — but the in-arena
    // cycle was held alive by `make-cycle`'s frame, which is gone.
    assert!(matches!(result, SLValue::List(_)));

    // Forcing a full collection must reclaim the in-arena cycle, leaving zero
    // live allocations (the popped result is an owned `SLValue`, not a `Gc`).
    exec.collect_all();
    let after = exec.gc_count();
    assert_eq!(
      after, 0,
      "expected 0 live Gc allocations after collecting the List<->Cell cycle, got {}",
      after,
    );
  }

  #[test]
  fn list_empty() {
    assert_eq!(eval_main("(fn main () (std.list))"), SLValue::List(vec![]));
  }

  #[test]
  fn list_with_ints() {
    assert_eq!(
      eval_main("(fn main () (std.list 1 2 3))"),
      SLValue::List(vec![SLValue::Int(1), SLValue::Int(2), SLValue::Int(3)])
    );
  }

  #[test]
  fn list_first_class_as_callable_value() {
    // `list` is a builtin (not a special form): its FunctionRef can be pushed
    // onto the stack and invoked via CallDynamic, exactly as a future `(let l
    // std.list)` binding would compile to once the compiler supports
    // referencing builtins as values.
    let std_mod = 1u32; // "std" is the second module after "main"
                        // `list` is registered after `+`, `-`, `==`, `concat`, so its function
                        // index in the `std` module is 4.
    let std_list = 4u32;
    let main = compiler::Function {
      num_locals: 0,
      num_params: 0,
      instructions: vec![
        Instruction::PushInt(1),
        Instruction::PushInt(2),
        Instruction::PushInt(3),
        Instruction::MakeFunctionRef((std_mod, std_list)),
        Instruction::CallDynamic(3),
        Instruction::Return,
      ],
    };
    let pkg = Package {
      modules: vec![
        (
          "main".to_string(),
          vec![("main".to_string(), Callable::Function(main))],
        ),
        (
          "std".to_string(),
          vec![
            ("+".to_string(), Callable::Builtin),
            ("-".to_string(), Callable::Builtin),
            ("==".to_string(), Callable::Builtin),
            ("concat".to_string(), Callable::Builtin),
            ("list".to_string(), Callable::Builtin),
          ],
        ),
      ],
      main: Some((0, 0)),
    };
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    assert_eq!(
      exec.run_until_done().unwrap(),
      SLValue::List(vec![SLValue::Int(1), SLValue::Int(2), SLValue::Int(3)])
    );
  }

  #[test]
  fn concat_empty_lists() {
    assert_eq!(
      eval_main("(fn main () (std.concat (std.list) (std.list)))"),
      SLValue::List(vec![])
    );
  }

  #[test]
  fn concat_lists() {
    assert_eq!(
      eval_main("(fn main () (std.concat (std.list 1 2) (std.list 3 4)))"),
      SLValue::List(vec![
        SLValue::Int(1),
        SLValue::Int(2),
        SLValue::Int(3),
        SLValue::Int(4)
      ])
    );
  }

  #[test]
  fn concat_list_mismatch_errors() {
    let err = eval_main_err("(fn main () (std.concat (std.list 1) \"x\"))");
    assert!(err.contains("Couldn't concat"), "unexpected error: {}", err);
  }

  #[test]
  fn list_equality() {
    assert_eq!(
      eval_main("(fn main () (std.== (std.list 1 2 3) (std.list 1 2 3)))"),
      SLValue::Bool(true)
    );
    assert_eq!(
      eval_main("(fn main () (std.== (std.list 1 2 3) (std.list 1 2 4)))"),
      SLValue::Bool(false)
    );
    assert_eq!(
      eval_main("(fn main () (std.== (std.list 1) (std.list 1 2)))"),
      SLValue::Bool(false)
    );
  }

  #[test]
  fn idx_list_positive() {
    assert_eq!(
      eval_main("(fn main () (std.idx (std.list 10 20 30) 0))"),
      SLValue::Int(10)
    );
    assert_eq!(
      eval_main("(fn main () (std.idx (std.list 10 20 30) 2))"),
      SLValue::Int(30)
    );
  }

  #[test]
  fn idx_list_negative() {
    assert_eq!(
      eval_main("(fn main () (std.idx (std.list 10 20 30) -1))"),
      SLValue::Int(30)
    );
    assert_eq!(
      eval_main("(fn main () (std.idx (std.list 10 20 30) -3))"),
      SLValue::Int(10)
    );
  }

  #[test]
  fn idx_list_out_of_range_errors() {
    let err = eval_main_err("(fn main () (std.idx (std.list 1 2) 5))");
    assert!(err.contains("out of range"), "unexpected error: {}", err);
    let err = eval_main_err("(fn main () (std.idx (std.list 1 2) -3))");
    assert!(err.contains("out of range"), "unexpected error: {}", err);
  }

  #[test]
  fn idx_string() {
    assert_eq!(
      eval_main("(fn main () (std.idx \"hello\" 0))"),
      SLValue::String("h".to_string())
    );
    assert_eq!(
      eval_main("(fn main () (std.idx \"hello\" 4))"),
      SLValue::String("o".to_string())
    );
    assert_eq!(
      eval_main("(fn main () (std.idx \"hello\" -1))"),
      SLValue::String("o".to_string())
    );
  }

  #[test]
  fn idx_string_out_of_range_errors() {
    let err = eval_main_err("(fn main () (std.idx \"hi\" 5))");
    assert!(err.contains("out of range"), "unexpected error: {}", err);
  }

  #[test]
  fn push_returns_new_list() {
    assert_eq!(
      eval_main("(fn main () (std.push (std.list 1 2) 3))"),
      SLValue::List(vec![SLValue::Int(1), SLValue::Int(2), SLValue::Int(3)])
    );
    // empty + one
    assert_eq!(
      eval_main("(fn main () (std.push (std.list) 7))"),
      SLValue::List(vec![SLValue::Int(7)])
    );
  }

  #[test]
  fn push_is_non_mutating() {
    // The original list is unaffected: indexing it afterwards still works.
    let source = "(fn main () (let l (std.list 1 2)) (std.push l 3) (std.idx l 1))";
    assert_eq!(eval_main(source), SLValue::Int(2));
  }

  #[test]
  fn slice_list_basic() {
    assert_eq!(
      eval_main("(fn main () (std.slice (std.list 1 2 3 4 5) 1 4))"),
      SLValue::List(vec![SLValue::Int(2), SLValue::Int(3), SLValue::Int(4)])
    );
  }

  #[test]
  fn slice_list_negative_indices() {
    assert_eq!(
      eval_main("(fn main () (std.slice (std.list 1 2 3 4 5) -3 -1))"),
      SLValue::List(vec![SLValue::Int(3), SLValue::Int(4)])
    );
  }

  #[test]
  fn slice_list_clamped_bounds() {
    // stop past the end clamps to length.
    assert_eq!(
      eval_main("(fn main () (std.slice (std.list 1 2 3) 1 100))"),
      SLValue::List(vec![SLValue::Int(2), SLValue::Int(3)])
    );
    // start before 0 clamps to 0.
    assert_eq!(
      eval_main("(fn main () (std.slice (std.list 1 2 3) -100 2))"),
      SLValue::List(vec![SLValue::Int(1), SLValue::Int(2)])
    );
  }

  #[test]
  fn slice_list_empty_result() {
    assert_eq!(
      eval_main("(fn main () (std.slice (std.list 1 2 3) 2 2))"),
      SLValue::List(vec![])
    );
    // start > stop yields empty.
    assert_eq!(
      eval_main("(fn main () (std.slice (std.list 1 2 3) 3 0))"),
      SLValue::List(vec![])
    );
  }

  #[test]
  fn slice_string() {
    assert_eq!(
      eval_main("(fn main () (std.slice \"hello world\" 0 5))"),
      SLValue::String("hello".to_string())
    );
    assert_eq!(
      eval_main("(fn main () (std.slice \"hello\" -2 100))"),
      SLValue::String("lo".to_string())
    );
  }

  #[test]
  fn recursive_sum_of_list() {
    // Recursion over a list using `len` for the base case, `idx` for the head,
    // and `slice` for the tail. `(std.slice l 1 (std.len l))` yields everything
    // from index 1 to the end.
    let source = "
      (fn sum (l)
        (if (std.== (std.len l) 0)
          0
          (std.+ (std.idx l 0) (sum (std.slice l 1 (std.len l))))))
      (fn main () (sum (std.list 1 2 3 4 5)))
    ";
    assert_eq!(eval_main(source), SLValue::Int(15));
  }

  /// Helper: evaluate `main` and return the error string (panics if no error).
  fn eval_main_err(source: &str) -> String {
    let pkg = compile_executable_from_source(
      &source.to_string(),
      ("main", "main"),
      &default_builtins().specs(),
    )
    .unwrap();
    let interp = Interpreter::new(pkg);
    let mut exec = interp.call_main().unwrap();
    exec.run_until_done().unwrap_err()
  }
}
