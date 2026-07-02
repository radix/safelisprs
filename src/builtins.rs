use crate::interpreter::{BuiltinResult, Builtins, SLVal, Stack};

/// A compile-time description of a builtin: which module/name it lives in and
/// how many arguments it takes. The compiler/linker uses this to register
/// `Callable::Builtin` entries in the right module's function table so that
/// `Call` instructions can resolve; the *behavior* is still supplied at
/// runtime by the `Builtins` trait impl. `num_params` is `None` for variadic
/// builtins (reserved for future use — see the variadic-builtins TODO item).
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct BuiltinSpec {
  pub module: &'static str,
  pub name: &'static str,
  pub num_params: Option<u16>,
}

/// The default builtin registry: the four `std` builtins implemented by
/// `DefaultBuiltins`. This must be kept up-to-date with `DefaultBuiltins::call`.
pub const DEFAULT_BUILTIN_SPECS: &[BuiltinSpec] = &[
  BuiltinSpec {
    module: "std",
    name: "+",
    num_params: Some(2),
  },
  BuiltinSpec {
    module: "std",
    name: "-",
    num_params: Some(2),
  },
  BuiltinSpec {
    module: "std",
    name: "==",
    num_params: Some(2),
  },
  BuiltinSpec {
    module: "std",
    name: "concat",
    num_params: Some(2),
  },
];

#[derive(Clone)]
pub struct DefaultBuiltins;

impl Builtins for DefaultBuiltins {
  fn call<'gc, 'stack>(
    &self,
    mod_name: &str,
    name: &str,
    stack: &mut Stack<'gc, 'stack>,
  ) -> BuiltinResult {
    // This must be kept up-to-date with DEFAULT_BUILTIN_SPECS.
    match (mod_name, name) {
      ("std", "+") => Some(builtin_add(stack)),
      ("std", "-") => Some(builtin_sub(stack)),
      ("std", "==") => Some(builtin_eq(stack)),
      ("std", "concat") => Some(builtin_concat(stack)),
      _ => None,
    }
  }
}

fn builtin_add<'gc, 'stack>(stack: &mut Stack<'gc, 'stack>) -> Result<(), String> {
  // Pop both operands before borrowing `mc` from `stack`, to avoid overlapping
  // the immutable `mc` borrow with the mutable `pop`/`push` borrows.
  let one = stack.pop()?;
  let two = stack.pop()?;
  let result = match (&*one, &*two) {
    (SLVal::Int(one), SLVal::Int(two)) => SLVal::Int(one + two),
    (SLVal::Float(one), SLVal::Float(two)) => SLVal::Float(one + two),
    _ => return Err(format!("Couldn't add {:?} and {:?}", one, two)),
  };
  stack.push(result);
  Ok(())
}

fn builtin_sub<'gc, 'stack>(stack: &mut Stack<'gc, 'stack>) -> Result<(), String> {
  let one = stack.pop()?;
  let two = stack.pop()?;
  let result = match (&*one, &*two) {
    (SLVal::Int(one), SLVal::Int(two)) => SLVal::Int(two - one),
    (SLVal::Float(one), SLVal::Float(two)) => SLVal::Float(two - one),
    _ => return Err(format!("Couldn't sub {:?} and {:?}", one, two)),
  };
  stack.push(result);
  Ok(())
}

fn builtin_eq<'gc, 'stack>(stack: &mut Stack<'gc, 'stack>) -> Result<(), String> {
  let one = stack.pop()?;
  let two = stack.pop()?;
  let result = SLVal::Bool(one == two);
  stack.push(result);
  Ok(())
}

fn builtin_concat<'gc, 'stack>(stack: &mut Stack<'gc, 'stack>) -> Result<(), String> {
  // Operands are popped in reverse order of how they were pushed, so `one`
  // is the right-hand argument and `two` is the left-hand argument.
  let one = stack.pop()?;
  let two = stack.pop()?;
  let result = match (&*one, &*two) {
    (SLVal::String(one), SLVal::String(two)) => SLVal::String(format!("{two}{one}")),
    _ => return Err(format!("Couldn't concat {:?} and {:?}", one, two)),
  };
  stack.push(result);
  Ok(())
}
