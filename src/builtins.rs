use std::rc::Rc;

use crate::interpreter::{BuiltinResult, SLVal, Stack};

pub fn builtin_builtins(mod_name: &str, name: &str, stack: &mut Stack) -> BuiltinResult {
  // This must be kept up-to-date with std.sl
  match (mod_name, name) {
    ("std", "+") => Some(builtin_add(stack)),
    _ => None,
  }
}

pub fn builtin_add(stack: &mut Stack) -> Result<(), String> {
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
