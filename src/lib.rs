extern crate atoms;

#[macro_use]
extern crate maplit;

pub mod compiler;
pub mod interpreter;
pub mod parser;

pub fn compile_from_source(s: &str) -> Result<compiler::Module, String> {
  let asts = parser::read_multiple(s)?;
  compiler::compile_module(&asts)
}

#[cfg(test)]
mod test {
  //! End-to-end tests

  use std::rc::Rc;

  use super::*;
  use super::{interpreter};

  #[test]
  fn compile_basic_module() {
    let source = "
    (fn add1 (n) (+ n 1))
    (fn main () (add1 3))
    ";

    let module = compile_from_source(source).unwrap();
    println!("{:?}", module);
    let result = interpreter::call_in_module(&module, "main").unwrap();
    assert_eq!(result, Rc::new(interpreter::SLVal::Int(4)));
  }
}
