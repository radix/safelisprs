extern crate atoms;

#[cfg(test)]
#[macro_use]
extern crate maplit;

use std::collections::HashMap;
use std::rc::Rc; // TODO: use Manishearth/rust-gc

mod compiler;
mod interpreter;
mod parser;
