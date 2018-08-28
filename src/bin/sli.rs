extern crate bincode;
extern crate clap;
extern crate safelisp;
extern crate serde_yaml;

use std::fs::File;
use std::io::prelude::*;

use clap::{App, Arg};

use safelisp::interpreter::call_in_module;

fn main() {
  let args = App::new("SafeLisp Interpreter")
    .arg(Arg::with_name("INPUT").required(true).index(1))
    .arg(Arg::with_name("function").default_value("main"))
    .get_matches();

  let input_file = args.value_of("INPUT").unwrap();
  let function_name = args.value_of("function").unwrap();

  let contents = {
    let mut f = File::open(input_file).expect(&format!("Couldn't open {}", input_file));
    let mut v = vec![];
    f.read_to_end(&mut v).expect(&format!("Couldn't read from file {}", input_file));
    v
  };
  let module = bincode::deserialize(&contents[..]).expect(&format!("Couldn't deserialize module from {}", input_file));
  let result = call_in_module(&module, function_name).expect(&format!("Error calling {}", function_name));
  println!("Result: {:?}", result);
}
