extern crate bincode;
extern crate clap;
extern crate safelisp;
extern crate serde_yaml;

use std::fs::File;
use std::io::prelude::*;

use anyhow::{anyhow, Result};
use clap::{App, Arg};

use safelisp::compiler::Package;
use safelisp::interpreter::Interpreter;

fn main() -> Result<()> {
  let args = App::new("SafeLisp Interpreter")
    .arg(Arg::with_name("INPUT").required(true).index(1))
    .arg(
      Arg::with_name("format")
        .long("format")
        .takes_value(true)
        .default_value("bincode"),
    )
    .get_matches();

  let input_file = args.value_of("INPUT").unwrap();

  let mut f = File::open(input_file).unwrap_or_else(|_| panic!("Couldn't open {}", input_file));
  let package: Package = match args.value_of("format") {
    Some("bincode") => {
      let mut v = vec![];
      f.read_to_end(&mut v)
        .unwrap_or_else(|_| panic!("Couldn't read from file {}", input_file));
      let package: Result<Package> = bincode::deserialize(&v[..]).map_err(|e| e.into());
      package
    }
    Some("yaml") => {
      let mut s = String::new();
      f.read_to_string(&mut s)
        .unwrap_or_else(|_| panic!("Couldn't read from file {}", input_file));
      let package: Result<Package> = serde_yaml::from_str(&s).map_err(|e| e.into());
      package
    }
    format => Err(anyhow!("invalid format: {:?}", format)),
  }?;

  let mut interpreter = Interpreter::new(package);
  let result = interpreter.call_main().expect("Error calling main");
  println!("Result: {:?}", result);
  Ok(())
}
