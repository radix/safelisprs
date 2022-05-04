extern crate bincode;
extern crate clap;
extern crate safelisp;
extern crate serde_yaml;

use std::fs::File;
use std::io::prelude::*;

use anyhow::{anyhow, Result};
use clap::Parser;

use safelisp::compiler::Package;
use safelisp::interpreter::Interpreter;

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct Args {
  #[clap(long, default_value = "bincode", help = "either yaml or bincode")]
  format: String,

  #[clap(help = "A .slc file to interpret")]
  input_file: String,
}

fn main() -> Result<()> {
  let args = Args::parse();

  let input_file = &args.input_file;

  let mut f = File::open(input_file).unwrap_or_else(|_| panic!("Couldn't open {}", input_file));
  let package: Package = match args.format.as_str() {
    "bincode" => {
      let mut v = vec![];
      f.read_to_end(&mut v)
        .unwrap_or_else(|_| panic!("Couldn't read from file {}", input_file));
      let package: Result<Package> = bincode::deserialize(&v[..]).map_err(|e| e.into());
      package
    }
    "yaml" => {
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
