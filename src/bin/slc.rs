extern crate bincode;
extern crate clap;
#[macro_use] extern crate failure;
extern crate safelisp;
extern crate serde_yaml;
#[macro_use] extern crate maplit;

use std::path::PathBuf;
use std::fs::File;
use std::io::prelude::*;

use clap::{App, Arg};

use safelisp::compiler::compile_from_sources;

fn main() -> Result<(), failure::Error> {
  let args = App::new("SafeLisp Compiler")
    .arg(Arg::with_name("output").long("output").takes_value(true))
    .arg(Arg::with_name("format").long("format").takes_value(true))
    .arg(
      Arg::with_name("main-module")
        .long("main-module")
        .takes_value(true)
        .help("Override the default name of the main module"),
    )
    .arg(
      Arg::with_name("main-function")
        .long("main-function")
        .takes_value(true)
        .default_value("main")
        .help("Override the default name of the main function"),
    )
    .arg(
      Arg::with_name("no-main")
        .long("no-main")
        .help("Don't set a main function, even if there's a main.rs with a main function inside."),
    )
    .arg(
      Arg::with_name("INPUT")
        .required(true)
        .index(1)
        .multiple(true)
        .help("Input files. Each one will count as a module."),
    )
    .get_matches();
  println!("args is {:?}", args);

  let input_files = args.values_of("INPUT").ok_or_else(|| format_err!("Must provide input files on the command line."))?.collect();
  let format = args.value_of("format").unwrap_or("bincode");
  let output_file = args.value_of("output").expect("Must specify --output");

  println!("Compiling {:?} to {}", input_files, output_file);

  let mut module_asts = hashmap!{};

  for input_filename in input_files {
    let input_filename = PathBuf::from(input_filename);

    let input_data = {
      let mut f = File::open(input_filename)?;
      let mut input_data = String::new();
      f.read_to_string(&mut input_data)?;
      input_data
    };

    let module = compile_from_source(&input_data).expect("Error while compiling module");
  }

  let output = match format {
    "yaml" => serde_yaml::to_string(&module)
      .expect("Error serializing to YAML")
      .into_bytes(),
    "bincode" => bincode::serialize(&module).expect("Error serializing to Bincode"),
    _ => panic!(format!("Invalid format: {}", format)),
  };
  let mut outfile =
    File::create(&output_file).expect(&format!("Couldn't create file {}", output_file));
  outfile
    .write_all(&output)
}
