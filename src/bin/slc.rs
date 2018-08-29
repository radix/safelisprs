extern crate bincode;
extern crate clap;
extern crate safelisp;
extern crate serde_yaml;

use std::fs::File;
use std::io::prelude::*;

use clap::{App, Arg};

use safelisp::compile_from_source;

fn main() {
  let args = App::new("SafeLisp Compiler")
    .arg(Arg::with_name("output").long("output").takes_value(true))
    .arg(Arg::with_name("format").long("format").takes_value(true))
    .arg(
      Arg::with_name("main-module")
        .long("main-module")
        .takes_value(true),
    )
    .arg(
      Arg::with_name("main-function")
        .long("main-function")
        .takes_value(true)
        .default_value("main"),
    )
    .arg(
      Arg::with_name("INPUT")
        .required(true)
        .index(1)
        .multiple(true),
    )
    .get_matches();
  println!("args is {}", args);

  let input_file = args.value_of("INPUT").expect("argument is required");
  let format = args.value_of("format").unwrap_or("bincode");
  let output_file = args
    .value_of("output")
    .map(|x| x.to_string())
    .unwrap_or_else(|| input_file.to_string() + "." + format);

  println!("Compiling {} to {}", input_file, output_file);

  let input_data = {
    let mut f = File::open(input_file).expect(&format!("Couldn't find {}", input_file));
    let mut input_data = String::new();
    f.read_to_string(&mut input_data)
      .expect(&format!("Error reading file {}", input_file));
    input_data
  };

  let module = compile_from_source(&input_data).expect("Error while compiling module");
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
    .expect(&format!("Error while writing to {}", output_file));
}
