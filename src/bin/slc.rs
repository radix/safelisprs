extern crate bincode;
extern crate clap;
extern crate safelisp;
extern crate serde_yaml;

use std::fs::File;
use std::io::prelude::*;
use std::path::PathBuf;

use anyhow::{anyhow, Result};
use clap::Parser;

use safelisp::compiler::{compile_executable_from_source, compile_from_source};

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct Args {
  #[clap(long)]
  output: String,

  #[clap(long, default_value = "bincode", help = "either yaml or bincode")]
  format: String,

  #[clap(long, default_value = "main", help = "name of the main function")]
  main_function: String,

  #[clap(long, help = "skip main generation")]
  no_main: bool,

  #[clap(help = "Main .sl file")]
  input_file: String,
}

fn main() -> Result<()> {
  let args = Args::parse();

  let input_file = args.input_file;
  let format = args.format;
  let output_file = args.output;

  println!("Compiling {:?} to {}", input_file, output_file);

  let input_filename = PathBuf::from(input_file);

  let input_data = {
    let mut f = File::open(&input_filename)?;
    let mut input_data = String::new();
    f.read_to_string(&mut input_data)?;
    input_data
  };

  let package = if args.no_main {
    compile_from_source(&input_data)
  } else {
    let main_func = args.main_function;
    compile_executable_from_source(&input_data, ("main", &main_func))
  }
  .map_err(|e| anyhow!("{}", e))?;

  let output = match format.as_str() {
    "yaml" => serde_yaml::to_string(&package)?.into_bytes(),
    "bincode" => bincode::serialize(&package)?,
    _ => panic!("Invalid format: {}", format),
  };
  let mut outfile =
    File::create(&output_file).unwrap_or_else(|_| panic!("Couldn't create file {}", output_file));
  outfile.write_all(&output)?;
  Ok(())
}
