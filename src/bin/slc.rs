extern crate bincode;
extern crate clap;
extern crate safelisp;
extern crate serde_yaml;

use std::fs::File;
use std::io::prelude::*;
use std::path::{Path, PathBuf};

use anyhow::{anyhow, Result};
use clap::Parser;

use safelisp::compiler::{compile_executable_from_sources, compile_from_sources};

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct Args {
  #[clap(long)]
  output: String,

  #[clap(long, default_value = "bincode", help="either yaml or bincode")]
  format: String,

  #[clap(long, help="the name of the main module (filename without the .sl)")]
  main_module: Option<String>,
  #[clap(long, default_value = "main", help="name of the main function")]
  main_function: String,

  #[clap(long, help="skip main generation")]
  no_main: bool,

  #[clap(help="list of .sl files")]
  input_files: Vec<String>,
}

fn main() -> Result<()> {
  let args = Args::parse();

  let input_files = args.input_files;
  let format = args.format;
  let output_file = args.output;

  println!("Compiling {:?} to {}", input_files, output_file);

  let mut module_sources = vec![];

  for input_filename in input_files {
    let input_filename = PathBuf::from(input_filename);
    let module_name = Path::new(
      input_filename
        .file_stem()
        .ok_or_else(|| anyhow!("{:?} is not a file", input_filename))?,
    )
    .file_name()
    .ok_or_else(|| anyhow!("Couldn't get file name of {:?}", input_filename))?;

    let input_data = {
      let mut f = File::open(&input_filename)?;
      let mut input_data = String::new();
      f.read_to_string(&mut input_data)?;
      input_data
    };

    module_sources.push((module_name.to_string_lossy().to_string(), input_data));
  }

  let package = if args.no_main {
    compile_from_sources(&module_sources)
  } else {
    let main_mod = args
      .main_module
      .ok_or_else(|| anyhow!("There must be a main module"))?;
    let main_func = args.main_function;
    compile_executable_from_sources(&module_sources, (&main_mod, &main_func))
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
