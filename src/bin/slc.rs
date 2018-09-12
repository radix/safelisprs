extern crate bincode;
extern crate clap;
#[macro_use] extern crate failure;
extern crate safelisp;
extern crate serde_yaml;

use std::path::{Path, PathBuf};
use std::fs::File;
use std::io::prelude::*;

use clap::{App, Arg};

use safelisp::compiler::{compile_from_sources, compile_executable_from_sources};

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

  let input_files : Vec<&str> = args.values_of("INPUT").ok_or_else(|| format_err!("Must provide input files on the command line."))?.collect();
  let format = args.value_of("format").unwrap_or("bincode");
  let output_file = args.value_of("output").ok_or_else(|| format_err!("Must specify output file"))?;

  println!("Compiling {:?} to {}", input_files, output_file);

  let mut module_sources = vec![];

  for input_filename in input_files {
    let input_filename = PathBuf::from(input_filename);
    let module_name = Path::new(input_filename.file_stem().ok_or_else(|| format_err!("{:?} is not a file", input_filename))?).file_name().ok_or_else(|| format_err!("Couldn't get file name of {:?}", input_filename))?;

    let input_data = {
      let mut f = File::open(&input_filename)?;
      let mut input_data = String::new();
      f.read_to_string(&mut input_data)?;
      input_data
    };

    module_sources.push((module_name.to_string_lossy().to_string(), input_data));
  }

  let package = if args.is_present("no-main") {
    compile_from_sources(&module_sources)
  } else {
    let main_mod = args.value_of("main-module").ok_or_else(|| format_err!("There must be a main module"))?;
    let main_func = args.value_of("main-function").ok_or_else(|| format_err!("There must be a main function"))?;
    compile_executable_from_sources(&module_sources, (main_mod, main_func))
  }.map_err(|e| format_err!("{}", e))?;

  let output = match format {
    "yaml" => serde_yaml::to_string(&package)?.into_bytes(),
    "bincode" => bincode::serialize(&package)?,
    _ => panic!(format!("Invalid format: {}", format)),
  };
  let mut outfile =
    File::create(&output_file).expect(&format!("Couldn't create file {}", output_file));
  outfile.write_all(&output)?;
  Ok(())
}
