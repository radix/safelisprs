extern crate bincode;
extern crate clap;
extern crate safelisp;
extern crate serde_yaml;

use std::fs::File;
use std::io::prelude::*;
use std::time::Duration;

use anyhow::{anyhow, Result};
use clap::Parser;

use safelisp::builtins::default_builtins;
use safelisp::compiler::compile_executable_from_source;
use safelisp::compiler::Package;
use safelisp::interpreter::{Interpreter, Status};

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct Args {
  #[clap(long, default_value = "bincode", help = "either yaml or bincode")]
  format: String,

  #[clap(
    long,
    help = "maximum number of bytecodes to execute (default: unlimited)"
  )]
  instruction_limit: Option<u64>,

  #[clap(
    long,
    help = "maximum number of milliseconds to execute (default: unlimited)"
  )]
  time_limit_ms: Option<u64>,

  #[clap(
    long,
    help = "maximum number of live GC-allocated bytes (default: unlimited)"
  )]
  memory_limit_bytes: Option<usize>,

  #[clap(
    short = 'c',
    long = "code",
    value_name = "SOURCE",
    conflicts_with = "input_file",
    required_unless_present = "input_file",
    help = "compile and execute a SafeLisp source string"
  )]
  code: Option<String>,

  #[clap(
    name = "input_file",
    conflicts_with = "code",
    required_unless_present = "code",
    help = "A .slc file to interpret"
  )]
  input_file: Option<String>,
}

fn main() -> Result<()> {
  let args = Args::parse();

  let package = match (&args.code, &args.input_file) {
    (Some(source), None) => {
      compile_executable_from_source(source, ("main", "main"), &default_builtins().specs())
        .map_err(|e| anyhow!("{}", e))?
    }
    (None, Some(input_file)) => {
      let mut f = File::open(input_file).unwrap_or_else(|_| panic!("Couldn't open {}", input_file));
      let package = match args.format.as_str() {
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
      package
    }
    _ => unreachable!("clap requires exactly one source input"),
  };

  let interpreter = Interpreter::new(package);
  let mut exec = interpreter.call_main().expect("Error setting up main");
  if let Some(bytes) = args.memory_limit_bytes {
    exec.set_memory_limit(Some(bytes));
  }
  match (args.instruction_limit, args.time_limit_ms) {
    (None, None) => {
      let result = exec.run_until_done().map_err(|e| anyhow!("{}", e))?;
      println!("Result: {:?}", result);
    }
    (Some(limit), None) => {
      let status = exec.run(limit).map_err(|e| anyhow!("{}", e))?;
      match status {
        Status::Done(v) => println!("Result: {:?}", v),
        Status::Paused => {
          eprintln!(
            "instruction limit exceeded: {} instructions executed (limit {})",
            exec.executed, limit
          );
          std::process::exit(1);
        }
      }
    }
    (None, Some(ms)) => {
      let status = exec
        .run_for_duration(Duration::from_millis(ms))
        .map_err(|e| anyhow!("{}", e))?;
      match status {
        Status::Done(v) => println!("Result: {:?}", v),
        Status::Paused => {
          eprintln!(
            "time limit exceeded: {} instructions executed (limit {}ms)",
            exec.executed, ms
          );
          std::process::exit(1);
        }
      }
    }
    (Some(_), Some(_)) => {
      return Err(anyhow!(
        "--instruction-limit and --time-limit-ms are mutually exclusive"
      ));
    }
  }
  Ok(())
}
