use std::process::Command;

#[test]
fn code_argument_compiles_and_executes_source() {
  let output = Command::new(env!("CARGO_BIN_EXE_sli"))
    .args(["-c", "(fn main () ->Int (std::+ 1 2))"])
    .output()
    .expect("failed to run sli");

  assert!(
    output.status.success(),
    "sli failed: {}",
    String::from_utf8_lossy(&output.stderr)
  );
  assert!(String::from_utf8_lossy(&output.stdout).contains("Result: Int(3)"));
}

#[test]
fn code_argument_uses_std_prelude() {
  let output = Command::new(env!("CARGO_BIN_EXE_sli"))
    .args(["-c", "(fn main () ->Int (+ 1 2))"])
    .output()
    .expect("failed to run sli");

  assert!(
    output.status.success(),
    "sli failed: {}",
    String::from_utf8_lossy(&output.stderr)
  );
  assert!(String::from_utf8_lossy(&output.stdout).contains("Result: Int(3)"));
}

#[test]
fn code_argument_conflicts_with_input_file() {
  let output = Command::new(env!("CARGO_BIN_EXE_sli"))
    .args(["-c", "(fn main () ->Int 1)", "program.slc"])
    .output()
    .expect("failed to run sli");

  assert!(!output.status.success());
  assert!(String::from_utf8_lossy(&output.stderr).contains("cannot be used with"));
}

#[test]
fn slc_uses_std_prelude() {
  let mut input_path = std::env::temp_dir();
  input_path.push(format!(
    "safelisp-slc-std-prelude-{}.sl",
    std::process::id()
  ));
  let mut output_path = std::env::temp_dir();
  output_path.push(format!(
    "safelisp-slc-std-prelude-{}.slc",
    std::process::id()
  ));

  std::fs::write(&input_path, "(fn main () ->Int (+ 1 2))").expect("write source");
  let input_arg = input_path.to_string_lossy().into_owned();
  let output_arg = output_path.to_string_lossy().into_owned();
  let compile = Command::new(env!("CARGO_BIN_EXE_slc"))
    .args(["--output", &output_arg, &input_arg])
    .output()
    .expect("failed to run slc");

  assert!(
    compile.status.success(),
    "slc failed: {}",
    String::from_utf8_lossy(&compile.stderr)
  );

  let run = Command::new(env!("CARGO_BIN_EXE_sli"))
    .arg(&output_arg)
    .output()
    .expect("failed to run sli");
  let _ = std::fs::remove_file(&input_path);
  let _ = std::fs::remove_file(&output_path);

  assert!(
    run.status.success(),
    "sli failed: {}",
    String::from_utf8_lossy(&run.stderr)
  );
  assert!(String::from_utf8_lossy(&run.stdout).contains("Result: Int(3)"));
}
