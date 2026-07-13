use std::process::Command;

#[test]
fn code_argument_compiles_and_executes_source() {
  let output = Command::new(env!("CARGO_BIN_EXE_sli"))
    .args(["-c", "(fn main () (std.+ 1 2))"])
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
    .args(["-c", "(fn main () 1)", "program.slc"])
    .output()
    .expect("failed to run sli");

  assert!(!output.status.success());
  assert!(String::from_utf8_lossy(&output.stderr).contains("cannot be used with"));
}
