use std::process::Command;

use safelisp::{Interpreter, Package, SLValue};

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

  let package_bytes = std::fs::read(&output_path).expect("read compiled package");
  let package: Package = bincode::deserialize(&package_bytes).expect("deserialize package");
  let result = Interpreter::new(package)
    .call_main()
    .expect("call main")
    .run_until_done()
    .expect("run package");
  let _ = std::fs::remove_file(&input_path);
  let _ = std::fs::remove_file(&output_path);

  assert_eq!(result, SLValue::Int(3));
}
