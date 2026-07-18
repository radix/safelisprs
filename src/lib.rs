#[macro_use]
extern crate serde_derive;

#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;

mod builtins;
mod closure;
mod compiler;
mod interpreter;
mod parser;
mod prelude;
mod typecheck;
#[cfg(feature = "wasm")]
pub mod wasm;

pub use builtins::{
  default_builtins, sig, Builtin, BuiltinSignature, BuiltinSpec, Builtins, Trait, TypeConst,
};
pub use compiler::{compile_executable_from_source, Package};
pub use interpreter::{
  Accounted, CellContents, EnumInstance, Execution, HostCtx, HostPoll, Interpreter,
  MemoryReservation, Partial, SLVal, SLValue, Status, StructInstance, Value,
};
pub use prelude::std_prelude_from_specs;
