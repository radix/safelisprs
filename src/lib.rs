//! SafeLisp parser, compiler, and interpreter APIs.
//!
//! The crate exposes the pieces needed to compile SafeLisp source into an
//! executable [`Package`], run it with [`Interpreter`], and extend the runtime
//! with host-provided [`Builtin`] functions.
#![deny(missing_docs)]

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
mod types;
#[cfg(feature = "wasm")]
/// WebAssembly compilation support for SafeLisp programs.
pub mod wasm;

pub use builtins::{
  sig, Builtin, BuiltinSignature, BuiltinSpec, CustomFieldSpec, CustomTypeSpec, Library, Trait,
  TypeConst,
};
pub use compiler::{compile_executable_from_source, Package};
pub use interpreter::{
  Accounted, CellContents, EnumInstance, Execution, HostCtx, HostPoll, Interpreter,
  MemoryReservation, Partial, SLVal, SLValue, Status, StructInstance, Value,
};
