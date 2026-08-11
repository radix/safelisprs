//! SafeLisp parser, compiler, and interpreter APIs.
//!
//! The crate exposes the pieces needed to compile SafeLisp source into an
//! executable [`Package`], run it with [`Interpreter`], and extend the runtime
//! with host-provided [`Builtin`] functions.
#![deny(missing_docs)]
#![deny(unsafe_code)]

#[macro_use]
extern crate serde_derive;

#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;

/// Builtin host libraries and the APIs for composing them.
pub mod builtins;
mod closure;
mod compiler;
mod interpreter;
mod parser;
mod resolver;
mod typecheck;
mod types;

pub use builtins::{
  sig, Builtin, BuiltinSignature, BuiltinSpec, CustomFieldSpec, CustomTypeKind, CustomTypeSpec,
  CustomVariantSpec, Library, Trait,
};
pub use compiler::{
  compile_executable_from_source, compile_executable_from_source_with_options, CompileOptions,
  Package,
};
pub use interpreter::{
  Accounted, Args, CellContents, EnumInstance, Execution, HostCtx, HostPoll, Interpreter,
  MemoryReservation, Partial, SLVal, SLValue, Status, StructInstance, Value,
};
pub use types::{QualifiedTypeName, Signature};
