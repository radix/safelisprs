use std::fmt;

pub(crate) const SOURCE_MODULE: &str = "main";

/// A type name qualified by the module that declares it.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct QualifiedTypeName {
  module: String,
  name: String,
}

impl QualifiedTypeName {
  /// Construct a qualified type name.
  pub fn new(module: impl Into<String>, name: impl Into<String>) -> Self {
    Self {
      module: module.into(),
      name: name.into(),
    }
  }

  /// Return the module that declares this type.
  pub fn module(&self) -> &str {
    &self.module
  }

  /// Return the unqualified type name.
  pub fn name(&self) -> &str {
    &self.name
  }

  pub(crate) fn source(name: impl Into<String>) -> Self {
    Self::new(SOURCE_MODULE, name)
  }

  fn display(&self) -> String {
    if self.module == SOURCE_MODULE {
      self.name.clone()
    } else {
      format!("{}::{}", self.module, self.name)
    }
  }
}

impl fmt::Display for QualifiedTypeName {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.display())
  }
}

/// A host-authored SafeLisp type expression used in function signatures.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Signature {
  /// The integer type.
  Int,
  /// The floating-point type.
  Float,
  /// The string type.
  String,
  /// The boolean type.
  Bool,
  /// The void type.
  Void,
  /// A mutable cell containing a value of the given type.
  Cell(Box<Signature>),
  /// A list containing values of the given type.
  List(Box<Signature>),
  /// A callable type with fixed parameters and a return type.
  Fn {
    /// Parameter types accepted by the callable.
    params: Vec<Signature>,
    /// Return type produced by the callable.
    ret: Box<Signature>,
  },
  /// A user-defined or library-defined named type.
  Named(QualifiedTypeName),
  /// A generic type variable by name.
  Var(String),
}

impl Signature {
  /// Construct a type variable with the given name.
  pub fn var(name: impl Into<String>) -> Self {
    Self::Var(name.into())
  }

  /// Construct a cell type containing `item`.
  pub fn cell(item: Signature) -> Self {
    Self::Cell(Box::new(item))
  }

  /// Construct a list type containing `item`.
  pub fn list(item: Signature) -> Self {
    Self::List(Box::new(item))
  }

  /// Construct a function type from parameter and return types.
  pub fn function(params: Vec<Signature>, ret: Signature) -> Self {
    Self::Fn {
      params,
      ret: Box::new(ret),
    }
  }

  /// Construct a named type reference.
  pub fn named(module: impl Into<String>, name: impl Into<String>) -> Self {
    Self::Named(QualifiedTypeName::new(module, name))
  }
}
