use std::fmt;

pub(crate) const SOURCE_MODULE: &str = "main";

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct QualifiedTypeName {
  pub(crate) module: String,
  pub(crate) name: String,
}

impl QualifiedTypeName {
  pub(crate) fn new(module: impl Into<String>, name: impl Into<String>) -> Self {
    Self {
      module: module.into(),
      name: name.into(),
    }
  }

  pub(crate) fn source(name: impl Into<String>) -> Self {
    Self::new(SOURCE_MODULE, name)
  }

  pub(crate) fn display(&self) -> String {
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
