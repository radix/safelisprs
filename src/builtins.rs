use std::sync::Arc;

use gc_arena::Gc;

use crate::interpreter::SLVal;

/// A compile-time description of a builtin: which module/name it lives in and
/// how many arguments it takes. `num_params` is `None` for variadic builtins.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct BuiltinSpec {
  pub module: &'static str,
  pub name: &'static str,
  pub num_params: Option<u16>,
}

/// A builtin's runtime handler. Takes the evaluated arguments (as `Gc` pointers
/// into the current execution's arena) and returns either a result value or a
/// runtime error string.
///
/// The `for<'gc>` higher-ranked bound lets one `'static` handler serve any
/// execution's arena.
pub type HostFn = Arc<dyn for<'gc> Fn(&[Gc<'gc, SLVal<'gc>>]) -> Result<SLVal<'gc>, String>>;

/// A builtin: metadata ([`BuiltinSpec`]) plus its handler ([`HostFn`]).
#[derive(Clone)]
pub struct Builtin {
  spec: BuiltinSpec,
  func: HostFn,
}

impl Builtin {
  pub fn spec(&self) -> BuiltinSpec {
    self.spec
  }

  /// Invoke this builtin's handler with the given arguments.
  pub fn call<'gc>(&self, args: &[Gc<'gc, SLVal<'gc>>]) -> Result<SLVal<'gc>, String> {
    (self.func)(args)
  }

  /// A unary (one-arg) builtin.
  pub fn unary(
    module: &'static str,
    name: &'static str,
    func: impl for<'gc> Fn(Gc<'gc, SLVal<'gc>>) -> Result<SLVal<'gc>, String> + 'static,
  ) -> Self {
    Builtin {
      spec: BuiltinSpec {
        module,
        name,
        num_params: Some(1),
      },
      func: Arc::new(move |args| func(args[0])),
    }
  }

  /// A binary (two-arg) builtin. `func` receives `(left, right)`.
  pub fn binary(
    module: &'static str,
    name: &'static str,
    func: impl for<'gc> Fn(Gc<'gc, SLVal<'gc>>, Gc<'gc, SLVal<'gc>>) -> Result<SLVal<'gc>, String>
      + 'static,
  ) -> Self {
    Builtin {
      spec: BuiltinSpec {
        module,
        name,
        num_params: Some(2),
      },
      func: Arc::new(move |args| func(args[0], args[1])),
    }
  }

  /// A ternary (three-arg) builtin.
  pub fn ternary(
    module: &'static str,
    name: &'static str,
    func: impl for<'gc> Fn(
        Gc<'gc, SLVal<'gc>>,
        Gc<'gc, SLVal<'gc>>,
        Gc<'gc, SLVal<'gc>>,
      ) -> Result<SLVal<'gc>, String>
      + 'static,
  ) -> Self {
    Builtin {
      spec: BuiltinSpec {
        module,
        name,
        num_params: Some(3),
      },
      func: Arc::new(move |args| func(args[0], args[1], args[2])),
    }
  }

  /// A variadic builtin: it receives the whole argument slice and may be called
  /// with any number of args (including zero). `num_params` is `None`, so the
  /// interpreter uses the call-site arity (carried on `Instruction::Call` /
  /// `Instruction::CallDynamic`) to know how many args to pop.
  pub fn variadic(
    module: &'static str,
    name: &'static str,
    func: impl for<'gc> Fn(&[Gc<'gc, SLVal<'gc>>]) -> Result<SLVal<'gc>, String> + 'static,
  ) -> Self {
    Builtin {
      spec: BuiltinSpec {
        module,
        name,
        num_params: None,
      },
      func: Arc::new(move |args| func(args)),
    }
  }
}

/// A registry of builtins available to a program. The compiler reads the
/// [`BuiltinSpec`]s (via [`Builtins::specs`]) to register `Callable::Builtin`
/// slots; the interpreter looks up the handler (via [`Builtins::lookup`]) at
/// runtime. Mirrors the WASM backend's `wasm::Builtins`.
#[derive(Clone, Default)]
pub struct Builtins {
  entries: Vec<Builtin>,
}

impl Builtins {
  pub fn new() -> Self {
    Self::default()
  }

  /// Register a builtin (builder style).
  pub fn with_builtin(mut self, builtin: Builtin) -> Self {
    self.entries.push(builtin);
    self
  }

  pub fn iter(&self) -> impl Iterator<Item = &Builtin> {
    self.entries.iter()
  }

  /// The specs of all registered builtins, for the compiler to inject as
  /// `Callable::Builtin` slots.
  pub fn specs(&self) -> Vec<BuiltinSpec> {
    self.entries.iter().map(|b| b.spec).collect()
  }

  /// Look up a builtin by `(module, name)`.
  pub fn lookup(&self, module: &str, name: &str) -> Option<&Builtin> {
    self
      .entries
      .iter()
      .find(|b| b.spec.module == module && b.spec.name == name)
  }
}

/// The default builtin registry.
pub fn default_builtins() -> Builtins {
  Builtins::new()
    .with_builtin(Builtin::binary("std", "+", |a, b| match (&*a, &*b) {
      (SLVal::Int(x), SLVal::Int(y)) => Ok(SLVal::Int(x + y)),
      (SLVal::Float(x), SLVal::Float(y)) => Ok(SLVal::Float(x + y)),
      _ => Err(format!("Couldn't add {:?} and {:?}", a, b)),
    }))
    .with_builtin(Builtin::binary("std", "-", |a, b| match (&*a, &*b) {
      // `a` is the left operand, `b` is the right operand: left - right.
      (SLVal::Int(x), SLVal::Int(y)) => Ok(SLVal::Int(x - y)),
      (SLVal::Float(x), SLVal::Float(y)) => Ok(SLVal::Float(x - y)),
      _ => Err(format!("Couldn't sub {:?} and {:?}", a, b)),
    }))
    .with_builtin(Builtin::binary("std", "==", |a, b| Ok(SLVal::Bool(a == b))))
    .with_builtin(Builtin::binary("std", "concat", |a, b| match (&*a, &*b) {
      (SLVal::String(x), SLVal::String(y)) => Ok(SLVal::String(format!("{x}{y}"))),
      _ => Err(format!("Couldn't concat {:?} and {:?}", a, b)),
    }))
}
