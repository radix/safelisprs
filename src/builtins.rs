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
      (SLVal::List(x), SLVal::List(y)) => {
        let mut combined = x.clone();
        combined.extend(y.iter().cloned());
        Ok(SLVal::List(combined))
      }
      _ => Err(format!("Couldn't concat {:?} and {:?}", a, b)),
    }))
    .with_builtin(Builtin::variadic("std", "list", |args| {
      Ok(SLVal::List(args.to_vec()))
    }))
    .with_builtin(Builtin::unary("std", "len", |a| match &*a {
      SLVal::List(items) => Ok(SLVal::Int(items.len() as i64)),
      _ => Err(format!("len: expected a List, got {:?}", a)),
    }))
    .with_builtin(Builtin::binary("std", "idx", |a, b| match (&*a, &*b) {
      (SLVal::List(items), SLVal::Int(i)) => {
        let len = items.len() as i64;
        let idx = if *i < 0 { *i + len } else { *i };
        if idx < 0 || idx >= len {
          Err(format!(
            "idx: index {} out of range for list of length {}",
            i, len
          ))
        } else {
          Ok((**items.get(idx as usize).unwrap()).clone())
        }
      }
      (SLVal::String(s), SLVal::Int(i)) => {
        let len = s.chars().count() as i64;
        let idx = if *i < 0 { *i + len } else { *i };
        if idx < 0 || idx >= len {
          Err(format!(
            "idx: index {} out of range for string of length {}",
            i, len
          ))
        } else {
          Ok(SLVal::String(
            s.chars().nth(idx as usize).unwrap().to_string(),
          ))
        }
      }
      _ => Err(format!(
        "idx: expected (List, Int) or (String, Int), got ({:?}, {:?})",
        a, b
      )),
    }))
    .with_builtin(Builtin::binary("std", "push", |a, b| match &*a {
      SLVal::List(items) => {
        let mut new = items.clone();
        new.push(b);
        Ok(SLVal::List(new))
      }
      _ => Err(format!("push: expected a List, got {:?}", a)),
    }))
    .with_builtin(Builtin::ternary("std", "slice", |a, b, c| {
      match (&*a, &*b, &*c) {
        (SLVal::List(items), SLVal::Int(start), SLVal::Int(stop)) => {
          let len = items.len() as i64;
          let s = norm_index(*start, len);
          let e = norm_index(*stop, len);
          let s = s.clamp(0, len);
          let e = e.clamp(0, len);
          if s >= e {
            Ok(SLVal::List(vec![]))
          } else {
            Ok(SLVal::List(items[s as usize..e as usize].to_vec()))
          }
        }
        (SLVal::String(s), SLVal::Int(start), SLVal::Int(stop)) => {
          let chars: Vec<char> = s.chars().collect();
          let len = chars.len() as i64;
          let st = norm_index(*start, len).clamp(0, len) as usize;
          let en = norm_index(*stop, len).clamp(0, len) as usize;
          if st >= en {
            Ok(SLVal::String(String::new()))
          } else {
            Ok(SLVal::String(chars[st..en].iter().collect()))
          }
        }
        _ => Err(format!(
          "slice: expected (List, Int, Int) or (String, Int, Int), got ({:?}, {:?}, {:?})",
          a, b, c
        )),
      }
    }))
}

/// Normalize a possibly-negative index into a non-negative clamped index, using
/// Python's `l[start:stop]` semantics: a negative index counts from the end.
/// The result is *not* clamped to `[0, len]`; the caller does that after both
/// bounds are normalized (so `l[2:100]` works on a length-3 list).
fn norm_index(i: i64, len: i64) -> i64 {
  if i < 0 {
    i + len
  } else {
    i
  }
}
