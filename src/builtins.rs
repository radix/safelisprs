//! Builtin definitions and composable host libraries.

use ::std::sync::Arc;

use crate::interpreter::{HostCtx, HostPoll, SLVal, Value};
use crate::types::Signature;

/// Deterministic random-number builtins and value construction.
pub mod rand;
/// The standard builtin library.
pub mod std;

/// A type-class-style bound that a generic builtin type variable can require.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Trait {
  /// Values support addition through the `+` builtin.
  Add,
  /// Values support subtraction through the `-` builtin.
  Sub,
  /// Values support equality comparison.
  Eq,
  /// Values support concatenation.
  Concat,
  /// Values support indexed slicing.
  Slice,
}

/// The type signature of a builtin function.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BuiltinSignature {
  /// Generic type variables and their required trait bounds.
  pub type_vars: Vec<(String, Vec<Trait>)>,
  /// Fixed positional parameter types.
  pub params: Vec<Signature>,
  /// Variadic rest parameter type, if the builtin accepts extra arguments.
  pub rest: Option<Signature>,
  /// Return type of the builtin.
  pub ret: Signature,
}

/// A compile-time description of a builtin: which module/name it lives in and
/// how many arguments it takes. `num_params` is `None` for variadic builtins.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BuiltinSpec {
  /// Module name containing the builtin.
  pub module: &'static str,
  /// Function name exported by the builtin.
  pub name: &'static str,
  /// Fixed argument count, or `None` for a variadic builtin.
  pub num_params: Option<u16>,
  /// Compile-time type signature for the builtin.
  pub signature: BuiltinSignature,
}

/// A field declared by a library-owned type constructor.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CustomFieldSpec {
  /// Field name.
  pub name: &'static str,
  /// Field type.
  pub ty: Signature,
}

/// A variant declared by a library-owned enum type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CustomVariantSpec {
  /// Variant name.
  pub name: &'static str,
  /// Variant fields, in declaration order.
  pub fields: Vec<CustomFieldSpec>,
}

/// The declaration kind of a custom SafeLisp type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CustomTypeKind {
  /// A struct type.
  Struct {
    /// Struct fields, in declaration order.
    fields: Vec<CustomFieldSpec>,
  },
  /// An enum type.
  Enum {
    /// Enum variants, in declaration order.
    variants: Vec<CustomVariantSpec>,
  },
}

/// A custom SafeLisp type supplied by a host library.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CustomTypeSpec {
  /// Module name containing the type.
  pub module: &'static str,
  /// Type name.
  pub name: &'static str,
  /// Whether the type is a struct or enum and its constructors.
  pub kind: CustomTypeKind,
}

impl CustomTypeSpec {
  /// Construct a library-owned struct type.
  pub fn struct_(
    module: &'static str,
    name: &'static str,
    fields: Vec<(&'static str, Signature)>,
  ) -> Self {
    Self {
      module,
      name,
      kind: CustomTypeKind::Struct {
        fields: custom_fields(fields),
      },
    }
  }

  /// Construct a library-owned enum type.
  pub fn enum_(
    module: &'static str,
    name: &'static str,
    variants: Vec<(&'static str, Vec<(&'static str, Signature)>)>,
  ) -> Self {
    Self {
      module,
      name,
      kind: CustomTypeKind::Enum {
        variants: variants
          .into_iter()
          .map(|(name, fields)| CustomVariantSpec {
            name,
            fields: custom_fields(fields),
          })
          .collect(),
      },
    }
  }
}

fn custom_fields(fields: Vec<(&'static str, Signature)>) -> Vec<CustomFieldSpec> {
  fields
    .into_iter()
    .map(|(name, ty)| CustomFieldSpec { name, ty })
    .collect()
}

/// Construct a [`BuiltinSignature`] from borrowed type-variable metadata.
pub fn sig(
  type_vars: &[(&str, &[Trait])],
  params: Vec<Signature>,
  rest: Option<Signature>,
  ret: Signature,
) -> BuiltinSignature {
  BuiltinSignature {
    type_vars: type_vars
      .iter()
      .map(|(name, bounds)| ((*name).to_string(), bounds.to_vec()))
      .collect(),
    params,
    rest,
    ret,
  }
}

/// A builtin's runtime handler. Takes a [`HostCtx`] (which bundles the
/// `&mut ExecRoot`, the GC `Mutation` context, and the `Package`/`Library`
/// registries) and the evaluated arguments.
///
/// The `for<'gc, 'call>` higher-ranked bound lets one `'static` handler serve
/// any execution's arena. `'gc` is the arena brand (invariant, used in `Gc`
/// and `ExecRoot`); `'call` is the short mutable borrow of the root while
/// invoking one builtin (distinct from `'gc` so the borrow checker can
/// reborrow `&'gc mut ExecRoot<'gc>` as `&'call mut ExecRoot<'gc>` without
/// affecting the inner arena brand).
type HostFn = Arc<
  dyn for<'gc, 'call> Fn(&mut HostCtx<'gc, 'call>, &[Value<'gc>]) -> Result<Value<'gc>, String>,
>;

type HostStartFn =
  Arc<dyn for<'gc, 'call> Fn(&mut HostCtx<'gc, 'call>, &[Value<'gc>]) -> Result<(), String>>;

type HostResumeFn = Arc<
  dyn for<'gc, 'call> Fn(
    &mut HostCtx<'gc, 'call>,
    Option<Value<'gc>>,
  ) -> Result<HostPoll<'gc>, String>,
>;

#[derive(Clone)]
enum BuiltinImpl {
  Sync(HostFn),
  Resumable {
    start: HostStartFn,
    resume: HostResumeFn,
  },
}

/// A builtin: metadata ([`BuiltinSpec`]) plus its host handler.
#[derive(Clone)]
pub struct Builtin {
  spec: BuiltinSpec,
  func: BuiltinImpl,
}

impl Builtin {
  /// Return this builtin's compile-time metadata.
  pub fn spec(&self) -> &BuiltinSpec {
    &self.spec
  }

  /// Invoke this builtin's handler and push the returned shared value handle.
  pub(crate) fn call<'gc, 'call>(
    &self,
    ctx: &mut HostCtx<'gc, 'call>,
    builtin_id: (u32, u32),
    args: &[Value<'gc>],
  ) -> Result<(), String> {
    match &self.func {
      BuiltinImpl::Sync(func) => {
        let result = func(ctx, args)?;
        ctx.push(result);
        Ok(())
      }
      BuiltinImpl::Resumable { start, .. } => {
        let stack_base = ctx.stack_len();
        start(ctx, args)?;
        ctx.push_host_frame_at(builtin_id, stack_base);
        Ok(())
      }
    }
  }

  pub(crate) fn resume<'gc, 'call>(
    &self,
    ctx: &mut HostCtx<'gc, 'call>,
    pending_result: Option<Value<'gc>>,
  ) -> Result<HostPoll<'gc>, String> {
    match &self.func {
      BuiltinImpl::Sync(_) => Err(format!(
        "{}::{} is not a resumable builtin",
        self.spec.module, self.spec.name
      )),
      BuiltinImpl::Resumable { resume, .. } => resume(ctx, pending_result),
    }
  }

  /// Construct a builtin that receives the full execution context and raw
  /// argument slice. `num_params` is `None` for a variadic builtin.
  ///
  /// Contextual host functions are trusted runtime extensions: SafeLisp
  /// cannot prevent them from allocating untracked memory, blocking, or
  /// otherwise affecting the process. Implementations must reserve
  /// guest-sized Rust-heap allocations through [`HostCtx::reserve_memory`].
  pub fn contextual(
    module: &'static str,
    name: &'static str,
    num_params: Option<u16>,
    signature: BuiltinSignature,
    func: impl for<'gc, 'call> Fn(&mut HostCtx<'gc, 'call>, &[Value<'gc>]) -> Result<SLVal<'gc>, String>
      + 'static,
  ) -> Self {
    Builtin {
      spec: BuiltinSpec {
        module,
        name,
        num_params,
        signature,
      },
      func: BuiltinImpl::Sync(Arc::new(move |ctx, args| {
        let value = func(ctx, args)?;
        Ok(ctx.alloc_heap(value))
      })),
    }
  }

  /// Construct a contextual builtin that returns an existing or explicitly
  /// allocated in-arena value handle. Use this when an operation should
  /// preserve object identity, such as list indexing.
  pub fn contextual_value(
    module: &'static str,
    name: &'static str,
    num_params: Option<u16>,
    signature: BuiltinSignature,
    func: impl for<'gc, 'call> Fn(&mut HostCtx<'gc, 'call>, &[Value<'gc>]) -> Result<Value<'gc>, String>
      + 'static,
  ) -> Self {
    Builtin {
      spec: BuiltinSpec {
        module,
        name,
        num_params,
        signature,
      },
      func: BuiltinImpl::Sync(Arc::new(func)),
    }
  }

  /// Construct a builtin whose callback into SafeLisp can pause and resume
  /// through the ordinary interpreter loop. The start function stores durable
  /// state on the VM stack; the resume function advances that state by one
  /// host scheduling step.
  pub fn resumable(
    module: &'static str,
    name: &'static str,
    num_params: Option<u16>,
    signature: BuiltinSignature,
    start: impl for<'gc, 'call> Fn(&mut HostCtx<'gc, 'call>, &[Value<'gc>]) -> Result<(), String>
      + 'static,
    resume: impl for<'gc, 'call> Fn(
        &mut HostCtx<'gc, 'call>,
        Option<Value<'gc>>,
      ) -> Result<HostPoll<'gc>, String>
      + 'static,
  ) -> Self {
    Builtin {
      spec: BuiltinSpec {
        module,
        name,
        num_params,
        signature,
      },
      func: BuiltinImpl::Resumable {
        start: Arc::new(start),
        resume: Arc::new(resume),
      },
    }
  }

  /// A unary (one-arg) builtin.
  pub fn unary(
    module: &'static str,
    name: &'static str,
    signature: BuiltinSignature,
    func: impl for<'gc> Fn(Value<'gc>) -> Result<Value<'gc>, String> + 'static,
  ) -> Self {
    Builtin {
      spec: BuiltinSpec {
        module,
        name,
        num_params: Some(1),
        signature,
      },
      func: BuiltinImpl::Sync(Arc::new(move |_ctx, args| func(args[0]))),
    }
  }

  /// A binary (two-arg) builtin. `func` receives `(left, right)`.
  pub fn binary(
    module: &'static str,
    name: &'static str,
    signature: BuiltinSignature,
    func: impl for<'gc> Fn(Value<'gc>, Value<'gc>) -> Result<Value<'gc>, String> + 'static,
  ) -> Self {
    Builtin {
      spec: BuiltinSpec {
        module,
        name,
        num_params: Some(2),
        signature,
      },
      func: BuiltinImpl::Sync(Arc::new(move |_ctx, args| func(args[0], args[1]))),
    }
  }

  /// A variadic builtin: it receives the whole argument slice and may be called
  /// with any number of args (including zero). `num_params` is `None`, so the
  /// interpreter uses the call-site arity (carried on `Instruction::Call` /
  /// `Instruction::CallDynamic`) to know how many args to pop.
  pub fn variadic(
    module: &'static str,
    name: &'static str,
    signature: BuiltinSignature,
    func: impl for<'gc> Fn(&[Value<'gc>]) -> Result<Value<'gc>, String> + 'static,
  ) -> Self {
    Builtin {
      spec: BuiltinSpec {
        module,
        name,
        num_params: None,
        signature,
      },
      func: BuiltinImpl::Sync(Arc::new(move |_ctx, args| func(args))),
    }
  }
}

#[derive(Clone, Default)]
struct Builtins {
  entries: Vec<Builtin>,
}

impl Builtins {
  fn new() -> Self {
    Self::default()
  }

  fn with_builtin(mut self, builtin: Builtin) -> Self {
    self.entries.push(builtin);
    self
  }

  fn iter(&self) -> impl Iterator<Item = &Builtin> {
    self.entries.iter()
  }

  fn lookup(&self, module: &str, name: &str) -> Option<&Builtin> {
    self
      .entries
      .iter()
      .find(|b| b.spec.module == module && b.spec.name == name)
  }
}

/// A complete host library: custom SafeLisp type declarations plus the builtin
/// functions whose signatures and runtime behavior may depend on them.
#[derive(Clone)]
pub struct Library {
  builtins: Builtins,
  types: Vec<CustomTypeSpec>,
  prelude: Vec<(&'static str, &'static str)>,
}

impl Library {
  /// Create an empty library.
  pub fn new() -> Self {
    Self {
      builtins: Builtins::new(),
      types: Vec::new(),
      prelude: Vec::new(),
    }
  }

  /// Register a builtin (builder style).
  pub fn with_builtin(mut self, builtin: Builtin) -> Self {
    self.builtins = self.builtins.with_builtin(builtin);
    self
  }

  /// Register a custom type (builder style).
  pub fn with_type(mut self, type_: CustomTypeSpec) -> Self {
    self.types.push(type_);
    self
  }

  /// Add one builtin function to the default lexical prelude.
  pub fn with_prelude(mut self, module: &'static str, name: &'static str) -> Self {
    if !self.prelude.contains(&(module, name)) {
      self.prelude.push((module, name));
    }
    self
  }

  /// Add every builtin in `module` to the default lexical prelude.
  pub fn with_promoted_prelude(mut self, module: &str) -> Self {
    let promoted = self
      .builtins()
      .filter(|builtin| builtin.spec().module == module)
      .map(|builtin| (builtin.spec().module, builtin.spec().name))
      .collect::<Vec<_>>();
    for entry in promoted {
      if !self.prelude.contains(&entry) {
        self.prelude.push(entry);
      }
    }
    self
  }

  /// Merge another library into this one, preserving declaration order.
  pub fn merge(mut self, other: Library) -> Self {
    self.builtins.entries.extend(other.builtins.entries);
    self.types.extend(other.types);
    self.prelude.extend(other.prelude);
    self
  }

  /// Iterate over all registered builtins.
  pub fn builtins(&self) -> impl Iterator<Item = &Builtin> {
    self.builtins.iter()
  }

  /// Iterate over all registered custom types.
  pub fn types(&self) -> impl Iterator<Item = &CustomTypeSpec> {
    self.types.iter()
  }

  /// The functions imported into lexical scope by default for this library.
  pub fn prelude(&self) -> &[(&'static str, &'static str)] {
    &self.prelude
  }

  /// Look up a builtin by `(module, name)`.
  pub(crate) fn lookup_builtin(&self, module: &str, name: &str) -> Option<&Builtin> {
    self.builtins.lookup(module, name)
  }
}

impl Default for Library {
  fn default() -> Self {
    std::library()
      .merge(rand::library())
      .with_promoted_prelude("std")
  }
}

#[cfg(test)]
mod builtins_tests;
