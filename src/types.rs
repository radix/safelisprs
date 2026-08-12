use std::fmt;

use crate::builtins::CustomTypeSpec;
use crate::interpreter::{HostCtx, SLVal, Value};

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

/// A host-authored Safelisp type expression used in function signatures.
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
  /// An anonymous tuple of two or more values, in positional order.
  Tuple(Vec<Signature>),
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

  /// Construct a tuple type from its element types.
  pub fn tuple(elements: Vec<Signature>) -> Self {
    Self::Tuple(elements)
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

/// A Rust type that can be converted to and from in-arena Safelisp values.
///
/// Every field type of a `#[derive(SafelispValue)]` struct or enum must
/// implement this trait. The crate provides impls for the integer and float
/// primitives, `bool`, [`String`], `()`, [`Box<T>`], and [`Vec<T>`]; derive
/// impls cover user-defined structs and enums.
///
/// Positional (tuple) fields have no Rust name, so the derive requires an
/// explicit Safelisp name via `#[safelisp(field = "name")]` on each one:
///
/// ```
/// use safelisp::SafelispValue;
///
/// #[derive(SafelispValue)]
/// #[safelisp(module = "arp")]
/// enum Dice {
///   Plus(
///     #[safelisp(field = "left")] Box<Dice>,
///     #[safelisp(field = "right")] Box<Dice>,
///   ),
/// }
/// ```
///
/// Omitting the annotation on a positional field is a compile error:
///
/// ```compile_fail
/// use safelisp::SafelispValue;
///
/// #[derive(SafelispValue)]
/// #[safelisp(module = "arp")]
/// enum Bad {
///   V(u8),
/// }
/// ```
pub trait SafelispValue: Sized {
  /// The Safelisp type expression describing this Rust type.
  fn sl_signature() -> Signature;
  /// Convert `self` into an in-arena Safelisp value.
  fn to_value<'gc>(&self, ctx: &mut HostCtx<'gc, '_>) -> Result<Value<'gc>, String>;

  /// Default depth allowed when converting data structures with
  /// [`Self::from_value`]. A type may override this if it routinely appears
  /// deeper or shallower than the default.
  const DEFAULT_FROM_VALUE_DEPTH: usize = 128;

  /// Convert a Safelisp value back into this Rust type using
  /// [`Self::DEFAULT_FROM_VALUE_DEPTH`] as the depth limit. Equivalent to
  /// calling [`Self::from_value_with_depth`] with that limit.
  fn from_value<'gc>(ctx: &HostCtx<'gc, '_>, value: Value<'gc>) -> Result<Self, String> {
    Self::from_value_with_depth(ctx, value, Self::DEFAULT_FROM_VALUE_DEPTH)
  }

  /// Convert a Safelisp value back into this Rust type with an explicit depth
  /// limit.
  ///
  /// Each level of nested derived struct or enum consumes one unit of `depth`;
  /// the conversion returns an error once `depth` reaches zero.
  fn from_value_with_depth<'gc>(
    ctx: &HostCtx<'gc, '_>,
    value: Value<'gc>,
    depth: usize,
  ) -> Result<Self, String>;
}

/// A named Safelisp type that can describe itself to the compiler.
///
/// The `#[derive(SafelispValue)]` macro implements this alongside
/// [`SafelispValue`] for structs and enums; pass [`SafelispType::type_spec`]
/// to [`Library::with_type`](crate::builtins::Library::with_type) to register
/// the type.
pub trait SafelispType: SafelispValue {
  /// The compile-time description of this type.
  fn type_spec() -> CustomTypeSpec;
}

// ---------------------------------------------------------------------------
// Primitive impls
// ---------------------------------------------------------------------------

macro_rules! impl_int {
  ($($ty:ty),* $(,)?) => {
    $(
      impl SafelispValue for $ty {
        fn sl_signature() -> Signature {
          Signature::Int
        }
        fn to_value<'gc>(&self, _ctx: &mut HostCtx<'gc, '_>) -> Result<Value<'gc>, String> {
          Ok(Value::Int(i64::from(*self)))
        }
        fn from_value_with_depth<'gc>(
          _ctx: &HostCtx<'gc, '_>,
          value: Value<'gc>,
          _depth: usize,
        ) -> Result<Self, String> {
          let n = value.as_int()?;
          <$ty>::try_from(n).map_err(|_| {
            format!("Int {n} does not fit in {}", stringify!($ty))
          })
        }
      }
    )*
  };
}

impl_int!(i8, i16, i32, i64, u8, u16, u32);

macro_rules! impl_float {
  ($($ty:ty),* $(,)?) => {
    $(
      impl SafelispValue for $ty {
        fn sl_signature() -> Signature {
          Signature::Float
        }
        fn to_value<'gc>(&self, _ctx: &mut HostCtx<'gc, '_>) -> Result<Value<'gc>, String> {
          Ok(Value::Float(f64::from(*self)))
        }
        fn from_value_with_depth<'gc>(
          _ctx: &HostCtx<'gc, '_>,
          value: Value<'gc>,
          _depth: usize,
        ) -> Result<Self, String> {
          Ok(value.as_float()? as $ty)
        }
      }
    )*
  };
}

impl_float!(f32, f64);

impl SafelispValue for bool {
  fn sl_signature() -> Signature {
    Signature::Bool
  }
  fn to_value<'gc>(&self, _ctx: &mut HostCtx<'gc, '_>) -> Result<Value<'gc>, String> {
    Ok(Value::Bool(*self))
  }
  fn from_value_with_depth<'gc>(
    _ctx: &HostCtx<'gc, '_>,
    value: Value<'gc>,
    _depth: usize,
  ) -> Result<Self, String> {
    value.as_bool()
  }
}

impl SafelispValue for () {
  fn sl_signature() -> Signature {
    Signature::Void
  }
  fn to_value<'gc>(&self, _ctx: &mut HostCtx<'gc, '_>) -> Result<Value<'gc>, String> {
    Ok(Value::Void)
  }
  fn from_value_with_depth<'gc>(
    _ctx: &HostCtx<'gc, '_>,
    value: Value<'gc>,
    _depth: usize,
  ) -> Result<Self, String> {
    value.as_void()
  }
}

impl SafelispValue for String {
  fn sl_signature() -> Signature {
    Signature::String
  }
  fn to_value<'gc>(&self, ctx: &mut HostCtx<'gc, '_>) -> Result<Value<'gc>, String> {
    Ok(ctx.alloc_heap(SLVal::String(self.clone())))
  }
  fn from_value_with_depth<'gc>(
    _ctx: &HostCtx<'gc, '_>,
    value: Value<'gc>,
    _depth: usize,
  ) -> Result<Self, String> {
    Ok(value.as_string()?.to_string())
  }
}

impl<T: SafelispValue> SafelispValue for Box<T> {
  fn sl_signature() -> Signature {
    T::sl_signature()
  }
  fn to_value<'gc>(&self, ctx: &mut HostCtx<'gc, '_>) -> Result<Value<'gc>, String> {
    (**self).to_value(ctx)
  }
  fn from_value_with_depth<'gc>(
    ctx: &HostCtx<'gc, '_>,
    value: Value<'gc>,
    depth: usize,
  ) -> Result<Self, String> {
    // `Box<T>` is a transparent host-side indirection over the same Safelisp
    // value as `T`, so it forwards the depth limit unchanged.
    Ok(Box::new(T::from_value_with_depth(ctx, value, depth)?))
  }
}

impl<T: SafelispValue> SafelispValue for Vec<T> {
  fn sl_signature() -> Signature {
    Signature::list(T::sl_signature())
  }
  fn to_value<'gc>(&self, ctx: &mut HostCtx<'gc, '_>) -> Result<Value<'gc>, String> {
    let items = self
      .iter()
      .map(|item| item.to_value(ctx))
      .collect::<Result<Vec<_>, _>>()?;
    let list = ctx.list_from_vec(items);
    Ok(ctx.alloc_heap(SLVal::List(list)))
  }
  fn from_value_with_depth<'gc>(
    ctx: &HostCtx<'gc, '_>,
    value: Value<'gc>,
    depth: usize,
  ) -> Result<Self, String> {
    let list = value.as_list()?;
    list
      .iter()
      .map(|item| T::from_value_with_depth(ctx, item, depth))
      .collect()
  }
}
