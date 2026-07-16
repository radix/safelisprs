use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::Rc;

use crate::builtins::{BuiltinSignature, BuiltinSpec, Trait, TypeConst};
use crate::parser::{
  source_position, ASTKind, BindingId, Function, Identifier, ResolvedName, Span,
  Struct as StructAst, TypeAst, AST,
};

pub type TvRef = Rc<RefCell<TypeVar>>;

#[derive(Clone)]
pub enum Type {
  Int,
  Float,
  String,
  Bool,
  Void,
  Struct(String),
  Cell(Box<Type>),
  List(Box<Type>),
  Fn {
    params: Vec<Type>,
    rest: Option<Box<Type>>,
    ret: Box<Type>,
  },
  Var(TvRef),
}

impl Type {
  fn fixed_fn(params: Vec<Type>, ret: Type) -> Type {
    Type::Fn {
      params,
      rest: None,
      ret: Box::new(ret),
    }
  }

  fn fn_scheme_type(scheme: FnScheme) -> Type {
    Type::Fn {
      params: scheme.params,
      rest: scheme.rest.map(Box::new),
      ret: Box::new(scheme.ret),
    }
  }
}

#[derive(Clone)]
pub enum TypeVar {
  Unbound {
    id: usize,
    bounds: Vec<Trait>,
    origin: Option<String>,
  },
  Rigid {
    name: String,
    bounds: Vec<Trait>,
  },
  Link(Type),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeError {
  pub message: String,
  pub context: Vec<String>,
  pub span: Option<Span>,
}

impl TypeError {
  fn new(message: impl Into<String>) -> Self {
    Self {
      message: message.into(),
      context: Vec::new(),
      span: None,
    }
  }

  fn context(mut self, context: impl Into<String>) -> Self {
    self.context.push(context.into());
    self
  }

  fn at(mut self, span: Span) -> Self {
    if self.span.is_none() {
      self.span = Some(span);
    }
    self
  }

  pub fn render(&self, source: &str) -> String {
    match &self.span {
      Some(span) => {
        let (line, column) = source_position(source, span.start);
        format!("line {line}, column {column}: {self}")
      }
      None => self.to_string(),
    }
  }
}

impl fmt::Display for TypeError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "TypeError: {}", self.message)?;
    for context in &self.context {
      write!(f, "\n  {context}")?;
    }
    Ok(())
  }
}

impl fmt::Debug for Type {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", display_type(self))
  }
}

/// A polymorphic function type scheme.
///
/// This is the generalized type assigned to callables. `quantified` contains
/// the rigid type variables that are freshly instantiated each time the
/// function is referenced (e.g., the `forall A.` part, though this is not
/// actually exposed as a feature in SafeLisp yet).
#[derive(Clone)]
struct FnScheme {
  params: Vec<Type>,
  rest: Option<Type>,
  ret: Type,
  quantified: Vec<TvRef>,
}

#[derive(Clone)]
enum Binding {
  Mono(Type),
  PolyFn(FnScheme),
}

type Env = HashMap<BindingId, Binding>;
type TypeVars = HashMap<String, TvRef>;

pub fn typecheck(asts: &[AST], builtins: &[BuiltinSpec]) -> Result<(), TypeError> {
  typecheck_named(
    asts,
    builtins
      .iter()
      .map(|builtin| (builtin.module, builtin.name, &builtin.signature)),
  )
}

pub fn typecheck_named<'a>(
  asts: &[AST],
  builtins: impl IntoIterator<Item = (&'a str, &'a str, &'a BuiltinSignature)>,
) -> Result<(), TypeError> {
  Checker::new(builtins).check(asts)
}

struct Checker {
  schemes: HashMap<(String, String), FnScheme>,
  structs: HashMap<String, StructAst>,
  next_var: usize,
  inference_vars: Vec<TvRef>,
}

impl Checker {
  fn new<'a>(builtins: impl IntoIterator<Item = (&'a str, &'a str, &'a BuiltinSignature)>) -> Self {
    let mut checker = Self {
      schemes: HashMap::new(),
      structs: HashMap::new(),
      next_var: 0,
      inference_vars: Vec::new(),
    };
    for (module, name, signature) in builtins {
      let scheme = checker.scheme_from_builtin(signature);
      checker
        .schemes
        .insert((module.to_string(), name.to_string()), scheme);
    }
    checker
  }

  fn check(mut self, asts: &[AST]) -> Result<(), TypeError> {
    for ast in asts {
      if let ASTKind::DefineStruct(struct_) = &ast.kind {
        if self.structs.contains_key(&struct_.name) {
          return Err(
            TypeError::new(format!("duplicate struct `{}`", struct_.name)).at(ast.span.clone()),
          );
        }
        self.structs.insert(struct_.name.clone(), struct_.clone());
      };
    }

    for ast in asts {
      if let ASTKind::DefineStruct(struct_) = &ast.kind {
        self.validate_struct(struct_).map_err(|error| {
          error
            .context(format!("in struct `{}`", struct_.name))
            .at(ast.span.clone())
        })?;
      }
    }

    for ast in asts {
      let ASTKind::DefineFn(function) = &ast.kind else {
        if matches!(ast.kind, ASTKind::DefineStruct(_)) {
          continue;
        }
        return Err(
          TypeError::new("only function and struct definitions are allowed at top level")
            .at(ast.span.clone()),
        );
      };
      let key = ("main".to_string(), function.name.name.clone());
      if self.schemes.contains_key(&key) {
        return Err(
          TypeError::new(format!("duplicate function `main::{}`", function.name))
            .at(ast.span.clone()),
        );
      }
      let (scheme, _) = self
        .declared_scheme(function, &HashMap::new())
        .map_err(|error| error.at(ast.span.clone()))?;
      self.schemes.insert(key, scheme);
    }

    for ast in asts {
      if let ASTKind::DefineFn(function) = &ast.kind {
        let scheme = self.schemes[&("main".to_string(), function.name.name.clone())].clone();
        let vars = rigid_vars_by_name(&scheme.quantified);
        self
          .check_function(function, &scheme, Env::new(), vars, true)
          .map_err(|error| {
            error
              .context(format!("in function `{}`", function.name))
              .at(ast.span.clone())
          })?;
      }
    }
    Ok(())
  }

  fn validate_struct(&self, struct_: &StructAst) -> Result<(), TypeError> {
    let mut seen = HashSet::new();
    for (field, ty) in &struct_.fields {
      if !seen.insert(field) {
        return Err(TypeError::new(format!("duplicate field `{field}`")));
      }
      self.resolve_type(ty, &HashMap::new())?;
    }
    Ok(())
  }

  fn check_function(
    &mut self,
    function: &Function,
    scheme: &FnScheme,
    mut env: Env,
    type_vars: TypeVars,
    top_level: bool,
  ) -> Result<(), TypeError> {
    let checkpoint = self.inference_vars.len();
    let mut seen = HashSet::new();
    for ((name, annotation), ty) in function.params.iter().zip(&scheme.params) {
      if annotation.is_none() && top_level {
        return Err(TypeError::new(format!(
          "parameter `{name}` requires a type annotation"
        )));
      }
      if !seen.insert(name.as_str()) {
        return Err(TypeError::new(format!("duplicate parameter `{name}`")));
      }
      env.insert(name.binding, Binding::Mono(ty.clone()));
    }

    let inferred = self.infer_sequence(&mut env, &type_vars, &function.code)?;
    if !matches!(prune(&scheme.ret), Type::Void) {
      self.unify(inferred, scheme.ret.clone()).map_err(|error| {
        let span = function
          .code
          .last()
          .map(|expression| expression.span.clone())
          .unwrap_or_default();
        error.at(span)
      })?;
    }
    self.reject_unresolved(checkpoint)
  }

  fn infer_sequence(
    &mut self,
    env: &mut Env,
    type_vars: &TypeVars,
    expressions: &[AST],
  ) -> Result<Type, TypeError> {
    let mut result = Type::Void;
    let mut index = 0;
    while index < expressions.len() {
      if matches!(expressions[index].kind, ASTKind::DefineFn(_)) {
        let end = nested_function_group_end(expressions, index);
        result = self.infer_nested_function_group(env, type_vars, &expressions[index..end])?;
        index = end;
      } else {
        result = self.infer(env, type_vars, &expressions[index])?;
        index += 1;
      }
    }
    Ok(result)
  }

  fn infer_nested_function_group(
    &mut self,
    env: &mut Env,
    type_vars: &TypeVars,
    definitions: &[AST],
  ) -> Result<Type, TypeError> {
    let mut declared = Vec::with_capacity(definitions.len());
    let mut group_env = env.clone();
    for definition in definitions {
      let ASTKind::DefineFn(function) = &definition.kind else {
        unreachable!("nested function groups contain only function definitions");
      };
      let (scheme, nested_type_vars) = self
        .declared_scheme(function, type_vars)
        .map_err(|error| error.at(definition.span.clone()))?;
      group_env.insert(function.name.binding, Binding::PolyFn(scheme.clone()));
      declared.push((function, definition, scheme, nested_type_vars));
    }

    let mut result = Type::Void;
    for (function, definition, scheme, nested_type_vars) in &declared {
      self
        .check_function(
          function,
          scheme,
          group_env.clone(),
          nested_type_vars.clone(),
          false,
        )
        .map_err(|error| {
          error
            .context(format!("in nested function `{}`", function.name))
            .at(definition.span.clone())
        })?;

      let checkpoint = self.inference_vars.len();
      let instantiated = self.instantiate(scheme, Some(format!("function `{}`", function.name)));
      self.inference_vars.truncate(checkpoint);
      result = Type::fn_scheme_type(instantiated);
    }

    for (function, _, scheme, _) in declared {
      env.insert(function.name.binding, Binding::PolyFn(scheme));
    }
    Ok(result)
  }

  fn infer(&mut self, env: &mut Env, type_vars: &TypeVars, ast: &AST) -> Result<Type, TypeError> {
    self
      .infer_unlocated(env, type_vars, ast)
      .map_err(|error| error.at(ast.span.clone()))
  }

  fn infer_unlocated(
    &mut self,
    env: &mut Env,
    type_vars: &TypeVars,
    ast: &AST,
  ) -> Result<Type, TypeError> {
    match &ast.kind {
      ASTKind::Int(_) => Ok(Type::Int),
      ASTKind::Float(_) => Ok(Type::Float),
      ASTKind::String(_) => Ok(Type::String),
      ASTKind::Bool(_) => Ok(Type::Bool),
      ASTKind::Variable(name) => self.resolve_bare(env, name),
      ASTKind::FunctionRef(module, name) => self.resolve_scheme(module, name),
      ASTKind::Let(name, annotation, expression) => {
        let inferred = self.infer(env, type_vars, expression)?;
        if let Some(annotation) = annotation {
          let expected = self.resolve_type(annotation, type_vars)?;
          self
            .unify(inferred.clone(), expected)
            .map_err(|error| error.at(expression.span.clone()))?;
        }
        let binding = if let ASTKind::DefineFn(function) = &expression.kind {
          env
            .get(&function.name.binding)
            .cloned()
            .ok_or_else(|| TypeError::new("nested function was not bound after checking"))?
        } else {
          Binding::Mono(inferred.clone())
        };
        env.insert(name.binding, binding);
        Ok(inferred)
      }
      ASTKind::DefineFn(_) => {
        self.infer_nested_function_group(env, type_vars, std::slice::from_ref(ast))
      }
      ASTKind::Call(callee, args) => {
        let callee_type = self.infer(env, type_vars, callee)?;
        let arg_types = args
          .iter()
          .map(|arg| self.infer(env, type_vars, arg))
          .collect::<Result<Vec<_>, _>>()?;
        let ret = self.fresh(Some("dynamic call result".to_string()), Vec::new());
        self
          .unify(callee_type, Type::fixed_fn(arg_types, ret.clone()))
          .map_err(|error| error.at(callee.span.clone()))?;
        Ok(ret)
      }
      ASTKind::CallFixed(identifier, args) => {
        self.infer_fixed_call(env, type_vars, identifier, args)
      }
      ASTKind::NewStruct(name, fields) => self.infer_new_struct(env, type_vars, name, fields),
      ASTKind::FieldAccess(receiver, field) => {
        let receiver = self.infer(env, type_vars, receiver)?;
        self.field_type(receiver, field)
      }
      ASTKind::If(condition, then_branch, else_branch) => {
        let condition_type = self.infer(env, type_vars, condition)?;
        self
          .unify(condition_type, Type::Bool)
          .map_err(|error| error.at(condition.span.clone()))?;
        let mut then_env = env.clone();
        let mut else_env = env.clone();
        let then_type = self.infer(&mut then_env, type_vars, then_branch)?;
        let else_type = self.infer(&mut else_env, type_vars, else_branch)?;
        self
          .unify(then_type.clone(), else_type)
          .map_err(|error| error.at(else_branch.span.clone()))?;
        *env = intersect_compatible_bindings(then_env, &else_env);
        Ok(then_type)
      }
      ASTKind::Block(body) => self.infer_sequence(env, type_vars, body),
      ASTKind::PartialApply(_, _) => Err(TypeError::new(
        "internal transformed AST reached the source typechecker",
      )),
      ASTKind::DefineStruct(_) => Err(TypeError::new(
        "struct definitions are only allowed at top level",
      )),
    }
  }

  fn infer_new_struct(
    &mut self,
    env: &mut Env,
    type_vars: &TypeVars,
    name: &str,
    fields: &[(String, AST)],
  ) -> Result<Type, TypeError> {
    let struct_ = self
      .structs
      .get(name)
      .cloned()
      .ok_or_else(|| TypeError::new(format!("unknown struct `{name}`")))?;
    let mut provided = HashSet::new();
    for (field, expr) in fields {
      if !provided.insert(field.as_str()) {
        return Err(TypeError::new(format!(
          "duplicate initializer for field `{field}`"
        )));
      }
      let expected = struct_
        .fields
        .iter()
        .find(|(expected, _)| expected == field)
        .ok_or_else(|| TypeError::new(format!("unknown field `{field}` for struct `{name}`")))?;
      let actual = self.infer(env, type_vars, expr)?;
      let expected = self.resolve_type(&expected.1, type_vars)?;
      self.unify(actual, expected).map_err(|error| {
        error
          .at(expr.span.clone())
          .context(format!("while checking field `{field}` of `{name}`"))
      })?;
    }
    for (field, _) in &struct_.fields {
      if !provided.contains(field.as_str()) {
        return Err(TypeError::new(format!(
          "missing initializer for field `{field}` of `{name}`"
        )));
      }
    }
    Ok(Type::Struct(name.to_string()))
  }

  fn field_type(&self, receiver: Type, field: &str) -> Result<Type, TypeError> {
    match prune(&receiver) {
      Type::Struct(name) => {
        let struct_ = self
          .structs
          .get(&name)
          .ok_or_else(|| TypeError::new(format!("unknown struct `{name}`")))?;
        let (_, ty) = struct_
          .fields
          .iter()
          .find(|(name, _)| name == field)
          .ok_or_else(|| TypeError::new(format!("struct `{name}` has no field `{field}`")))?;
        self.resolve_type(ty, &HashMap::new())
      }
      other => Err(TypeError::new(format!(
        "field access expected a struct, got `{}`",
        display_type(&other)
      ))),
    }
  }

  fn infer_fixed_call(
    &mut self,
    env: &mut Env,
    type_vars: &TypeVars,
    identifier: &Identifier,
    args: &[AST],
  ) -> Result<Type, TypeError> {
    let (module, name) = match identifier {
      Identifier::Qualified(module, name) => (module, name),
      Identifier::Bare(name) => {
        return Err(TypeError::new(format!("unknown function `{name}`")));
      }
    };
    let label = format!("{module}::{name}");
    let scheme = self.schemes.get(&(module.clone(), name.clone())).cloned();
    let Some(scheme) = scheme else {
      return Err(TypeError::new(format!("unknown function `{label}`")));
    };

    let instantiated = self.instantiate(&scheme, Some(format!("call to `{label}`")));
    let minimum = instantiated.params.len();
    if (instantiated.rest.is_none() && args.len() != minimum)
      || (instantiated.rest.is_some() && args.len() < minimum)
    {
      let expected = if instantiated.rest.is_some() {
        format!("at least {minimum}")
      } else {
        minimum.to_string()
      };
      return Err(TypeError::new(format!(
        "call to `{label}` expects {expected} arguments, got {}",
        args.len()
      )));
    }
    for (index, arg) in args.iter().enumerate() {
      let actual = self.infer(env, type_vars, arg).map_err(|error| {
        error.context(format!(
          "while checking argument {} of call to `{label}`",
          index + 1
        ))
      })?;
      let expected = instantiated
        .params
        .get(index)
        .cloned()
        .or_else(|| instantiated.rest.clone())
        .expect("arity was checked");
      self.unify(actual, expected).map_err(|error| {
        error.at(arg.span.clone()).context(format!(
          "while checking argument {} of call to `{label}`",
          index + 1
        ))
      })?;
    }
    Ok(instantiated.ret)
  }

  fn resolve_bare(&mut self, env: &Env, name: &ResolvedName) -> Result<Type, TypeError> {
    let binding = env
      .get(&name.binding)
      .cloned()
      .ok_or_else(|| TypeError::new(format!("Unknown name `{name}`")))?;
    self.instantiate_binding(binding, Some(format!("variable `{name}`")))
  }

  fn resolve_scheme(&mut self, module: &str, name: &str) -> Result<Type, TypeError> {
    let scheme = self
      .schemes
      .get(&(module.to_string(), name.to_string()))
      .cloned()
      .ok_or_else(|| TypeError::new(format!("unknown function `{module}::{name}`")))?;
    let instantiated = self.instantiate(&scheme, Some(format!("function `{module}::{name}`")));
    Ok(Type::fn_scheme_type(instantiated))
  }

  fn declared_scheme(
    &mut self,
    function: &Function,
    enclosing: &TypeVars,
  ) -> Result<(FnScheme, TypeVars), TypeError> {
    let mut names = HashSet::new();
    for (_, annotation) in &function.params {
      let annotation = annotation.as_ref().ok_or_else(|| {
        TypeError::new(format!(
          "parameter requires a type annotation in `{}`",
          function.name
        ))
      })?;
      self.collect_type_vars(annotation, &mut names)?;
    }
    if let Some(ret) = &function.return_type {
      self.collect_type_vars(ret, &mut names)?;
    }

    let own_names: HashSet<String> = names
      .iter()
      .filter(|name| !enclosing.contains_key(*name))
      .cloned()
      .collect();
    let mut declared_bounds: HashMap<String, Vec<Trait>> = HashMap::new();
    for bound in &function.bounds {
      if !own_names.contains(&bound.var) {
        let reason = if enclosing.contains_key(&bound.var) {
          "cannot add bounds to an enclosing type variable"
        } else {
          "bound variable does not appear in the function signature"
        };
        return Err(TypeError::new(format!("{}: `{}`", reason, bound.var)));
      }
      let traits = declared_bounds.entry(bound.var.clone()).or_default();
      for name in &bound.traits {
        let trait_ = parse_trait(name)?;
        if !traits.contains(&trait_) {
          traits.push(trait_);
        }
      }
    }

    let mut vars = enclosing.clone();
    let mut quantified = Vec::new();
    let mut own_sorted: Vec<_> = own_names.into_iter().collect();
    own_sorted.sort();
    for name in own_sorted {
      let var = Rc::new(RefCell::new(TypeVar::Rigid {
        name: name.clone(),
        bounds: declared_bounds.remove(&name).unwrap_or_default(),
      }));
      vars.insert(name, var.clone());
      quantified.push(var);
    }

    let params = function
      .params
      .iter()
      .map(|(_, annotation)| self.resolve_type(annotation.as_ref().unwrap(), &vars))
      .collect::<Result<Vec<_>, _>>()?;
    let ret = function
      .return_type
      .as_ref()
      .map(|annotation| self.resolve_type(annotation, &vars))
      .transpose()?
      .unwrap_or(Type::Void);
    Ok((
      FnScheme {
        params,
        rest: None,
        ret,
        quantified,
      },
      vars,
    ))
  }

  fn scheme_from_builtin(&mut self, signature: &BuiltinSignature) -> FnScheme {
    let mut vars = HashMap::new();
    let mut quantified = Vec::new();
    for (name, bounds) in &signature.type_vars {
      let var = Rc::new(RefCell::new(TypeVar::Rigid {
        name: name.clone(),
        bounds: bounds.clone(),
      }));
      vars.insert(name.clone(), var.clone());
      quantified.push(var);
    }
    FnScheme {
      params: signature
        .params
        .iter()
        .map(|ty| type_from_const(ty, &vars))
        .collect(),
      rest: signature.rest.as_ref().map(|ty| type_from_const(ty, &vars)),
      ret: type_from_const(&signature.ret, &vars),
      quantified,
    }
  }

  fn instantiate(&mut self, scheme: &FnScheme, origin: Option<String>) -> FnScheme {
    let mut replacements = HashMap::new();
    for quantified in &scheme.quantified {
      let (name, bounds) = match &*quantified.borrow() {
        TypeVar::Rigid { name, bounds } => (name.clone(), bounds.clone()),
        _ => unreachable!("schemes quantify rigid variables"),
      };
      replacements.insert(
        Rc::as_ptr(quantified) as usize,
        self.fresh(origin.clone().or(Some(name)), bounds),
      );
    }
    FnScheme {
      params: scheme
        .params
        .iter()
        .map(|ty| replace_quantified(ty, &replacements))
        .collect(),
      rest: scheme
        .rest
        .as_ref()
        .map(|ty| replace_quantified(ty, &replacements)),
      ret: replace_quantified(&scheme.ret, &replacements),
      quantified: Vec::new(),
    }
  }

  fn instantiate_binding(
    &mut self,
    binding: Binding,
    origin: Option<String>,
  ) -> Result<Type, TypeError> {
    match binding {
      Binding::Mono(ty) => Ok(ty),
      Binding::PolyFn(scheme) => {
        let scheme = self.instantiate(&scheme, origin);
        Ok(Type::fn_scheme_type(scheme))
      }
    }
  }

  fn fresh(&mut self, origin: Option<String>, bounds: Vec<Trait>) -> Type {
    let var = Rc::new(RefCell::new(TypeVar::Unbound {
      id: self.next_var,
      bounds,
      origin,
    }));
    self.next_var += 1;
    self.inference_vars.push(var.clone());
    Type::Var(var)
  }

  fn unify(&mut self, left: Type, right: Type) -> Result<(), TypeError> {
    let left = prune(&left);
    let right = prune(&right);
    match (left, right) {
      (Type::Int, Type::Int)
      | (Type::Float, Type::Float)
      | (Type::String, Type::String)
      | (Type::Bool, Type::Bool)
      | (Type::Void, Type::Void) => Ok(()),
      (Type::Struct(a), Type::Struct(b)) if a == b => Ok(()),
      (Type::Cell(a), Type::Cell(b)) | (Type::List(a), Type::List(b)) => self.unify(*a, *b),
      (
        Type::Fn {
          params: a_params,
          rest: a_rest,
          ret: a_ret,
        },
        Type::Fn {
          params: b_params,
          rest: b_rest,
          ret: b_ret,
        },
      ) => {
        self.unify_fn_params(a_params, a_rest, b_params, b_rest)?;
        self.unify(*a_ret, *b_ret)
      }
      (Type::Var(a), Type::Var(b)) if Rc::ptr_eq(&a, &b) => Ok(()),
      (Type::Var(var), ty) => self.bind_var(var, ty),
      (ty, Type::Var(var)) => self.bind_var(var, ty),
      (actual, expected) => Err(TypeError::new(format!(
        "expected `{}`, got `{}`",
        display_type(&expected),
        display_type(&actual)
      ))),
    }
  }

  fn unify_fn_params(
    &mut self,
    a_params: Vec<Type>,
    a_rest: Option<Box<Type>>,
    b_params: Vec<Type>,
    b_rest: Option<Box<Type>>,
  ) -> Result<(), TypeError> {
    match (a_rest, b_rest) {
      (None, None) => {
        if a_params.len() != b_params.len() {
          return Err(TypeError::new(format!(
            "function arity mismatch: expected {}, got {}",
            a_params.len(),
            b_params.len()
          )));
        }
        for (a, b) in a_params.into_iter().zip(b_params) {
          self.unify(a, b)?;
        }
      }
      (Some(a_rest), None) => {
        self.unify_variadic_with_fixed(a_params, *a_rest, b_params)?;
      }
      (None, Some(b_rest)) => {
        self.unify_variadic_with_fixed(b_params, *b_rest, a_params)?;
      }
      (Some(a_rest), Some(b_rest)) => {
        if a_params.len() != b_params.len() {
          return Err(TypeError::new(format!(
            "function arity mismatch: expected at least {}, got at least {}",
            a_params.len(),
            b_params.len()
          )));
        }
        for (a, b) in a_params.into_iter().zip(b_params) {
          self.unify(a, b)?;
        }
        self.unify(*a_rest, *b_rest)?;
      }
    }
    Ok(())
  }

  fn unify_variadic_with_fixed(
    &mut self,
    params: Vec<Type>,
    rest: Type,
    fixed_params: Vec<Type>,
  ) -> Result<(), TypeError> {
    let minimum = params.len();
    if fixed_params.len() < minimum {
      return Err(TypeError::new(format!(
        "function arity mismatch: expected at least {}, got {}",
        minimum,
        fixed_params.len()
      )));
    }
    for (declared, actual) in params.into_iter().zip(fixed_params.iter().cloned()) {
      self.unify(declared, actual)?;
    }
    for actual in fixed_params.into_iter().skip(minimum) {
      self.unify(rest.clone(), actual)?;
    }
    Ok(())
  }

  fn bind_var(&mut self, var: TvRef, ty: Type) -> Result<(), TypeError> {
    let state = var.borrow().clone();
    match state {
      TypeVar::Link(link) => self.unify(link, ty),
      TypeVar::Rigid { name, bounds } => match prune(&ty) {
        Type::Var(other) if Rc::ptr_eq(&var, &other) => Ok(()),
        Type::Var(other) => {
          let other_state = other.borrow().clone();
          match other_state {
            TypeVar::Unbound {
              bounds: required, ..
            } => {
              ensure_bounds_available(&required, &bounds, &name)?;
              *other.borrow_mut() = TypeVar::Link(Type::Var(var));
              Ok(())
            }
            TypeVar::Link(link) => self.bind_var(var, link),
            TypeVar::Rigid {
              name: other_name, ..
            } => Err(TypeError::new(format!(
              "rigid type variable `{name}` does not match `{other_name}`"
            ))),
          }
        }
        concrete => Err(TypeError::new(format!(
          "rigid type variable `{name}` does not match `{}`",
          display_type(&concrete)
        ))),
      },
      TypeVar::Unbound {
        bounds,
        origin: _,
        id: _,
      } => {
        let ty = prune(&ty);
        if occurs(&var, &ty) {
          return Err(TypeError::new(format!(
            "infinite type: `{}` occurs in `{}`",
            display_type(&Type::Var(var)),
            display_type(&ty)
          )));
        }
        if let Type::Var(other) = &ty {
          let other_state = other.borrow().clone();
          match other_state {
            TypeVar::Unbound {
              bounds: other_bounds,
              origin,
              id,
            } => {
              let mut merged = other_bounds;
              for bound in bounds {
                if !merged.contains(&bound) {
                  merged.push(bound);
                }
              }
              *other.borrow_mut() = TypeVar::Unbound {
                id,
                bounds: merged,
                origin,
              };
              *var.borrow_mut() = TypeVar::Link(ty.clone());
              return Ok(());
            }
            TypeVar::Rigid {
              name,
              bounds: available,
            } => ensure_bounds_available(&bounds, &available, &name)?,
            TypeVar::Link(_) => unreachable!("prune chased links"),
          }
        } else {
          for bound in &bounds {
            if !satisfies(&ty, *bound) {
              return Err(TypeError::new(format!(
                "type `{}` does not satisfy trait `{:?}`",
                display_type(&ty),
                bound
              )));
            }
          }
        }
        *var.borrow_mut() = TypeVar::Link(ty);
        Ok(())
      }
    }
  }

  fn reject_unresolved(&self, checkpoint: usize) -> Result<(), TypeError> {
    for var in &self.inference_vars[checkpoint..] {
      if let TypeVar::Unbound { origin, .. } = &*var.borrow() {
        let hint = origin
          .as_ref()
          .map(|origin| format!(" for {origin}"))
          .unwrap_or_default();
        return Err(TypeError::new(format!(
          "type annotation needed: unresolved inference variable{hint}"
        )));
      }
    }
    Ok(())
  }

  fn resolve_type(&self, ast: &TypeAst, vars: &TypeVars) -> Result<Type, TypeError> {
    resolve_type(ast, vars, &self.structs)
  }

  fn collect_type_vars(&self, ast: &TypeAst, names: &mut HashSet<String>) -> Result<(), TypeError> {
    collect_type_vars(ast, names, &self.structs)
  }
}

fn intersect_compatible_bindings(then_env: Env, else_env: &Env) -> Env {
  then_env
    .into_iter()
    .filter(|(name, then_binding)| {
      else_env
        .get(name)
        .is_some_and(|else_binding| bindings_compatible(then_binding, else_binding))
    })
    .collect()
}

fn nested_function_group_end(expressions: &[AST], start: usize) -> usize {
  let mut names = HashSet::new();
  let mut end = start;
  while let Some(AST {
    kind: ASTKind::DefineFn(function),
    ..
  }) = expressions.get(end)
  {
    if !names.insert(function.name.as_str()) {
      break;
    }
    end += 1;
  }
  end
}

fn bindings_compatible(left: &Binding, right: &Binding) -> bool {
  match (left, right) {
    (Binding::Mono(left), Binding::Mono(right)) => types_equivalent(left, right),
    (Binding::PolyFn(left), Binding::PolyFn(right)) => schemes_equivalent(left, right),
    _ => false,
  }
}

fn schemes_equivalent(left: &FnScheme, right: &FnScheme) -> bool {
  left.params.len() == right.params.len()
    && left
      .params
      .iter()
      .zip(&right.params)
      .all(|(left, right)| types_equivalent(left, right))
    && match (&left.rest, &right.rest) {
      (Some(left), Some(right)) => types_equivalent(left, right),
      (None, None) => true,
      _ => false,
    }
    && types_equivalent(&left.ret, &right.ret)
    && left.quantified.len() == right.quantified.len()
    && left
      .quantified
      .iter()
      .zip(&right.quantified)
      .all(|(left, right)| Rc::ptr_eq(left, right))
}

fn types_equivalent(left: &Type, right: &Type) -> bool {
  match (prune(left), prune(right)) {
    (Type::Int, Type::Int)
    | (Type::Float, Type::Float)
    | (Type::String, Type::String)
    | (Type::Bool, Type::Bool)
    | (Type::Void, Type::Void) => true,
    (Type::Struct(left), Type::Struct(right)) => left == right,
    (Type::Cell(left), Type::Cell(right)) | (Type::List(left), Type::List(right)) => {
      types_equivalent(&left, &right)
    }
    (
      Type::Fn {
        params: left_params,
        rest: left_rest,
        ret: left_ret,
      },
      Type::Fn {
        params: right_params,
        rest: right_rest,
        ret: right_ret,
      },
    ) => {
      left_params.len() == right_params.len()
        && left_params
          .iter()
          .zip(&right_params)
          .all(|(left, right)| types_equivalent(left, right))
        && match (&left_rest, &right_rest) {
          (Some(left), Some(right)) => types_equivalent(left, right),
          (None, None) => true,
          _ => false,
        }
        && types_equivalent(&left_ret, &right_ret)
    }
    (Type::Var(left), Type::Var(right)) => Rc::ptr_eq(&left, &right),
    _ => false,
  }
}

fn resolve_type(
  ast: &TypeAst,
  vars: &TypeVars,
  structs: &HashMap<String, StructAst>,
) -> Result<Type, TypeError> {
  match ast {
    TypeAst::Named(name) => match name.as_str() {
      "Int" => Ok(Type::Int),
      "Float" => Ok(Type::Float),
      "String" => Ok(Type::String),
      "Bool" => Ok(Type::Bool),
      "Void" => Ok(Type::Void),
      "List" | "Cell" | "Fn" => Err(TypeError::new(format!(
        "type constructor `{name}` requires arguments"
      ))),
      _ if structs.contains_key(name) => Ok(Type::Struct(name.clone())),
      _ => vars
        .get(name)
        .cloned()
        .map(Type::Var)
        .ok_or_else(|| TypeError::new(format!("unknown type `{name}`"))),
    },
    TypeAst::Apply(name, args) => match (name.as_str(), args.as_slice()) {
      ("List", [item]) => Ok(Type::List(Box::new(resolve_type(item, vars, structs)?))),
      ("Cell", [item]) => Ok(Type::Cell(Box::new(resolve_type(item, vars, structs)?))),
      ("List" | "Cell", _) => Err(TypeError::new(format!(
        "type constructor `{name}` expects one argument, got {}",
        args.len()
      ))),
      _ => Err(TypeError::new(format!("unknown type constructor `{name}`"))),
    },
    TypeAst::Fn(params, rest, ret) => Ok(Type::Fn {
      params: params
        .iter()
        .map(|param| resolve_type(param, vars, structs))
        .collect::<Result<Vec<_>, _>>()?,
      rest: rest
        .as_ref()
        .map(|rest| resolve_type(rest, vars, structs).map(Box::new))
        .transpose()?,
      ret: Box::new(resolve_type(ret, vars, structs)?),
    }),
  }
}

fn collect_type_vars(
  ast: &TypeAst,
  names: &mut HashSet<String>,
  structs: &HashMap<String, StructAst>,
) -> Result<(), TypeError> {
  match ast {
    TypeAst::Named(name) => match name.as_str() {
      "Int" | "Float" | "String" | "Bool" | "Void" => {}
      "List" | "Cell" | "Fn" => {
        return Err(TypeError::new(format!(
          "type constructor `{name}` requires arguments"
        )))
      }
      _ if structs.contains_key(name) => {}
      _ if name.chars().next().is_some_and(char::is_uppercase) => {
        names.insert(name.clone());
      }
      _ => return Err(TypeError::new(format!("unknown type `{name}`"))),
    },
    TypeAst::Apply(name, args) => {
      if !matches!(name.as_str(), "List" | "Cell") {
        return Err(TypeError::new(format!("unknown type constructor `{name}`")));
      }
      if args.len() != 1 {
        return Err(TypeError::new(format!(
          "type constructor `{name}` expects one argument, got {}",
          args.len()
        )));
      }
      collect_type_vars(&args[0], names, structs)?;
    }
    TypeAst::Fn(params, rest, ret) => {
      for param in params {
        collect_type_vars(param, names, structs)?;
      }
      if let Some(rest) = rest {
        collect_type_vars(rest, names, structs)?;
      }
      collect_type_vars(ret, names, structs)?;
    }
  }
  Ok(())
}

fn parse_trait(name: &str) -> Result<Trait, TypeError> {
  match name {
    "Add" => Ok(Trait::Add),
    "Sub" => Ok(Trait::Sub),
    "Eq" => Ok(Trait::Eq),
    "Concat" => Ok(Trait::Concat),
    "Slice" => Ok(Trait::Slice),
    _ => Err(TypeError::new(format!("unknown trait `{name}`"))),
  }
}

pub fn satisfies(ty: &Type, trait_: Trait) -> bool {
  let ty = prune(ty);
  if trait_ == Trait::Eq {
    return true;
  }
  match (&ty, trait_) {
    (Type::Int | Type::Float, Trait::Add | Trait::Sub) => true,
    (Type::String | Type::List(_), Trait::Concat | Trait::Slice) => true,
    (Type::Var(var), required) => match &*var.borrow() {
      TypeVar::Rigid { bounds, .. } | TypeVar::Unbound { bounds, .. } => bounds.contains(&required),
      TypeVar::Link(link) => satisfies(link, required),
    },
    _ => false,
  }
}

fn ensure_bounds_available(
  required: &[Trait],
  available: &[Trait],
  rigid_name: &str,
) -> Result<(), TypeError> {
  for required in required {
    if !available.contains(required) {
      return Err(TypeError::new(format!(
        "type variable `{rigid_name}` requires trait `{required:?}` here, but its declared bounds are `{available:?}`"
      )));
    }
  }
  Ok(())
}

fn type_from_const(ty: &TypeConst, vars: &HashMap<String, TvRef>) -> Type {
  match ty {
    TypeConst::Int => Type::Int,
    TypeConst::Float => Type::Float,
    TypeConst::String => Type::String,
    TypeConst::Bool => Type::Bool,
    TypeConst::Void => Type::Void,
    TypeConst::Cell(item) => Type::Cell(Box::new(type_from_const(item, vars))),
    TypeConst::List(item) => Type::List(Box::new(type_from_const(item, vars))),
    TypeConst::Fn { params, ret } => Type::fixed_fn(
      params
        .iter()
        .map(|param| type_from_const(param, vars))
        .collect(),
      type_from_const(ret, vars),
    ),
    TypeConst::Var(name) => Type::Var(vars[name].clone()),
  }
}

fn rigid_vars_by_name(vars: &[TvRef]) -> TypeVars {
  vars
    .iter()
    .map(|var| match &*var.borrow() {
      TypeVar::Rigid { name, .. } => (name.clone(), var.clone()),
      _ => unreachable!(),
    })
    .collect()
}

fn replace_quantified(ty: &Type, replacements: &HashMap<usize, Type>) -> Type {
  match prune(ty) {
    Type::Cell(item) => Type::Cell(Box::new(replace_quantified(&item, replacements))),
    Type::List(item) => Type::List(Box::new(replace_quantified(&item, replacements))),
    Type::Fn { params, rest, ret } => Type::Fn {
      params: params
        .iter()
        .map(|param| replace_quantified(param, replacements))
        .collect(),
      rest: rest
        .as_ref()
        .map(|rest| Box::new(replace_quantified(rest, replacements))),
      ret: Box::new(replace_quantified(&ret, replacements)),
    },
    Type::Var(var) => replacements
      .get(&(Rc::as_ptr(&var) as usize))
      .cloned()
      .unwrap_or(Type::Var(var)),
    concrete => concrete,
  }
}

fn prune(ty: &Type) -> Type {
  let Type::Var(var) = ty else {
    return ty.clone();
  };
  let link = match &*var.borrow() {
    TypeVar::Link(link) => Some(link.clone()),
    _ => None,
  };
  if let Some(link) = link {
    let pruned = prune(&link);
    *var.borrow_mut() = TypeVar::Link(pruned.clone());
    pruned
  } else {
    ty.clone()
  }
}

fn occurs(needle: &TvRef, ty: &Type) -> bool {
  match prune(ty) {
    Type::Var(var) => Rc::ptr_eq(needle, &var),
    Type::Cell(item) | Type::List(item) => occurs(needle, &item),
    Type::Fn { params, rest, ret } => {
      params.iter().any(|param| occurs(needle, param))
        || rest.as_ref().is_some_and(|rest| occurs(needle, &rest))
        || occurs(needle, &ret)
    }
    _ => false,
  }
}

fn display_type(ty: &Type) -> String {
  match prune(ty) {
    Type::Int => "Int".to_string(),
    Type::Float => "Float".to_string(),
    Type::String => "String".to_string(),
    Type::Bool => "Bool".to_string(),
    Type::Void => "Void".to_string(),
    Type::Struct(name) => name,
    Type::Cell(item) => format!("(Cell {})", display_type(&item)),
    Type::List(item) => format!("(List {})", display_type(&item)),
    Type::Fn { params, rest, ret } => {
      let mut params = params.iter().map(display_type).collect::<Vec<_>>();
      if let Some(rest) = rest {
        params.push(format!("...{}", display_type(&rest)));
      }
      format!("(Fn ({}) -> {})", params.join(" "), display_type(&ret))
    }
    Type::Var(var) => match &*var.borrow() {
      TypeVar::Unbound { id, .. } => format!("?{id}"),
      TypeVar::Rigid { name, .. } => name.clone(),
      TypeVar::Link(link) => display_type(link),
    },
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::builtins::default_builtins;
  use crate::parser::read_multiple;
  use crate::prelude::resolve_module_names;

  fn check(source: &str) -> Result<(), TypeError> {
    let asts = read_multiple(source).unwrap();
    let asts = resolve_module_names("main", &asts, &[], &[]).unwrap();
    typecheck(&asts, &default_builtins().specs())
  }

  #[test]
  fn polymorphic_identity_can_be_used_at_two_types() {
    check(
      "(fn id (a:A) ->A a)
       (fn main () ->Bool (block (id 1) (std::== (id \"x\") \"x\")))",
    )
    .unwrap();
  }

  #[test]
  fn structs_typecheck_construction_and_field_access() {
    check(
      "(struct Foo x:Int y:(Cell Int))
       (fn main () ->Int
         (let foo (new Foo y:(std::cell 2) x:3))
         foo.x)",
    )
    .unwrap();
  }

  #[test]
  fn chained_struct_field_access_typechecks() {
    check(
      "(struct Point x:Int y:Int)
       (struct Box origin:Point size:Int)
       (fn main () ->Int
         (let b (new Box size:10 origin:(new Point x:4 y:5)))
         (std::+ b.origin.x b.origin.y))",
    )
    .unwrap();
  }

  #[test]
  fn struct_construction_requires_known_fields() {
    let error = check(
      "(struct Foo x:Int)
       (fn main () ->Foo
         (new Foo x:1 y:2))",
    )
    .unwrap_err();
    assert!(error.message.contains("unknown field `y`"), "{error}");
  }

  #[test]
  fn struct_construction_requires_all_fields() {
    let error = check(
      "(struct Foo x:Int y:Int)
       (fn main () ->Foo
         (new Foo x:1))",
    )
    .unwrap_err();
    assert!(
      error.message.contains("missing initializer for field `y`"),
      "{error}"
    );
  }

  #[test]
  fn missing_bound_is_rejected() {
    let error = check("(fn double (a:A) ->A (std::+ a a))").unwrap_err();
    assert!(error.message.contains("requires trait `Add`"), "{error}");
  }

  #[test]
  fn occurs_check_rejects_infinite_type() {
    let mut checker = Checker::new(std::iter::empty());
    let variable = checker.fresh(Some("test".to_string()), Vec::new());
    let error = checker
      .unify(variable.clone(), Type::List(Box::new(variable)))
      .unwrap_err();
    assert!(error.message.contains("infinite type"), "{error}");
  }

  #[test]
  fn unresolved_empty_list_is_rejected() {
    let error = check("(fn main () ->Int (let xs (std::list)) (std::len xs))").unwrap_err();
    assert!(error.message.contains("type annotation needed"), "{error}");
  }

  #[test]
  fn variadic_builtin_can_be_called_through_local_binding() {
    check(
      "(fn main () ->(List Int)
         (let make std::list)
         (make 1 2 3))",
    )
    .unwrap();
  }

  #[test]
  fn variadic_builtin_can_be_passed_to_annotated_parameter() {
    check(
      "(fn use-list (make:(Fn (...Int) -> (List Int))) ->(List Int)
         (make 1 2 3))
       (fn main () ->(List Int)
         (use-list std::list))",
    )
    .unwrap();
  }

  #[test]
  fn map_accepts_top_level_function() {
    check(
      "(fn sq (x:Int) ->Int (std::+ x x))
       (fn main () ->(List Int) (std::map (std::range 0 5) sq))",
    )
    .unwrap();
  }

  #[test]
  fn rigid_variable_cannot_be_replaced_by_a_concrete_type() {
    let error = check("(fn bad (a:A) ->A 5)").unwrap_err();
    assert!(error.message.contains("rigid type variable `A`"), "{error}");
  }

  #[test]
  fn declared_trait_bound_allows_polymorphic_builtin_use() {
    check("(fn double (a:A) ->A where ((A Add)) (std::+ a a))").unwrap();
  }

  #[test]
  fn unbound_variable_bounds_are_merged() {
    let mut checker = Checker::new(std::iter::empty());
    let add = checker.fresh(Some("add".to_string()), vec![Trait::Add]);
    let sub = checker.fresh(Some("sub".to_string()), vec![Trait::Sub]);
    checker.unify(add.clone(), sub).unwrap();
    let error = checker.unify(add, Type::String).unwrap_err();
    assert!(error.message.contains("does not satisfy trait"), "{error}");
  }

  #[test]
  fn nested_function_can_capture_enclosing_type_variable() {
    check(
      "(fn outer (a:A) ->(Fn (A) -> A)
         (fn inner (ignored:A) ->A a))",
    )
    .unwrap();
  }

  #[test]
  fn nested_declared_function_is_generalized() {
    check(
      "(fn main () ->Bool
         (let id (fn id (a:A) ->A a))
         (id 1)
         (std::== (id \"x\") \"x\"))",
    )
    .unwrap();
  }

  #[test]
  fn void_return_discards_the_final_expression_type() {
    check("(fn main () (std::+ 1 2))").unwrap();
  }

  #[test]
  fn if_condition_must_be_bool() {
    let error = check("(fn main () ->Int (if 1 2 3))").unwrap_err();
    assert!(
      error.message.contains("expected `Bool`, got `Int`"),
      "{error}"
    );
  }

  #[test]
  fn unknown_bare_value_name_is_reported_as_a_name_error() {
    let error = check("(fn main () y)").unwrap_err();
    assert_eq!(error.message, "Unknown name `y`");
  }

  #[test]
  fn bare_builtin_call_requires_prelude_resolution_before_typecheck() {
    let error = check("(fn main () ->Int (+ 1 2))").unwrap_err();
    assert_eq!(error.message, "unknown function `+`");
  }

  #[test]
  fn binding_created_in_only_one_if_branch_is_not_available_afterward() {
    let error = check(
      "(fn main () ->Int
         (if true (let x 1) (let y 2))
         y)",
    )
    .unwrap_err();
    assert_eq!(error.message, "Unknown name `y`");
  }

  #[test]
  fn nested_self_recursion_is_accepted() {
    check(
      "(fn main () ->Int
         (fn recurse (n:Int) ->Int
           (if (std::== n 0) 0 (recurse (std::- n 1))))
         (recurse 3))",
    )
    .unwrap();
  }

  #[test]
  fn nested_mutual_recursion_is_accepted() {
    check(
      "(fn main () ->Bool
         (fn even (n:Int) ->Bool
           (if (std::== n 0) true (odd (std::- n 1))))
         (fn odd (n:Int) ->Bool
           (if (std::== n 0) false (even (std::- n 1))))
         (even 4))",
    )
    .unwrap();
  }

  #[test]
  fn nested_function_is_not_visible_before_its_recursive_group() {
    let error = check(
      "(fn main () ->Int
         (later)
         (fn later () ->Int 1))",
    )
    .unwrap_err();
    assert_eq!(error.message, "unknown function `later`");
  }

  #[test]
  fn type_errors_point_to_the_offending_argument() {
    let source = "(fn main () ->Int\n  (std::+ 1\n    \"not-an-int\"))";
    let error = check(source).unwrap_err();
    let start = source.find("\"not-an-int\"").unwrap();
    assert_eq!(error.span.as_ref().map(|span| span.start), Some(start));
    assert_eq!(
      error.render(source).lines().next(),
      Some("line 3, column 5: TypeError: expected `Int`, got `String`")
    );
  }

  #[test]
  fn outer_context_does_not_replace_an_inner_error_span() {
    let source = "(fn main () ->Int\n  (std::+ 1\n    missing))";
    let error = check(source).unwrap_err();
    let start = source.find("missing").unwrap();
    assert_eq!(error.span.as_ref().map(|span| span.start), Some(start));
    assert!(
      error.render(source).starts_with("line 3, column 5:"),
      "{error}"
    );
  }
}
