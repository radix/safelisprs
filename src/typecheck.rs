use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::Rc;

use crate::builtins::{BuiltinSignature, BuiltinSpec, Trait, TypeConst};
use crate::parser::{source_position, ASTKind, Function, Identifier, Span, TypeAst, AST};

pub type TvRef = Rc<RefCell<TypeVar>>;

#[derive(Clone)]
pub enum Type {
  Int,
  Float,
  String,
  Bool,
  Void,
  Cell(Box<Type>),
  List(Box<Type>),
  Fn(Vec<Type>, Box<Type>),
  Var(TvRef),
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

#[derive(Clone)]
struct Scheme {
  params: Vec<Type>,
  rest: Option<Type>,
  ret: Type,
  quantified: Vec<TvRef>,
}

#[derive(Clone)]
enum Binding {
  Mono(Type),
  Poly(Scheme),
  ForbiddenNestedSelf,
}

type Env = HashMap<String, Binding>;
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
  schemes: HashMap<(String, String), Scheme>,
  next_var: usize,
  inference_vars: Vec<TvRef>,
}

impl Checker {
  fn new<'a>(builtins: impl IntoIterator<Item = (&'a str, &'a str, &'a BuiltinSignature)>) -> Self {
    let mut checker = Self {
      schemes: HashMap::new(),
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
      let ASTKind::DefineFn(function) = &ast.kind else {
        return Err(
          TypeError::new("only function definitions are allowed at top level").at(ast.span.clone()),
        );
      };
      let key = ("main".to_string(), function.name.clone());
      if self.schemes.contains_key(&key) {
        return Err(
          TypeError::new(format!("duplicate function `main.{}`", function.name))
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
        let scheme = self.schemes[&("main".to_string(), function.name.clone())].clone();
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

  fn check_function(
    &mut self,
    function: &Function,
    scheme: &Scheme,
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
      if !seen.insert(name) {
        return Err(TypeError::new(format!("duplicate parameter `{name}`")));
      }
      env.insert(name.clone(), Binding::Mono(ty.clone()));
    }

    let last = function.code.len().saturating_sub(1);
    for (index, expression) in function.code.iter().enumerate() {
      let inferred = self.infer(&mut env, &type_vars, expression)?;
      if index == last && !matches!(prune(&scheme.ret), Type::Void) {
        self
          .unify(inferred, scheme.ret.clone())
          .map_err(|error| error.at(expression.span.clone()))?;
      }
    }
    self.reject_unresolved(checkpoint)
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
          let expected = resolve_type(annotation, type_vars)?;
          self
            .unify(inferred.clone(), expected)
            .map_err(|error| error.at(expression.span.clone()))?;
        }
        let binding = if let ASTKind::DefineFn(function) = &expression.kind {
          env
            .get(&function.name)
            .cloned()
            .ok_or_else(|| TypeError::new("nested function was not bound after checking"))?
        } else {
          Binding::Mono(inferred.clone())
        };
        env.insert(name.clone(), binding);
        Ok(inferred)
      }
      ASTKind::DefineFn(function) => {
        let (scheme, nested_type_vars) = self.declared_scheme(function, type_vars)?;
        let mut nested_env = env.clone();
        nested_env.insert(function.name.clone(), Binding::ForbiddenNestedSelf);
        self
          .check_function(function, &scheme, nested_env, nested_type_vars, false)
          .map_err(|error| error.context(format!("in nested function `{}`", function.name)))?;
        // The definition itself is a declared polymorphic value. Its result
        // gets an instantiation for expression typing, but those variables are
        // generalized by the declaration and must not be reported as
        // ambiguous monomorphic locals.
        let checkpoint = self.inference_vars.len();
        let result = self.instantiate(&scheme, Some(format!("function `{}`", function.name)));
        self.inference_vars.truncate(checkpoint);
        env.insert(function.name.clone(), Binding::Poly(scheme));
        Ok(Type::Fn(result.params, Box::new(result.ret)))
      }
      ASTKind::Call(callee, args) => {
        let callee_type = self.infer(env, type_vars, callee)?;
        let arg_types = args
          .iter()
          .map(|arg| self.infer(env, type_vars, arg))
          .collect::<Result<Vec<_>, _>>()?;
        let ret = self.fresh(Some("dynamic call result".to_string()), Vec::new());
        self
          .unify(callee_type, Type::Fn(arg_types, Box::new(ret.clone())))
          .map_err(|error| error.at(callee.span.clone()))?;
        Ok(ret)
      }
      ASTKind::CallFixed(identifier, args) => {
        self.infer_fixed_call(env, type_vars, identifier, args)
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
      ASTKind::Block(body) => {
        let mut result = Type::Void;
        for expression in body {
          result = self.infer(env, type_vars, expression)?;
        }
        Ok(result)
      }
      ASTKind::Cell(_)
      | ASTKind::DerefCell(_)
      | ASTKind::SetCell(_, _)
      | ASTKind::PartialApply(_, _) => Err(TypeError::new(
        "internal transformed AST reached the source typechecker",
      )),
    }
  }

  fn infer_fixed_call(
    &mut self,
    env: &mut Env,
    type_vars: &TypeVars,
    identifier: &Identifier,
    args: &[AST],
  ) -> Result<Type, TypeError> {
    let label = match identifier {
      Identifier::Bare(name) => name.clone(),
      Identifier::Qualified(module, name) => format!("{module}.{name}"),
    };

    if let Identifier::Bare(name) = identifier {
      if let Some(binding) = env.get(name).cloned() {
        let callee = self.instantiate_binding(binding, Some(format!("call to `{label}`")))?;
        let arg_types = args
          .iter()
          .enumerate()
          .map(|(index, arg)| {
            self.infer(env, type_vars, arg).map_err(|error| {
              error.context(format!(
                "while checking argument {} of call to `{label}`",
                index + 1
              ))
            })
          })
          .collect::<Result<Vec<_>, _>>()?;
        let ret = self.fresh(Some(format!("result of call to `{label}`")), Vec::new());
        self
          .unify(callee, Type::Fn(arg_types, Box::new(ret.clone())))
          .map_err(|error| error.context(format!("while checking call to `{label}`")))?;
        return Ok(ret);
      }
    }

    let scheme = match identifier {
      Identifier::Bare(name) => self
        .schemes
        .get(&("main".to_string(), name.clone()))
        .cloned()
        .or_else(|| {
          let mut matches = self
            .schemes
            .iter()
            .filter(|((_, candidate), _)| candidate == name)
            .map(|(_, scheme)| scheme.clone());
          let first = matches.next()?;
          matches.next().is_none().then_some(first)
        }),
      Identifier::Qualified(module, name) => {
        self.schemes.get(&(module.clone(), name.clone())).cloned()
      }
    }
    .ok_or_else(|| TypeError::new(format!("unknown function `{label}`")))?;

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

  fn resolve_bare(&mut self, env: &Env, name: &str) -> Result<Type, TypeError> {
    if let Some(binding) = env.get(name).cloned() {
      return self.instantiate_binding(binding, Some(format!("variable `{name}`")));
    }
    self.resolve_scheme("main", name)
  }

  fn resolve_scheme(&mut self, module: &str, name: &str) -> Result<Type, TypeError> {
    let scheme = self
      .schemes
      .get(&(module.to_string(), name.to_string()))
      .cloned()
      .ok_or_else(|| TypeError::new(format!("unknown function `{module}.{name}`")))?;
    let instantiated = self.instantiate(&scheme, Some(format!("function `{module}.{name}`")));
    Ok(Type::Fn(instantiated.params, Box::new(instantiated.ret)))
  }

  fn declared_scheme(
    &mut self,
    function: &Function,
    enclosing: &TypeVars,
  ) -> Result<(Scheme, TypeVars), TypeError> {
    let mut names = HashSet::new();
    for (_, annotation) in &function.params {
      let annotation = annotation.as_ref().ok_or_else(|| {
        TypeError::new(format!(
          "parameter requires a type annotation in `{}`",
          function.name
        ))
      })?;
      collect_type_vars(annotation, &mut names)?;
    }
    if let Some(ret) = &function.return_type {
      collect_type_vars(ret, &mut names)?;
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
      .map(|(_, annotation)| resolve_type(annotation.as_ref().unwrap(), &vars))
      .collect::<Result<Vec<_>, _>>()?;
    let ret = function
      .return_type
      .as_ref()
      .map(|annotation| resolve_type(annotation, &vars))
      .transpose()?
      .unwrap_or(Type::Void);
    Ok((
      Scheme {
        params,
        rest: None,
        ret,
        quantified,
      },
      vars,
    ))
  }

  fn scheme_from_builtin(&mut self, signature: &BuiltinSignature) -> Scheme {
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
    Scheme {
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

  fn instantiate(&mut self, scheme: &Scheme, origin: Option<String>) -> Scheme {
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
    Scheme {
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
      Binding::Poly(scheme) => {
        let scheme = self.instantiate(&scheme, origin);
        Ok(Type::Fn(scheme.params, Box::new(scheme.ret)))
      }
      Binding::ForbiddenNestedSelf => Err(TypeError::new(
        "nested functions cannot refer to themselves recursively",
      )),
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
      (Type::Cell(a), Type::Cell(b)) | (Type::List(a), Type::List(b)) => self.unify(*a, *b),
      (Type::Fn(a_params, a_ret), Type::Fn(b_params, b_ret)) => {
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

fn bindings_compatible(left: &Binding, right: &Binding) -> bool {
  match (left, right) {
    (Binding::Mono(left), Binding::Mono(right)) => types_equivalent(left, right),
    (Binding::Poly(left), Binding::Poly(right)) => schemes_equivalent(left, right),
    (Binding::ForbiddenNestedSelf, Binding::ForbiddenNestedSelf) => true,
    _ => false,
  }
}

fn schemes_equivalent(left: &Scheme, right: &Scheme) -> bool {
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
    (Type::Cell(left), Type::Cell(right)) | (Type::List(left), Type::List(right)) => {
      types_equivalent(&left, &right)
    }
    (Type::Fn(left_params, left_ret), Type::Fn(right_params, right_ret)) => {
      left_params.len() == right_params.len()
        && left_params
          .iter()
          .zip(&right_params)
          .all(|(left, right)| types_equivalent(left, right))
        && types_equivalent(&left_ret, &right_ret)
    }
    (Type::Var(left), Type::Var(right)) => Rc::ptr_eq(&left, &right),
    _ => false,
  }
}

fn resolve_type(ast: &TypeAst, vars: &TypeVars) -> Result<Type, TypeError> {
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
      _ => vars
        .get(name)
        .cloned()
        .map(Type::Var)
        .ok_or_else(|| TypeError::new(format!("unknown type `{name}`"))),
    },
    TypeAst::Apply(name, args) => match (name.as_str(), args.as_slice()) {
      ("List", [item]) => Ok(Type::List(Box::new(resolve_type(item, vars)?))),
      ("Cell", [item]) => Ok(Type::Cell(Box::new(resolve_type(item, vars)?))),
      ("List" | "Cell", _) => Err(TypeError::new(format!(
        "type constructor `{name}` expects one argument, got {}",
        args.len()
      ))),
      _ => Err(TypeError::new(format!("unknown type constructor `{name}`"))),
    },
    TypeAst::Fn(params, ret) => Ok(Type::Fn(
      params
        .iter()
        .map(|param| resolve_type(param, vars))
        .collect::<Result<Vec<_>, _>>()?,
      Box::new(resolve_type(ret, vars)?),
    )),
  }
}

fn collect_type_vars(ast: &TypeAst, names: &mut HashSet<String>) -> Result<(), TypeError> {
  match ast {
    TypeAst::Named(name) => match name.as_str() {
      "Int" | "Float" | "String" | "Bool" | "Void" => {}
      "List" | "Cell" | "Fn" => {
        return Err(TypeError::new(format!(
          "type constructor `{name}` requires arguments"
        )))
      }
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
      collect_type_vars(&args[0], names)?;
    }
    TypeAst::Fn(params, ret) => {
      for param in params {
        collect_type_vars(param, names)?;
      }
      collect_type_vars(ret, names)?;
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
    TypeConst::Fn { params, ret } => Type::Fn(
      params
        .iter()
        .map(|param| type_from_const(param, vars))
        .collect(),
      Box::new(type_from_const(ret, vars)),
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
    Type::Fn(params, ret) => Type::Fn(
      params
        .iter()
        .map(|param| replace_quantified(param, replacements))
        .collect(),
      Box::new(replace_quantified(&ret, replacements)),
    ),
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
    Type::Fn(params, ret) => {
      params.iter().any(|param| occurs(needle, param)) || occurs(needle, &ret)
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
    Type::Cell(item) => format!("(Cell {})", display_type(&item)),
    Type::List(item) => format!("(List {})", display_type(&item)),
    Type::Fn(params, ret) => format!(
      "(Fn ({}) -> {})",
      params
        .iter()
        .map(display_type)
        .collect::<Vec<_>>()
        .join(" "),
      display_type(&ret)
    ),
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

  fn check(source: &str) -> Result<(), TypeError> {
    let asts = read_multiple(source).unwrap();
    typecheck(&asts, &default_builtins().specs())
  }

  #[test]
  fn polymorphic_identity_can_be_used_at_two_types() {
    check(
      "(fn id (a:A) ->A a)
       (fn main () ->Bool (block (id 1) (std.== (id \"x\") \"x\")))",
    )
    .unwrap();
  }

  #[test]
  fn missing_bound_is_rejected() {
    let error = check("(fn double (a:A) ->A (std.+ a a))").unwrap_err();
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
    let error = check("(fn main () ->Int (let xs (std.list)) (std.len xs))").unwrap_err();
    assert!(error.message.contains("type annotation needed"), "{error}");
  }

  #[test]
  fn map_accepts_top_level_function() {
    check(
      "(fn sq (x:Int) ->Int (std.+ x x))
       (fn main () ->(List Int) (std.map (std.range 0 5) sq))",
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
    check("(fn double (a:A) ->A where ((A Add)) (std.+ a a))").unwrap();
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
         (std.== (id \"x\") \"x\"))",
    )
    .unwrap();
  }

  #[test]
  fn void_return_discards_the_final_expression_type() {
    check("(fn main () (std.+ 1 2))").unwrap();
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
  fn binding_created_in_only_one_if_branch_is_not_available_afterward() {
    let error = check(
      "(fn main () ->Int
         (if true (let x 1) (let y 2))
         y)",
    )
    .unwrap_err();
    assert!(
      error.message.contains("unknown function `main.y`"),
      "{error}"
    );
  }

  #[test]
  fn nested_self_recursion_is_rejected() {
    let error = check(
      "(fn main ()
         (fn recurse (n:Int) ->Int (recurse n)))",
    )
    .unwrap_err();
    assert!(
      error.message.contains("cannot refer to themselves"),
      "{error}"
    );
  }

  #[test]
  fn type_errors_point_to_the_offending_argument() {
    let source = "(fn main () ->Int\n  (std.+ 1\n    \"not-an-int\"))";
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
    let source = "(fn main () ->Int\n  (std.+ 1\n    missing))";
    let error = check(source).unwrap_err();
    let start = source.find("missing").unwrap();
    assert_eq!(error.span.as_ref().map(|span| span.start), Some(start));
    assert!(
      error.render(source).starts_with("line 3, column 5:"),
      "{error}"
    );
  }
}
