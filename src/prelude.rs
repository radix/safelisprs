use std::collections::{HashMap, HashSet};

use crate::builtins::BuiltinSpec;
use crate::parser::{
  try_map_ast_children, ASTKind, BindingId, Function, Identifier, ImplDef, MatchArm, MatchPattern,
  ResolvedName, AST,
};

pub fn std_prelude_from_specs(specs: &[BuiltinSpec]) -> Vec<(&str, &str)> {
  specs
    .iter()
    .filter(|spec| spec.module == "std")
    .map(|spec| (spec.module, spec.name))
    .collect()
}

/// Resolve all lexical names to binding IDs and qualify module/prelude symbols.
pub fn resolve_module_names(
  module_name: &str,
  asts: &[AST],
  prelude: &[(&str, &str)],
  module_symbols: &[&str],
) -> Result<Vec<AST>, String> {
  let mut resolver = Resolver {
    module_name,
    prelude,
    module_symbols: module_symbols.iter().copied().collect(),
    next_binding: 0,
    module_functions: HashSet::new(),
  };
  resolver.resolve_module(asts)
}

struct Resolver<'a> {
  module_name: &'a str,
  prelude: &'a [(&'a str, &'a str)],
  module_symbols: HashSet<&'a str>,
  next_binding: u32,
  module_functions: HashSet<BindingId>,
}

type Scope = HashMap<String, BindingId>;

impl Resolver<'_> {
  fn resolve_module(&mut self, asts: &[AST]) -> Result<Vec<AST>, String> {
    let mut module_scope = Scope::new();
    let top_level_names = asts
      .iter()
      .map(|ast| match &ast.kind {
        ASTKind::DefineFn(function) => {
          let name = self.fresh_name(function.name.as_str());
          module_scope.insert(name.name.clone(), name.binding);
          self.module_functions.insert(name.binding);
          Some(name)
        }
        _ => None,
      })
      .collect::<Vec<_>>();

    asts
      .iter()
      .zip(top_level_names)
      .map(|(ast, name)| match (&ast.kind, name) {
        (ASTKind::DefineFn(function), Some(name)) => Ok(ast.with_kind(ASTKind::DefineFn(
          self.resolve_function(function, name, &module_scope)?,
        ))),
        (ASTKind::DefineImpl(impl_), _) => Ok(ast.with_kind(ASTKind::DefineImpl(
          self.resolve_impl(impl_, &module_scope)?,
        ))),
        _ => Ok(ast.clone()),
      })
      .collect()
  }

  fn fresh_name(&mut self, name: &str) -> ResolvedName {
    let binding = BindingId::resolved(self.next_binding);
    self.next_binding += 1;
    ResolvedName::resolved(name, binding)
  }

  fn resolve_function(
    &mut self,
    function: &Function,
    name: ResolvedName,
    outer_scope: &Scope,
  ) -> Result<Function, String> {
    let mut scope = outer_scope.clone();
    scope.insert(name.name.clone(), name.binding);

    let mut params = Vec::with_capacity(function.params.len());
    for (param, annotation) in &function.params {
      let param = self.fresh_name(param.as_str());
      scope.insert(param.name.clone(), param.binding);
      params.push((param, annotation.clone()));
    }

    Ok(Function {
      name,
      params,
      return_type: function.return_type.clone(),
      bounds: function.bounds.clone(),
      code: self.resolve_sequence(&function.code, &mut scope)?,
    })
  }

  fn resolve_impl(&mut self, impl_: &ImplDef, outer_scope: &Scope) -> Result<ImplDef, String> {
    let mut methods = Vec::with_capacity(impl_.methods.len());
    for method in &impl_.methods {
      let name = self.fresh_name(method.name.as_str());
      methods.push(self.resolve_function(method, name, outer_scope)?);
    }
    Ok(ImplDef {
      trait_name: impl_.trait_name.clone(),
      target: impl_.target.clone(),
      methods,
    })
  }

  fn resolve_sequence(
    &mut self,
    expressions: &[AST],
    scope: &mut Scope,
  ) -> Result<Vec<AST>, String> {
    let mut resolved = Vec::with_capacity(expressions.len());
    let mut index = 0;
    while index < expressions.len() {
      if matches!(expressions[index].kind, ASTKind::DefineFn(_)) {
        let end = nested_function_group_end(expressions, index);
        let definitions = &expressions[index..end];
        let mut names = Vec::with_capacity(definitions.len());
        for definition in definitions {
          let ASTKind::DefineFn(function) = &definition.kind else {
            unreachable!("nested function groups contain only functions");
          };
          let name = self.fresh_name(function.name.as_str());
          scope.insert(name.name.clone(), name.binding);
          names.push(name);
        }
        for (definition, name) in definitions.iter().zip(names) {
          let ASTKind::DefineFn(function) = &definition.kind else {
            unreachable!("nested function groups contain only functions");
          };
          resolved.push(definition.with_kind(ASTKind::DefineFn(
            self.resolve_function(function, name, scope)?,
          )));
        }
        index = end;
      } else {
        resolved.push(self.resolve_expr(&expressions[index], scope)?);
        index += 1;
      }
    }
    Ok(resolved)
  }

  fn resolve_expr(&mut self, ast: &AST, scope: &mut Scope) -> Result<AST, String> {
    match &ast.kind {
      ASTKind::Let(name, annotation, expr) => {
        let expr = self.resolve_expr(expr, scope)?;
        let name = self.fresh_name(name.as_str());
        scope.insert(name.name.clone(), name.binding);
        Ok(ast.with_kind(ASTKind::Let(name, annotation.clone(), Box::new(expr))))
      }
      ASTKind::DefineFn(function) => {
        let name = self.fresh_name(function.name.as_str());
        scope.insert(name.name.clone(), name.binding);
        Ok(ast.with_kind(ASTKind::DefineFn(
          self.resolve_function(function, name, scope)?,
        )))
      }
      ASTKind::CallFixed(identifier, args) => {
        let mut resolved_args = Vec::with_capacity(args.len());
        for arg in args {
          resolved_args.push(self.resolve_expr(arg, scope)?);
        }
        let kind = match identifier {
          Identifier::Bare(name) => {
            if let Some(binding) = scope.get(name.as_str()) {
              let name = ResolvedName::resolved(name.as_str(), *binding);
              if self.module_functions.contains(binding) {
                ASTKind::CallFixed(
                  Identifier::Qualified(self.module_name.to_string(), name.name),
                  resolved_args,
                )
              } else {
                ASTKind::Call(
                  Box::new(AST::new(ASTKind::Variable(name), ast.span.clone())),
                  resolved_args,
                )
              }
            } else if let Some((module, function)) = self.resolve_external_name(name.as_str())? {
              ASTKind::CallFixed(Identifier::Qualified(module, function), resolved_args)
            } else {
              ASTKind::CallFixed(identifier.clone(), resolved_args)
            }
          }
          Identifier::Qualified(_, _) => ASTKind::CallFixed(identifier.clone(), resolved_args),
        };
        Ok(ast.with_kind(kind))
      }
      ASTKind::Variable(name) => {
        if let Some(binding) = scope.get(name.as_str()) {
          if self.module_functions.contains(binding) {
            Ok(ast.with_kind(ASTKind::FunctionRef(
              self.module_name.to_string(),
              name.name.clone(),
            )))
          } else {
            Ok(ast.with_kind(ASTKind::Variable(ResolvedName::resolved(
              name.as_str(),
              *binding,
            ))))
          }
        } else if let Some((module, function)) = self.resolve_external_name(name.as_str())? {
          Ok(ast.with_kind(ASTKind::FunctionRef(module, function)))
        } else {
          Ok(ast.clone())
        }
      }
      ASTKind::If(cond, then, els) => {
        let cond = self.resolve_expr(cond, scope)?;
        let baseline = scope.clone();
        let mut then_scope = baseline.clone();
        let mut else_scope = baseline.clone();
        let mut then = self.resolve_expr(then, &mut then_scope)?;
        let mut els = self.resolve_expr(els, &mut else_scope)?;
        let mut joined = Scope::new();
        for (name, then_binding) in &then_scope {
          let Some(else_binding) = else_scope.get(name) else {
            continue;
          };
          if then_binding == else_binding {
            joined.insert(name.clone(), *then_binding);
            continue;
          }
          let joined_name = self.fresh_name(name);
          append_binding_copy(
            &mut then,
            ResolvedName::resolved(name, *then_binding),
            joined_name.clone(),
          );
          append_binding_copy(
            &mut els,
            ResolvedName::resolved(name, *else_binding),
            joined_name.clone(),
          );
          joined.insert(name.clone(), joined_name.binding);
        }
        *scope = joined;
        Ok(ast.with_kind(ASTKind::If(Box::new(cond), Box::new(then), Box::new(els))))
      }
      ASTKind::Block(body) => {
        Ok(ast.with_kind(ASTKind::Block(self.resolve_sequence(body, scope)?)))
      }
      ASTKind::Match(scrutinee, arms) => {
        let scrutinee = self.resolve_expr(scrutinee, scope)?;
        let mut resolved_arms = Vec::with_capacity(arms.len());
        for arm in arms {
          let mut arm_scope = scope.clone();
          let pattern = match &arm.pattern {
            MatchPattern::Variant { variant, fields } => {
              let mut resolved_fields = Vec::with_capacity(fields.len());
              for field in fields {
                let field = self.fresh_name(field.as_str());
                arm_scope.insert(field.name.clone(), field.binding);
                resolved_fields.push(field);
              }
              MatchPattern::Variant {
                variant: variant.clone(),
                fields: resolved_fields,
              }
            }
            MatchPattern::Default => MatchPattern::Default,
          };
          resolved_arms.push(MatchArm {
            pattern,
            body: self.resolve_expr(&arm.body, &mut arm_scope)?,
          });
        }
        Ok(ast.with_kind(ASTKind::Match(Box::new(scrutinee), resolved_arms)))
      }
      _ => try_map_ast_children(ast, |child| self.resolve_expr(child, scope)),
    }
  }

  fn resolve_prelude_name(&self, name: &str) -> Result<Option<(&str, &str)>, String> {
    let mut matches = self
      .prelude
      .iter()
      .copied()
      .filter(|(_, prelude_name)| *prelude_name == name);
    let Some(first) = matches.next() else {
      return Ok(None);
    };
    if let Some(second) = matches.next() {
      return Err(format!(
        "ambiguous prelude function `{name}`: {}::{} and {}::{}",
        first.0, first.1, second.0, second.1
      ));
    }
    Ok(Some(first))
  }

  fn resolve_external_name(&self, name: &str) -> Result<Option<(String, String)>, String> {
    if self.module_symbols.contains(name) {
      Ok(Some((self.module_name.to_string(), name.to_string())))
    } else {
      Ok(
        self
          .resolve_prelude_name(name)?
          .map(|(module, function)| (module.to_string(), function.to_string())),
      )
    }
  }
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

fn append_binding_copy(branch: &mut AST, source: ResolvedName, joined: ResolvedName) {
  let span = branch.span.clone();
  let original = branch.clone();
  let copy = AST::new(
    ASTKind::Let(
      joined,
      None,
      Box::new(AST::new(ASTKind::Variable(source), span.clone())),
    ),
    span.clone(),
  );
  *branch = AST::new(ASTKind::Block(vec![original, copy]), span);
}

#[cfg(test)]
mod prelude_tests;
