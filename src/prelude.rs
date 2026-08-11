use std::collections::{HashMap, HashSet};

use crate::parser::{
  try_map_ast_children, ASTKind, BindingId, Function, Identifier, MatchArm, MatchPattern,
  ResolvedName, AST,
};

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
    locals: HashSet::new(),
  };
  resolver.resolve_module(asts)
}

struct Resolver<'a> {
  module_name: &'a str,
  prelude: &'a [(&'a str, &'a str)],
  module_symbols: HashSet<&'a str>,
  next_binding: u32,
  module_functions: HashSet<BindingId>,
  locals: HashSet<BindingId>,
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
        _ => Ok(ast.clone()),
      })
      .collect()
  }

  fn fresh_name(&mut self, name: &str) -> ResolvedName {
    let binding = BindingId::resolved(self.next_binding);
    self.next_binding += 1;
    self.locals.insert(binding);
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

    // `locals` tracks bindings in the *current* function. Reset it
    // at the function boundary so that `let`/`shd` checks only see locals of
    // this function: a `let` may shadow bindings from enclosing functions or
    // module-level functions, while `shd` may only reassign current-function
    // locals (never creating a capture of an outer binding).
    let prev_locals = std::mem::take(&mut self.locals);

    let mut params = Vec::with_capacity(function.params.len());
    for (param, annotation) in &function.params {
      let param = self.fresh_name(param.as_str());
      scope.insert(param.name.clone(), param.binding);
      params.push((param, annotation.clone()));
    }

    let code_result = self.resolve_sequence(&function.code, &mut scope);
    self.locals = prev_locals;
    let code = code_result?;

    Ok(Function {
      name,
      params,
      return_type: function.return_type.clone(),
      bounds: function.bounds.clone(),
      code,
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
        if let Some(binding) = scope.get(name.as_str()) {
          if self.locals.contains(binding) {
            return Err(format!(
              "`let` cannot bind `{name}` because it is already in scope; use `shd` to reassign an existing binding"
            ));
          }
        }
        let expr = self.resolve_expr(expr, scope)?;
        let name = self.fresh_name(name.as_str());
        scope.insert(name.name.clone(), name.binding);
        Ok(ast.with_kind(ASTKind::Let(name, annotation.clone(), Box::new(expr))))
      }
      ASTKind::Shd(name, annotation, expr) => {
        let binding = match scope.get(name.as_str()) {
          Some(binding) if self.locals.contains(binding) => *binding,
          _ => {
            return Err(format!(
              "`shd` cannot reassign `{name}` because it is not bound in the local scope"
            ))
          }
        };
        let expr = self.resolve_expr(expr, scope)?;
        Ok(ast.with_kind(ASTKind::Let(
          ResolvedName::resolved(name.as_str(), binding),
          annotation.clone(),
          Box::new(expr),
        )))
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
        // Branches are resolved in cloned scopes and then discarded: names
        // introduced with `let` inside a branch stay branch-local, and `shd`
        // reuses existing bindings (whose scope mappings are already present),
        // so the outer scope is unchanged by either branch.
        let mut then_scope = scope.clone();
        let then = self.resolve_expr(then, &mut then_scope)?;
        let els = match els {
          Some(els) => {
            let mut else_scope = scope.clone();
            let els = self.resolve_expr(els, &mut else_scope)?;
            Some(Box::new(els))
          }
          None => None,
        };
        Ok(ast.with_kind(ASTKind::If(Box::new(cond), Box::new(then), els)))
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
      ASTKind::For(name, iterable, body) => {
        let iterable = self.resolve_expr(iterable, scope)?;
        let name = self.fresh_name(name.as_str());
        let mut body_scope = scope.clone();
        body_scope.insert(name.name.clone(), name.binding);
        let body = self.resolve_sequence(body, &mut body_scope)?;
        Ok(ast.with_kind(ASTKind::For(name, Box::new(iterable), body)))
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

#[cfg(test)]
mod prelude_tests;
