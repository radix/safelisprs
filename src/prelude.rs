use std::collections::{HashMap, HashSet};

use crate::builtins::BuiltinSpec;
use crate::parser::{ASTKind, BindingId, Function, Identifier, ResolvedName, AST};

pub fn std_prelude_from_specs(specs: &[BuiltinSpec]) -> Vec<(&str, &str)> {
  specs
    .iter()
    .filter(|spec| spec.module == "std")
    .map(|spec| (spec.module, spec.name))
    .collect()
}

/// Resolve all lexical names to binding IDs and qualify visible prelude symbols.
pub fn resolve_module_names(asts: &[AST], prelude: &[(&str, &str)]) -> Result<Vec<AST>, String> {
  let mut resolver = Resolver {
    prelude,
    next_binding: 0,
  };
  resolver.resolve_module(asts)
}

struct Resolver<'a> {
  prelude: &'a [(&'a str, &'a str)],
  next_binding: u32,
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
        let identifier = match identifier {
          Identifier::Bare(name) if scope.contains_key(name.as_str()) => {
            Identifier::Bare(ResolvedName::resolved(name.as_str(), scope[name.as_str()]))
          }
          Identifier::Bare(name) => self
            .resolve_prelude_name(name.as_str())?
            .map(|(module, function)| {
              Identifier::Qualified(module.to_string(), function.to_string())
            })
            .unwrap_or_else(|| identifier.clone()),
          Identifier::Qualified(_, _) => identifier.clone(),
        };
        let mut resolved_args = Vec::with_capacity(args.len());
        for arg in args {
          resolved_args.push(self.resolve_expr(arg, scope)?);
        }
        Ok(ast.with_kind(ASTKind::CallFixed(identifier, resolved_args)))
      }
      ASTKind::Variable(name) => {
        if let Some(binding) = scope.get(name.as_str()) {
          Ok(ast.with_kind(ASTKind::Variable(ResolvedName::resolved(
            name.as_str(),
            *binding,
          ))))
        } else if let Some((module, function)) = self.resolve_prelude_name(name.as_str())? {
          Ok(ast.with_kind(ASTKind::FunctionRef(
            module.to_string(),
            function.to_string(),
          )))
        } else {
          Ok(ast.clone())
        }
      }
      ASTKind::Call(callable, args) => {
        let callable = self.resolve_expr(callable, scope)?;
        let mut resolved_args = Vec::with_capacity(args.len());
        for arg in args {
          resolved_args.push(self.resolve_expr(arg, scope)?);
        }
        Ok(ast.with_kind(ASTKind::Call(Box::new(callable), resolved_args)))
      }
      ASTKind::PartialApply(callable, args) => {
        let callable = self.resolve_expr(callable, scope)?;
        let mut resolved_args = Vec::with_capacity(args.len());
        for arg in args {
          resolved_args.push(self.resolve_expr(arg, scope)?);
        }
        Ok(ast.with_kind(ASTKind::PartialApply(Box::new(callable), resolved_args)))
      }
      ASTKind::NewStruct(name, fields) => {
        let mut resolved_fields = Vec::with_capacity(fields.len());
        for (field, expr) in fields {
          resolved_fields.push((field.clone(), self.resolve_expr(expr, scope)?));
        }
        Ok(ast.with_kind(ASTKind::NewStruct(name.clone(), resolved_fields)))
      }
      ASTKind::FieldAccess(receiver, field) => {
        let receiver = self.resolve_expr(receiver, scope)?;
        Ok(ast.with_kind(ASTKind::FieldAccess(Box::new(receiver), field.clone())))
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
      ASTKind::Int(_)
      | ASTKind::Float(_)
      | ASTKind::String(_)
      | ASTKind::Bool(_)
      | ASTKind::FunctionRef(_, _)
      | ASTKind::DefineStruct(_) => Ok(ast.clone()),
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
mod tests {
  use super::*;
  use crate::parser::{erase_bindings, read_multiple};

  fn resolve(source: &str) -> Vec<AST> {
    erase_bindings(&resolve_module_names(&read_multiple(source).unwrap(), &[("std", "+")]).unwrap())
  }

  fn resolve_with_bindings(source: &str) -> Vec<AST> {
    resolve_module_names(&read_multiple(source).unwrap(), &[("std", "+")]).unwrap()
  }

  #[test]
  fn rewrites_bare_fixed_call_to_qualified_call() {
    assert_eq!(
      resolve("(fn main () ->Int (+ 1 2))")[0],
      AST::DefineFn(Function {
        name: "main".into(),
        params: vec![],
        return_type: Some(crate::parser::TypeAst::Named("Int".to_string())),
        bounds: vec![],
        code: vec![AST::CallFixed(
          Identifier::Qualified("std".to_string(), "+".to_string()),
          vec![AST::Int(1), AST::Int(2)],
        )],
      })
    );
  }

  #[test]
  fn rewrites_bare_value_to_function_ref() {
    assert_eq!(
      resolve("(fn main () (let add +) add)")[0],
      AST::DefineFn(Function {
        name: "main".into(),
        params: vec![],
        return_type: None,
        bounds: vec![],
        code: vec![
          AST::Let(
            "add".to_string(),
            Box::new(AST::FunctionRef("std".to_string(), "+".to_string())),
          ),
          AST::Variable("add".to_string()),
        ],
      })
    );
  }

  #[test]
  fn local_name_shadows_prelude() {
    assert_eq!(
      resolve("(fn main () (let + 1) +)")[0],
      AST::DefineFn(Function {
        name: "main".into(),
        params: vec![],
        return_type: None,
        bounds: vec![],
        code: vec![
          AST::Let("+".to_string(), Box::new(AST::Int(1))),
          AST::Variable("+".to_string()),
        ],
      })
    );
  }

  #[test]
  fn same_module_function_shadows_prelude() {
    assert_eq!(
      resolve("(fn + () 1) (fn main () ->Int (+))")[1],
      AST::DefineFn(Function {
        name: "main".into(),
        params: vec![],
        return_type: Some(crate::parser::TypeAst::Named("Int".to_string())),
        bounds: vec![],
        code: vec![AST::CallFixed(Identifier::Bare("+".into()), vec![])],
      })
    );
  }

  #[test]
  fn binding_in_only_one_if_branch_does_not_shadow_after_if() {
    assert_eq!(
      resolve("(fn main () ->Int (if true (let + 1) 0) (+ 1 2))")[0],
      AST::DefineFn(Function {
        name: "main".into(),
        params: vec![],
        return_type: Some(crate::parser::TypeAst::Named("Int".to_string())),
        bounds: vec![],
        code: vec![
          AST::synthetic(ASTKind::If(
            Box::new(AST::synthetic(ASTKind::Bool(true))),
            Box::new(AST::Let("+".to_string(), Box::new(AST::Int(1)))),
            Box::new(AST::Int(0)),
          )),
          AST::CallFixed(
            Identifier::Qualified("std".to_string(), "+".to_string()),
            vec![AST::Int(1), AST::Int(2)],
          ),
        ],
      })
    );
  }

  #[test]
  fn binding_in_both_if_branches_shadows_after_if() {
    assert_eq!(
      resolve("(fn main () (if true (let + 1) (let + 2)) +)")[0],
      AST::DefineFn(Function {
        name: "main".into(),
        params: vec![],
        return_type: None,
        bounds: vec![],
        code: vec![
          AST::synthetic(ASTKind::If(
            Box::new(AST::synthetic(ASTKind::Bool(true))),
            Box::new(AST::synthetic(ASTKind::Block(vec![
              AST::Let("+".to_string(), Box::new(AST::Int(1))),
              AST::Let("+".to_string(), Box::new(AST::Variable("+".to_string())),),
            ]))),
            Box::new(AST::synthetic(ASTKind::Block(vec![
              AST::Let("+".to_string(), Box::new(AST::Int(2))),
              AST::Let("+".to_string(), Box::new(AST::Variable("+".to_string())),),
            ]))),
          )),
          AST::Variable("+".to_string()),
        ],
      })
    );
  }

  #[test]
  fn parameter_shadowing_uses_the_parameter_binding() {
    let asts = resolve_with_bindings(
      "(fn outer ()
         (fn a (b:Int) ->Int b)
         (fn b (n:Int) ->Int n)
         a)",
    );
    let ASTKind::DefineFn(outer) = &asts[0].kind else {
      panic!("expected outer function");
    };
    let ASTKind::DefineFn(a) = &outer.code[0].kind else {
      panic!("expected nested function a");
    };
    let ASTKind::DefineFn(b) = &outer.code[1].kind else {
      panic!("expected nested function b");
    };
    let ASTKind::Variable(use_) = &a.code[0].kind else {
      panic!("expected parameter reference");
    };
    assert_eq!(use_.binding, a.params[0].0.binding);
    assert_ne!(use_.binding, b.name.binding);
  }

  #[test]
  fn mutual_recursion_references_the_sibling_binding() {
    let asts = resolve_with_bindings(
      "(fn outer ()
         (fn even (n:Int) ->Bool (odd n))
         (fn odd (n:Int) ->Bool (even n))
         even)",
    );
    let ASTKind::DefineFn(outer) = &asts[0].kind else {
      panic!("expected outer function");
    };
    let ASTKind::DefineFn(even) = &outer.code[0].kind else {
      panic!("expected even");
    };
    let ASTKind::DefineFn(odd) = &outer.code[1].kind else {
      panic!("expected odd");
    };
    let ASTKind::CallFixed(Identifier::Bare(odd_ref), _) = &even.code[0].kind else {
      panic!("expected call to odd");
    };
    let ASTKind::CallFixed(Identifier::Bare(even_ref), _) = &odd.code[0].kind else {
      panic!("expected call to even");
    };
    assert_eq!(odd_ref.binding, odd.name.binding);
    assert_eq!(even_ref.binding, even.name.binding);
  }

  #[test]
  fn if_branch_bindings_share_one_joined_identity() {
    let asts = resolve_with_bindings("(fn main () ->Int (if true (let x 1) (let x 2)) x)");
    let ASTKind::DefineFn(main) = &asts[0].kind else {
      panic!("expected main");
    };
    let ASTKind::If(_, then_branch, else_branch) = &main.code[0].kind else {
      panic!("expected if");
    };
    let ASTKind::Block(then_body) = &then_branch.kind else {
      panic!("expected joined then branch");
    };
    let ASTKind::Block(else_body) = &else_branch.kind else {
      panic!("expected joined else branch");
    };
    let ASTKind::Let(then_name, _, _) = &then_body[0].kind else {
      panic!("expected source then binding");
    };
    let ASTKind::Let(then_join, _, then_source) = &then_body[1].kind else {
      panic!("expected then join binding");
    };
    let ASTKind::Let(else_name, _, _) = &else_body[0].kind else {
      panic!("expected source else binding");
    };
    let ASTKind::Let(else_join, _, else_source) = &else_body[1].kind else {
      panic!("expected else join binding");
    };
    let ASTKind::Variable(then_source) = &then_source.kind else {
      panic!("expected then source reference");
    };
    let ASTKind::Variable(else_source) = &else_source.kind else {
      panic!("expected else source reference");
    };
    let ASTKind::Variable(use_) = &main.code[1].kind else {
      panic!("expected joined reference");
    };
    assert_ne!(then_name.binding, else_name.binding);
    assert_eq!(then_source.binding, then_name.binding);
    assert_eq!(else_source.binding, else_name.binding);
    assert_eq!(then_join.binding, else_join.binding);
    assert_eq!(then_join.binding, use_.binding);
    assert_ne!(then_join.binding, then_name.binding);
    assert_ne!(then_join.binding, else_name.binding);
  }

  #[test]
  fn repeated_lets_receive_distinct_bindings() {
    let asts = resolve_with_bindings("(fn main () ->Int (let x 1) (let x 2) x)");
    let ASTKind::DefineFn(main) = &asts[0].kind else {
      panic!("expected main");
    };
    let ASTKind::Let(first, _, _) = &main.code[0].kind else {
      panic!("expected first binding");
    };
    let ASTKind::Let(second, _, _) = &main.code[1].kind else {
      panic!("expected second binding");
    };
    let ASTKind::Variable(use_) = &main.code[2].kind else {
      panic!("expected reference");
    };
    assert_ne!(first.binding, second.binding);
    assert_eq!(second.binding, use_.binding);
  }
}
