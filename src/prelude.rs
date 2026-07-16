use std::collections::HashSet;

use crate::builtins::BuiltinSpec;
use crate::parser::{ASTKind, Function, Identifier, AST};

pub fn std_prelude_from_specs(specs: &[BuiltinSpec]) -> Vec<(&str, &str)> {
  specs
    .iter()
    .filter(|spec| spec.module == "std")
    .map(|spec| (spec.module, spec.name))
    .collect()
}

pub fn resolve_module_prelude(asts: &[AST], prelude: &[(&str, &str)]) -> Result<Vec<AST>, String> {
  let module_functions = asts
    .iter()
    .filter_map(|ast| match &ast.kind {
      ASTKind::DefineFn(function) => Some(function.name.clone()),
      _ => None,
    })
    .collect::<HashSet<_>>();
  let resolver = Resolver {
    module_functions: &module_functions,
    prelude,
  };
  asts
    .iter()
    .map(|ast| resolver.resolve_top_level(ast))
    .collect()
}

struct Resolver<'a> {
  module_functions: &'a HashSet<String>,
  prelude: &'a [(&'a str, &'a str)],
}

impl Resolver<'_> {
  fn resolve_top_level(&self, ast: &AST) -> Result<AST, String> {
    match &ast.kind {
      ASTKind::DefineFn(function) => {
        let visible = HashSet::new();
        Ok(ast.with_kind(ASTKind::DefineFn(
          self.resolve_function(function, &visible)?,
        )))
      }
      _ => Ok(ast.clone()),
    }
  }

  fn resolve_function(
    &self,
    function: &Function,
    outer_visible: &HashSet<String>,
  ) -> Result<Function, String> {
    let mut visible = outer_visible.clone();
    visible.insert(function.name.clone());
    visible.extend(function.params.iter().map(|(name, _)| name.clone()));

    let mut code = Vec::with_capacity(function.code.len());
    for expr in &function.code {
      code.push(self.resolve_expr(expr, &mut visible)?);
    }

    Ok(Function {
      name: function.name.clone(),
      params: function.params.clone(),
      return_type: function.return_type.clone(),
      bounds: function.bounds.clone(),
      code,
    })
  }

  fn resolve_expr(&self, ast: &AST, visible: &mut HashSet<String>) -> Result<AST, String> {
    match &ast.kind {
      ASTKind::Let(name, annotation, expr) => {
        let expr = self.resolve_expr(expr, visible)?;
        visible.insert(name.clone());
        Ok(ast.with_kind(ASTKind::Let(
          name.clone(),
          annotation.clone(),
          Box::new(expr),
        )))
      }
      ASTKind::DefineFn(function) => {
        let function = self.resolve_function(function, visible)?;
        visible.insert(function.name.clone());
        Ok(ast.with_kind(ASTKind::DefineFn(function)))
      }
      ASTKind::CallFixed(identifier, args) => {
        let identifier = match identifier {
          Identifier::Bare(name) if self.is_visible(visible, name) => identifier.clone(),
          Identifier::Bare(name) => self
            .resolve_prelude_name(name)?
            .map(|(module, function)| {
              Identifier::Qualified(module.to_string(), function.to_string())
            })
            .unwrap_or_else(|| identifier.clone()),
          Identifier::Qualified(_, _) => identifier.clone(),
        };
        let mut resolved_args = Vec::with_capacity(args.len());
        for arg in args {
          resolved_args.push(self.resolve_expr(arg, visible)?);
        }
        Ok(ast.with_kind(ASTKind::CallFixed(identifier, resolved_args)))
      }
      ASTKind::Variable(name) => {
        if self.is_visible(visible, name) {
          Ok(ast.clone())
        } else if let Some((module, function)) = self.resolve_prelude_name(name)? {
          Ok(ast.with_kind(ASTKind::FunctionRef(
            module.to_string(),
            function.to_string(),
          )))
        } else {
          Ok(ast.clone())
        }
      }
      ASTKind::Call(callable, args) => {
        let callable = self.resolve_expr(callable, visible)?;
        let mut resolved_args = Vec::with_capacity(args.len());
        for arg in args {
          resolved_args.push(self.resolve_expr(arg, visible)?);
        }
        Ok(ast.with_kind(ASTKind::Call(Box::new(callable), resolved_args)))
      }
      ASTKind::PartialApply(callable, args) => {
        let callable = self.resolve_expr(callable, visible)?;
        let mut resolved_args = Vec::with_capacity(args.len());
        for arg in args {
          resolved_args.push(self.resolve_expr(arg, visible)?);
        }
        Ok(ast.with_kind(ASTKind::PartialApply(Box::new(callable), resolved_args)))
      }
      ASTKind::NewStruct(name, fields) => {
        let mut resolved_fields = Vec::with_capacity(fields.len());
        for (field, expr) in fields {
          resolved_fields.push((field.clone(), self.resolve_expr(expr, visible)?));
        }
        Ok(ast.with_kind(ASTKind::NewStruct(name.clone(), resolved_fields)))
      }
      ASTKind::FieldAccess(receiver, field) => {
        let receiver = self.resolve_expr(receiver, visible)?;
        Ok(ast.with_kind(ASTKind::FieldAccess(Box::new(receiver), field.clone())))
      }
      ASTKind::If(cond, then, els) => {
        let cond = self.resolve_expr(cond, visible)?;
        let mut then_visible = visible.clone();
        let mut else_visible = visible.clone();
        let then = self.resolve_expr(then, &mut then_visible)?;
        let els = self.resolve_expr(els, &mut else_visible)?;
        *visible = then_visible
          .intersection(&else_visible)
          .cloned()
          .collect::<HashSet<_>>();
        Ok(ast.with_kind(ASTKind::If(Box::new(cond), Box::new(then), Box::new(els))))
      }
      ASTKind::Block(body) => {
        let mut resolved_body = Vec::with_capacity(body.len());
        for expr in body {
          resolved_body.push(self.resolve_expr(expr, visible)?);
        }
        Ok(ast.with_kind(ASTKind::Block(resolved_body)))
      }
      ASTKind::Int(_)
      | ASTKind::Float(_)
      | ASTKind::String(_)
      | ASTKind::Bool(_)
      | ASTKind::FunctionRef(_, _)
      | ASTKind::DefineStruct(_) => Ok(ast.clone()),
    }
  }

  fn is_visible(&self, visible: &HashSet<String>, name: &str) -> bool {
    visible.contains(name) || self.module_functions.contains(name)
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

#[cfg(test)]
mod tests {
  use super::*;
  use crate::parser::read_multiple;

  fn resolve(source: &str) -> Vec<AST> {
    resolve_module_prelude(&read_multiple(source).unwrap(), &[("std", "+")]).unwrap()
  }

  #[test]
  fn rewrites_bare_fixed_call_to_qualified_call() {
    assert_eq!(
      resolve("(fn main () ->Int (+ 1 2))")[0],
      AST::DefineFn(Function {
        name: "main".to_string(),
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
        name: "main".to_string(),
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
        name: "main".to_string(),
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
        name: "main".to_string(),
        params: vec![],
        return_type: Some(crate::parser::TypeAst::Named("Int".to_string())),
        bounds: vec![],
        code: vec![AST::CallFixed(Identifier::Bare("+".to_string()), vec![])],
      })
    );
  }

  #[test]
  fn binding_in_only_one_if_branch_does_not_shadow_after_if() {
    assert_eq!(
      resolve("(fn main () ->Int (if true (let + 1) 0) (+ 1 2))")[0],
      AST::DefineFn(Function {
        name: "main".to_string(),
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
        name: "main".to_string(),
        params: vec![],
        return_type: None,
        bounds: vec![],
        code: vec![
          AST::synthetic(ASTKind::If(
            Box::new(AST::synthetic(ASTKind::Bool(true))),
            Box::new(AST::Let("+".to_string(), Box::new(AST::Int(1)))),
            Box::new(AST::Let("+".to_string(), Box::new(AST::Int(2)))),
          )),
          AST::Variable("+".to_string()),
        ],
      })
    );
  }
}
