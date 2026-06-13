use std::collections::{HashMap, HashSet};

use crate::parser::{Function, AST};

pub fn transform_closures_in_module(module_name: &str, items: &[AST]) -> Result<Vec<AST>, String> {
  //! There isn't technically anything called a "closure" in either the runtime or compile time of
  //! Safelisp. We represent closures as more general-purpose things: top-level functions, Cells,
  //! and Partial Applications.
  //!
  //! Nested functions are lifted to top-level functions. Any variables they use from their
  //! definition environment are passed in as parameters. However, to allow shared mutation, they
  //! are wrapped up in Cells and all usage both inside the nested function and in the outer
  //! function is transformed to deref the contents from the cell. Then, to actually represent the
  //! "closure" (i.e. the callable object which has the environment bound to it), we PartialApply
  //! the inner function with all the cells that it uses.

  let mut result = vec![];
  for item in items {
    let out = match item {
      AST::DefineFn(func) => closurize_function(module_name, func)?,
      x => vec![x.clone()],
    };
    result.extend(out);
  }
  Ok(result)
}

#[derive(Debug, Clone)]
struct FunctionAnalysis {
  captures: Vec<String>,
  cell_vars: Vec<String>,
  nested: Vec<NestedFunctionAnalysis>,
}

#[derive(Debug, Clone)]
struct NestedFunctionAnalysis {
  original_name: String,
  lifted_name: String,
  analysis: FunctionAnalysis,
}

#[derive(Debug, Clone)]
struct FunctionBinding {
  lifted_name: String,
  captures: Vec<String>,
}

fn closurize_function(module_name: &str, outer_func: &Function) -> Result<Vec<AST>, String> {
  //! Do the following things to a top-levele function:
  //! 1. accumulate variable definitions
  //! 2. for any inner function, check for uses of outer variables, and convert them to DerefCell
  //! 3. Remove those inner functions from the outer function and emit them as (mangled) top-level
  //!    functions.
  //! 4. then, transform the outer function to wrap those inner-used variables in Cell.
  let analysis = analyze_function(outer_func, &hashset! {})?;
  let mut top_level = vec![];
  let outer_func = lower_function(
    module_name,
    outer_func,
    outer_func.name.clone(),
    &analysis,
    &mut top_level,
  )?;
  top_level.push(AST::DefineFn(outer_func));
  Ok(top_level)
}

fn analyze_function(
  func: &Function,
  environment: &HashSet<String>,
) -> Result<FunctionAnalysis, String> {
  let mut locals = hashset! {};
  locals.extend(func.params.iter().cloned());
  let mut captures = vec![];
  let mut cell_vars = vec![];
  let mut nested = vec![];
  for ast in &func.code {
    analyze_ast(
      ast,
      &mut locals,
      environment,
      &mut captures,
      &mut cell_vars,
      &mut nested,
    )?;
  }
  Ok(FunctionAnalysis {
    captures,
    cell_vars,
    nested,
  })
}

fn analyze_ast(
  ast: &AST,
  locals: &mut HashSet<String>,
  environment: &HashSet<String>,
  captures: &mut Vec<String>,
  cell_vars: &mut Vec<String>,
  nested: &mut Vec<NestedFunctionAnalysis>,
) -> Result<(), String> {
  match ast {
    AST::Let(name, expr) => {
      analyze_ast(expr, locals, environment, captures, cell_vars, nested)?;
      locals.insert(name.clone());
    }
    AST::DefineFn(inner_func) => {
      let mut inner_environment = environment.clone();
      inner_environment.extend(locals.iter().cloned());
      let inner_analysis = analyze_function(inner_func, &inner_environment)?;
      for capture in &inner_analysis.captures {
        if locals.contains(capture) {
          push_unique(cell_vars, capture.clone());
        } else if environment.contains(capture) {
          push_unique(captures, capture.clone());
        }
      }
      nested.push(NestedFunctionAnalysis {
        original_name: inner_func.name.clone(),
        lifted_name: mangle_closure_name(&inner_func.name),
        analysis: inner_analysis,
      });
    }
    AST::Variable(name) => {
      if !locals.contains(name) && environment.contains(name) {
        push_unique(captures, name.clone());
      }
    }
    AST::Call(callable, args) => {
      analyze_ast(callable, locals, environment, captures, cell_vars, nested)?;
      for arg in args {
        analyze_ast(arg, locals, environment, captures, cell_vars, nested)?;
      }
    }
    AST::CallFixed(_, args) => {
      for arg in args {
        analyze_ast(arg, locals, environment, captures, cell_vars, nested)?;
      }
    }
    AST::Cell(expr) | AST::DerefCell(expr) => {
      analyze_ast(expr, locals, environment, captures, cell_vars, nested)?;
    }
    AST::PartialApply(callable, args) => {
      analyze_ast(callable, locals, environment, captures, cell_vars, nested)?;
      for arg in args {
        analyze_ast(arg, locals, environment, captures, cell_vars, nested)?;
      }
    }
    AST::Import(_)
    | AST::DeclareFn(_)
    | AST::Int(_)
    | AST::Float(_)
    | AST::String(_)
    | AST::FunctionRef(_, _) => {}
  }
  Ok(())
}

fn lower_function(
  module_name: &str,
  func: &Function,
  lowered_name: String,
  analysis: &FunctionAnalysis,
  top_level: &mut Vec<AST>,
) -> Result<Function, String> {
  let mut locals = hashset! {};
  locals.extend(func.params.iter().cloned());
  let mut bindings = hashmap! {};
  let mut nested_iter = analysis.nested.iter();
  let mut code = vec![];
  for ast in &func.code {
    code.push(lower_ast(
      module_name,
      ast,
      analysis,
      &mut locals,
      &mut bindings,
      &mut nested_iter,
      top_level,
    )?);
  }

  for param in func.params.iter().rev() {
    if analysis.cell_vars.contains(param) {
      code.splice(
        0..0,
        [AST::Let(
          param.clone(),
          Box::new(AST::Cell(Box::new(AST::Variable(param.clone())))),
        )],
      );
    }
  }

  let mut params = vec![];
  params.extend(analysis.captures.iter().cloned());
  params.extend(func.params.clone());
  Ok(Function {
    name: lowered_name,
    params,
    code,
  })
}

fn lower_ast<'a>(
  module_name: &str,
  ast: &AST,
  analysis: &FunctionAnalysis,
  locals: &mut HashSet<String>,
  bindings: &mut HashMap<String, FunctionBinding>,
  nested_iter: &mut impl Iterator<Item = &'a NestedFunctionAnalysis>,
  top_level: &mut Vec<AST>,
) -> Result<AST, String> {
  match ast {
    AST::Let(name, expr) => {
      let expr = lower_ast(
        module_name,
        expr,
        analysis,
        locals,
        bindings,
        nested_iter,
        top_level,
      )?;
      locals.insert(name.clone());
      if analysis.cell_vars.contains(name) {
        Ok(AST::Let(name.clone(), Box::new(AST::Cell(Box::new(expr)))))
      } else {
        Ok(AST::Let(name.clone(), Box::new(expr)))
      }
    }
    AST::DefineFn(inner_func) => {
      let nested_analysis = nested_iter.next().ok_or_else(|| {
        format!(
          "Internal error: missing closure analysis for {}",
          inner_func.name
        )
      })?;
      let lowered = lower_function(
        module_name,
        inner_func,
        nested_analysis.lifted_name.clone(),
        &nested_analysis.analysis,
        top_level,
      )?;
      top_level.push(AST::DefineFn(lowered));
      let binding = FunctionBinding {
        lifted_name: nested_analysis.lifted_name.clone(),
        captures: nested_analysis.analysis.captures.clone(),
      };
      bindings.insert(nested_analysis.original_name.clone(), binding.clone());
      Ok(closure_expr(module_name, &binding))
    }
    AST::Variable(name) => {
      if let Some(binding) = bindings.get(name) {
        return Ok(closure_expr(module_name, binding));
      }
      if analysis.captures.contains(name) && !locals.contains(name) {
        Ok(AST::DerefCell(Box::new(AST::Variable(name.clone()))))
      } else if analysis.cell_vars.contains(name) && locals.contains(name) {
        Ok(AST::DerefCell(Box::new(AST::Variable(name.clone()))))
      } else {
        Ok(ast.clone())
      }
    }
    AST::Call(callable, args) => {
      let callable = lower_ast(
        module_name,
        callable,
        analysis,
        locals,
        bindings,
        nested_iter,
        top_level,
      )?;
      let mut new_args = vec![];
      for arg in args {
        new_args.push(lower_ast(
          module_name,
          arg,
          analysis,
          locals,
          bindings,
          nested_iter,
          top_level,
        )?);
      }
      Ok(AST::Call(Box::new(callable), new_args))
    }
    AST::CallFixed(ident, args) => {
      let mut new_args = vec![];
      for arg in args {
        new_args.push(lower_ast(
          module_name,
          arg,
          analysis,
          locals,
          bindings,
          nested_iter,
          top_level,
        )?);
      }
      Ok(AST::CallFixed(ident.clone(), new_args))
    }
    AST::Cell(expr) => Ok(AST::Cell(Box::new(lower_ast(
      module_name,
      expr,
      analysis,
      locals,
      bindings,
      nested_iter,
      top_level,
    )?))),
    AST::DerefCell(expr) => Ok(AST::DerefCell(Box::new(lower_ast(
      module_name,
      expr,
      analysis,
      locals,
      bindings,
      nested_iter,
      top_level,
    )?))),
    AST::PartialApply(callable, args) => {
      let callable = lower_ast(
        module_name,
        callable,
        analysis,
        locals,
        bindings,
        nested_iter,
        top_level,
      )?;
      let mut new_args = vec![];
      for arg in args {
        new_args.push(lower_ast(
          module_name,
          arg,
          analysis,
          locals,
          bindings,
          nested_iter,
          top_level,
        )?);
      }
      Ok(AST::PartialApply(Box::new(callable), new_args))
    }
    AST::Import(_)
    | AST::DeclareFn(_)
    | AST::Int(_)
    | AST::Float(_)
    | AST::String(_)
    | AST::FunctionRef(_, _) => Ok(ast.clone()),
  }
}

fn closure_expr(module_name: &str, binding: &FunctionBinding) -> AST {
  let func_ref = AST::FunctionRef(module_name.to_string(), binding.lifted_name.clone());
  if binding.captures.is_empty() {
    func_ref
  } else {
    AST::PartialApply(
      Box::new(func_ref),
      binding
        .captures
        .iter()
        .cloned()
        .map(AST::Variable)
        .collect(),
    )
  }
}

fn push_unique(items: &mut Vec<String>, item: String) {
  if !items.contains(&item) {
    items.push(item);
  }
}

fn mangle_closure_name(name: &str) -> String {
  format!("{}:(closure)", name)
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::parser::{read_multiple, Identifier};

  #[test]
  fn transformed_closure() -> Result<(), String> {
    let source = "
      (fn outer ()
        (let a 1)
        (fn inner () a))";
    let asts = read_multiple(source)?;
    let new_asts = transform_closures_in_module("main", &asts)?;
    use crate::parser::AST::*;
    let expected = vec![
      AST::DefineFn(Function {
        name: "inner:(closure)".to_string(),
        params: vec!["a".to_string()],
        code: vec![DerefCell(Box::new(Variable("a".to_string())))],
      }),
      AST::DefineFn(Function {
        name: "outer".to_string(),
        params: vec![],
        code: vec![
          Let("a".to_string(), Box::new(Cell(Box::new(Int(1))))),
          PartialApply(
            Box::new(FunctionRef(
              "main".to_string(),
              "inner:(closure)".to_string(),
            )),
            vec![AST::Variable("a".to_string())],
          ),
        ],
      }),
    ];
    assert_eq!(new_asts, expected);
    Ok(())
  }

  #[test]
  fn cell_pack_outer_parameters() -> Result<(), String> {
    //! When an outer function takes a parameter that is used by an inner
    //! function, it is transformed such that the parameter is rebound in a
    //! Cell.
    let source = "
      (fn outer (par)
        (let b (+ par 1))
        (fn inner () par))";
    let asts = read_multiple(source)?;
    let new_asts = transform_closures_in_module("main", &asts)?;
    use crate::parser::AST::*;
    let expected = vec![
      AST::DefineFn(Function {
        name: "inner:(closure)".to_string(),
        params: vec!["par".to_string()],
        code: vec![DerefCell(Box::new(Variable("par".to_string())))],
      }),
      AST::DefineFn(Function {
        name: "outer".to_string(),
        params: vec!["par".to_string()],
        code: vec![
          Let(
            "par".to_string(),
            Box::new(Cell(Box::new(Variable("par".to_string())))),
          ),
          Let(
            "b".to_string(),
            Box::new(CallFixed(
              Identifier::Bare("+".to_string()),
              vec![DerefCell(Box::new(Variable("par".to_string()))), Int(1)],
            )),
          ),
          PartialApply(
            Box::new(FunctionRef(
              "main".to_string(),
              "inner:(closure)".to_string(),
            )),
            vec![AST::Variable("par".to_string())],
          ),
        ],
      }),
    ];
    assert_eq!(new_asts, expected);
    Ok(())
  }

  #[test]
  fn non_closure_inner_fn() -> Result<(), String> {
    //! If an inner function doesn't use any variables from the outer function's
    //! environment, then it is not PartialApply'd.
    let source = "
      (fn outer ()
        (fn inner () 1))";
    let asts = read_multiple(source)?;
    let new_asts = transform_closures_in_module("main", &asts)?;
    use crate::parser::AST::*;
    let expected = vec![
      AST::DefineFn(Function {
        name: "inner:(closure)".to_string(),
        params: vec![],
        code: vec![Int(1)],
      }),
      AST::DefineFn(Function {
        name: "outer".to_string(),
        params: vec![],
        code: vec![FunctionRef(
          "main".to_string(),
          "inner:(closure)".to_string(),
        )],
      }),
    ];
    assert_eq!(new_asts, expected);
    Ok(())
  }

  #[test]
  fn tricksy_inner_var_non_closure() -> Result<(), String> {
    //! Inner functions which *define* variables that happen to have the same
    //! name as a variable in an outer function will not bring that outer
    //! variable in as a cell.
    let source = "
      (fn outer ()
        (let a 1)
        (fn inner ()
          (let a 2)
          a))";
    let asts = read_multiple(source)?;
    let new_asts = transform_closures_in_module("main", &asts)?;
    use crate::parser::AST::*;
    let expected = vec![
      AST::DefineFn(Function {
        name: "inner:(closure)".to_string(),
        params: vec![],
        code: vec![
          Let("a".to_string(), Box::new(Int(2))),
          Variable("a".to_string()),
        ],
      }),
      AST::DefineFn(Function {
        name: "outer".to_string(),
        params: vec![],
        code: vec![
          Let("a".to_string(), Box::new(Int(1))),
          FunctionRef("main".to_string(), "inner:(closure)".to_string()),
        ],
      }),
    ];
    assert_eq!(new_asts, expected);
    Ok(())
  }

  #[test]
  fn tricksier_inner_var_closure() -> Result<(), String> {
    //! Inner functions which use and *then* shadow outer variables will still
    //! be treated as closures, but only the variable usages *before* the
    //! rebinding will be treated as cells.
    let source = "
      (fn outer ()
        (let a 1)
        (fn inner ()
          (let a (+ a 1))
          a))";
    let asts = read_multiple(source)?;
    let new_asts = transform_closures_in_module("main", &asts)?;
    use crate::parser::AST::*;
    let expected = vec![
      AST::DefineFn(Function {
        name: "inner:(closure)".to_string(),
        params: vec!["a".to_string()],
        code: vec![
          Let(
            "a".to_string(),
            Box::new(CallFixed(
              Identifier::Bare("+".to_string()),
              vec![DerefCell(Box::new(Variable("a".to_string()))), Int(1)],
            )),
          ),
          Variable("a".to_string()),
        ],
      }),
      AST::DefineFn(Function {
        name: "outer".to_string(),
        params: vec![],
        code: vec![
          Let("a".to_string(), Box::new(AST::Cell(Box::new(Int(1))))),
          PartialApply(
            Box::new(FunctionRef(
              "main".to_string(),
              "inner:(closure)".to_string(),
            )),
            vec![AST::Variable("a".to_string())],
          ),
        ],
      }),
    ];
    assert_eq!(new_asts, expected);
    Ok(())
  }

  #[test]
  fn nested_closures() -> Result<(), String> {
    let source = "
      (fn outer ()
        (let a 1)
        (fn intermediate ()
          (fn inner () a)
        )
        intermediate
      )
    ";
    let asts = read_multiple(source)?;
    let new_asts = transform_closures_in_module("main", &asts)?;

    let expected = vec![
      AST::DefineFn(Function {
        name: "inner:(closure)".to_string(),
        params: vec!["a".to_string()],
        code: vec![AST::DerefCell(Box::new(AST::Variable("a".to_string())))],
      }),
      AST::DefineFn(Function {
        name: "intermediate:(closure)".to_string(),
        params: vec!["a".to_string()],
        code: vec![AST::PartialApply(
          Box::new(AST::FunctionRef(
            "main".to_string(),
            "inner:(closure)".to_string(),
          )),
          vec![AST::Variable("a".to_string())],
        )],
      }),
      AST::DefineFn(Function {
        name: "outer".to_string(),
        params: vec![],
        code: vec![
          AST::Let("a".to_string(), Box::new(AST::Cell(Box::new(AST::Int(1))))),
          AST::PartialApply(
            Box::new(AST::FunctionRef(
              "main".to_string(),
              "intermediate:(closure)".to_string(),
            )),
            vec![AST::Variable("a".to_string())],
          ),
          AST::PartialApply(
            Box::new(AST::FunctionRef(
              "main".to_string(),
              "intermediate:(closure)".to_string(),
            )),
            vec![AST::Variable("a".to_string())],
          ),
        ],
      }),
    ];
    assert_eq!(new_asts, expected);
    Ok(())
  }
}
