//! AST transformations.

use std::collections::{HashMap, HashSet};

use parser::{Function, AST};

pub fn transform_closures_in_module(items: &[AST]) -> Result<Vec<AST>, String> {
  //! There isn't technically anything called a "closure" in either the runtime
  //! or compile time of Safelisp. Instead, nested functions are represented as
  //! top-level functions. Any variables they use from their definition
  //! environment are passed in as parameters. However, to allow shared
  //! mutation, they are wrapped up in Cells and all usage both inside the
  //! nested function and in the outer function is transformed to deref the
  //! contents from the cell. Then, to actually represent the "closure" (i.e.
  //! the callable object which has the environment bound to it), we
  //! PartialApply the inner function with all the cells that it uses.

  let mut result = vec![];
  for item in items {
    let out = match item {
      AST::DefineFn(func) => closurize_function(func)?,
      x => vec![x.clone()],
    };
    result.extend(out);
  }
  Ok(result)
}

fn closurize_function(outer_func: &Function) -> Result<Vec<AST>, String> {
  //! Do the following things to a top-levele function:
  //! 1. accumulate variable definitions
  //! 2. for any inner function, check for uses of outer variables, and convert them to DerefCell
  //! 3. Remove those inner functions from the outer function and emit them as (mangled) top-level
  //!    functions.
  //! 4. then, transform the outer function to wrap those inner-used variables in Cell.
  let mut top_level = vec![];
  let mut locals = hashset!{};
  locals.extend(outer_func.params.iter().cloned());
  let mut all_closure_bindings = hashmap!{};

  let code = transform_multi(outer_func.code.iter(), &mut |ast: &AST| {
    _closure_code_transform(ast, &mut locals, &mut all_closure_bindings, &mut top_level)
  })?;
  let outer_func = Function {
    name: outer_func.name.clone(),
    params: outer_func.params.clone(),
    code,
  };
  let mut outer_func = transform_outer_func(&outer_func, &all_closure_bindings)?;

  // If a closure uses one of our parameters, we need to insert a `(let param
  // (cell param))` at the top.
  let mut rebindings = vec![];
  for bindings in all_closure_bindings.values() {
    for binding in bindings {
      if outer_func.params.contains(binding) {
        rebindings.push(AST::Let(
          binding.clone(),
          Box::new(AST::Cell(Box::new(AST::Variable(binding.clone())))),
        ));
      }
    }
  }
  outer_func.code.splice(0..0, rebindings);

  top_level.push(AST::DefineFn(outer_func));
  Ok(top_level)
}

fn _closure_code_transform(
  ast: &AST,
  locals: &mut HashSet<String>,
  all_closure_bindings: &mut HashMap<String, Vec<String>>,
  top_level: &mut Vec<AST>,
) -> Result<Option<AST>, String> {
  //! This is a transformer suitable for passing to `transform`.
  //! See `closurize_function`.
  let new_ast = match ast {
    AST::Let(name, _v) => {
      locals.insert(name.clone());
      None
    }
    AST::DefineFn(inner_func) => {
      // TODO: handle name mangling / uniquification in a better way.
      let new_name = format!("{}:(closure)", inner_func.name);
      let (used_env_vars, inner_func) = transform_inner_func(inner_func, locals)?;
      all_closure_bindings.insert(inner_func.name.clone(), used_env_vars.clone());
      top_level.push(AST::DefineFn(Function {
        name: new_name.clone(),
        ..inner_func
      }));
      // Replace the definition of the function with a reference to its name.
      // Later we will transform this Variable to a PartialApply(variable, cell_args...),
      // *if* it uses variables from the environment.
      Some(AST::Variable(inner_func.name.clone()))
    }
    _ => None,
  };
  Ok(new_ast)
}

fn transform_inner_func(
  func: &Function,
  environment: &HashSet<String>,
) -> Result<(Vec<String>, Function), String> {
  //! Transform a function so that any free variables are converted to Celled parameters.
  //! Also returns the names of any variables used in the function that come from the environment.
  //! * `environment` - the names that are defined in the containing function.
  let mut locals = hashset!{};
  let mut env_vars = vec![];
  let code = {
    let mut transformer = |ast: &AST| {
      match ast {
        AST::Let(name, _v) => {
          locals.insert(name.clone());
          Ok(None)
        }
        AST::Variable(ref name) => {
          if (!locals.contains(name)) && environment.contains(name) {
            if !env_vars.contains(name) {
              env_vars.push(name.clone());
            }
            Ok(Some(AST::DerefCell(Box::new(AST::Variable(name.clone())))))
          } else {
            Ok(None)
          }
        }
        // Specifically avoid recursing into function definitions, we're only doing one at a time.
        // TODO: actually we need to closurize these too!!!???
        AST::DefineFn(_) => Ok(Some(ast.clone())),
        _ => Ok(None),
      }
    };
    transform_multi(func.code.iter(), &mut transformer)?
  };
  let mut params = vec![];
  params.extend(env_vars.iter().cloned());
  params.extend(func.params.clone());
  Ok((
    env_vars,
    Function {
      name: func.name.clone(),
      params,
      code,
    },
  ))
}

fn transform_outer_func(
  func: &Function,
  closure_bindings: &HashMap<String, Vec<String>>,
) -> Result<Function, String> {
  //! Transform a function which *contains* closures so that any variables that are
  //! 1. defined in the outer functions
  //! 2. used in any inner functions
  //! are wrapped in Cells.

  let mut all_used_free_vars = hashset!{};
  for bindings in closure_bindings.values() {
    all_used_free_vars.extend(bindings.iter().cloned());
  }
  let code = {
    let mut transformer = |ast: &AST| {
      match ast {
        // Variable definitions need to wrap their expression in Cell.
        AST::Let(name, expr) => {
          if all_used_free_vars.contains(name) {
            Ok(Some(AST::Let(
              name.clone(),
              Box::new(AST::Cell(expr.clone())),
            )))
          } else {
            Ok(None)
          }
        }
        // Variable *usage* needs to be wrapped in a DerefCell.
        AST::Variable(ref name) => {
          if all_used_free_vars.contains(name) {
            Ok(Some(AST::DerefCell(Box::new(AST::Variable(name.clone())))))
          } else if let Some(params) = closure_bindings.get(name) {
            if params.len() > 0 {
              Ok(Some(AST::PartialApply(
                Box::new(AST::Variable(format!("{}:(closure)", name))),
                params.iter().cloned().map(AST::Variable).collect(),
              )))
            } else {
              Ok(Some(AST::Variable(format!("{}:(closure)", name))))
            }
          } else {
            Ok(None)
          }
        }
        // Specifically avoid recursing into function definitions, we're only doing one at a time.
        // TODO: actually we need to closurize these too!!!???
        AST::DefineFn(_) => Ok(Some(ast.clone())),
        _ => Ok(None),
      }
    };
    transform_multi(func.code.iter(), &mut transformer)?
  };
  Ok(Function {
    name: func.name.clone(),
    params: func.params.clone(),
    code,
  })
}

// General transformation machinery

fn transform_multi<'a, T>(
  asts: impl Iterator<Item = &'a AST>,
  transformer: &'a mut T,
) -> Result<Vec<AST>, String>
where
  T: FnMut(&AST) -> Result<Option<AST>, String>,
{
  //! Transform an iterator of ASTs.
  asts.map(|ast| transform(ast, transformer)).collect()
}

pub fn transform<T>(ast: &AST, transformer: &mut T) -> Result<AST, String>
where
  T: FnMut(&AST) -> Result<Option<AST>, String>,
{
  //! Recursively transform an AST.
  //! If the visitor returns None, then the original AST is used and is then traversed.
  //! If the visitor returns a new AST, then it is used to replace the original AST.
  //! The new AST is not visited.
  let result = match transformer(ast)? {
    Some(replacement) => replacement,
    None => match ast {
      AST::Let(name, expr) => AST::Let(name.to_string(), Box::new(transform(&expr, transformer)?)),
      AST::DefineFn(func) => {
        let code = transform_multi(func.code.iter(), transformer)?;
        AST::DefineFn(Function {
          name: func.name.clone(),
          params: func.params.clone(),
          code,
        })
      }
      AST::Call(func_expr, args) => {
        let new_args = transform_multi(args.iter(), transformer)?;
        AST::Call(Box::new(transform(&func_expr, transformer)?), new_args)
      }
      AST::CallFixed(ident, args) => {
        let new_args = transform_multi(args.iter(), transformer)?;
        AST::CallFixed(ident.clone(), new_args)
      }
      AST::Cell(expr) => AST::Cell(Box::new(transform(&expr, transformer)?)),
      AST::DerefCell(expr) => AST::DerefCell(Box::new(transform(&expr, transformer)?)),
      AST::PartialApply(call_expr, args) => AST::PartialApply(
        Box::new(transform(&call_expr, transformer)?),
        transform_multi(args.iter(), transformer)?,
      ),

      AST::DeclareFn(_decl) => ast.clone(),
      AST::Variable(_ident) => ast.clone(),
      AST::Int(_num) => ast.clone(),
      AST::Float(_num) => ast.clone(),
      AST::String(_s) => ast.clone(),
      AST::FunctionRef(_mod_name, _func_name) => ast.clone(),
    },
  };
  Ok(result)
}

#[cfg(test)]
mod test {
  use super::*;
  use parser::{read_multiple, AST};

  #[test]
  fn test_transform_replacement_id() {
    let ast = AST::Let("a".to_string(), Box::new(AST::Int(42)));
    fn transformer(ast: &AST) -> Result<Option<AST>, String> {
      Ok(Some(ast.clone()))
    }
    let new_ast = transform(&ast, &mut transformer).unwrap();
    assert_eq!(new_ast, ast);
  }

  #[test]
  fn test_transform_no_replacements() {
    let ast = AST::Let("a".to_string(), Box::new(AST::Int(42)));
    fn transformer(_: &AST) -> Result<Option<AST>, String> {
      Ok(None)
    }
    let new_ast = transform(&ast, &mut transformer).unwrap();
    assert_eq!(new_ast, ast);
  }

  #[test]
  fn test_transform_no_replacement_closure() -> Result<(), String> {
    let ast = AST::Let("a".to_string(), Box::new(AST::Int(42)));
    let mut transformer = |_: &AST| Ok(None);
    assert_eq!(transform(&ast, &mut transformer)?, ast);
    Ok(())
  }

  #[test]
  fn transformed_closure() -> Result<(), String> {
    let source = "
      (fn outer ()
        (let a 1)
        (fn inner () a))";
    let asts = read_multiple(source)?;
    let new_asts = transform_closures_in_module(&asts)?;
    use parser::AST::*;
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
            Box::new(Variable("inner:(closure)".to_string())),
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
    let new_asts = transform_closures_in_module(&asts)?;
    use parser::AST::*;
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
            Box::new(Call(
              Box::new(Variable("+".to_string())),
              vec![DerefCell(Box::new(Variable("par".to_string()))), Int(1)],
            )),
          ),
          PartialApply(
            Box::new(Variable("inner:(closure)".to_string())),
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
    let new_asts = transform_closures_in_module(&asts)?;
    use parser::AST::*;
    let expected = vec![
      AST::DefineFn(Function {
        name: "inner:(closure)".to_string(),
        params: vec![],
        code: vec![Int(1)],
      }),
      AST::DefineFn(Function {
        name: "outer".to_string(),
        params: vec![],
        code: vec![Variable("inner:(closure)".to_string())],
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
    let new_asts = transform_closures_in_module(&asts)?;
    use parser::AST::*;
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
          Variable("inner:(closure)".to_string()),
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
    let new_asts = transform_closures_in_module(&asts)?;
    use parser::AST::*;
    let expected = vec![
      AST::DefineFn(Function {
        name: "inner:(closure)".to_string(),
        params: vec!["a".to_string()],
        code: vec![
          Let(
            "a".to_string(),
            Box::new(Call(
              Box::new(Variable("+".to_string())),
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
          Let("a".to_string(), Box::new(Int(1))),
          Variable("inner:(closure)".to_string()),
        ],
      }),
    ];
    assert_eq!(new_asts, expected);
    Ok(())
  }

}
