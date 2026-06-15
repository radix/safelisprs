use std::collections::HashSet;

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

/// Lifted function definitions and the capture facts needed by the parent.
struct TransformResult {
  /// Function definitions meant to be lifted to the top-level. This includes all
  /// nested functions and the function transformed by this result.
  lifted: Vec<AST>,
  /// Variables that a function *uses* from outer environment
  captures: Vec<String>,
}

fn closurize_function(module_name: &str, outer_func: &Function) -> Result<Vec<AST>, String> {
  //! Transform a function into a simpler form where nested functions are lifted
  //! to the top level and their captured environment is threaded in as hidden
  //! parameters.
  let result = transform_function(
    module_name,
    outer_func,
    outer_func.name.clone(),
    &HashSet::new(),
  )?;
  Ok(result.lifted)
}

fn transform_function(
  module_name: &str,
  func: &Function,
  transformed_name: String,
  environment: &HashSet<String>,
) -> Result<TransformResult, String> {
  //! Rewrite a function into simpler AST that the compiler already knows how to
  //! turn into bytecode, while also returning the capture info its parent
  //! needs.
  //!
  //! During transformation:
  //! - captured variables become hidden leading parameters
  //! - parameters/local bindings that are captured by nested functions are wrapped in `Cell`
  //! - any use of these captured variables is wrapped in `DerefCell`
  //! - nested `fn` definitions are lifted into `lifted` as separate functions
  //! - the original nested `fn` expression becomes `let name (partial-apply func [captures...])`
  let mut locals = hashset! {};
  locals.extend(func.params.iter().cloned());
  let mut lifted = vec![];
  let mut captures = vec![];
  let mut cell_vars = hashset! {};
  let mut code = vec![];
  for ast in &func.code {
    code.push(transform_ast(
      module_name,
      ast,
      environment,
      &mut locals,
      &mut captures,
      &mut cell_vars,
      &mut lifted,
    )?);
  }

  let mut patch_locals = hashset! {};
  patch_locals.extend(func.params.iter().cloned());
  // We have to patch cell accesses in a successive step since we don't know
  // which locals have been wrapped in cells until after transformation.
  let mut code = patch_cell_accesses(&code, &captures, &cell_vars, &mut patch_locals)?;

  for param in func.params.iter().rev() {
    if cell_vars.contains(param) {
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
  params.extend(captures.iter().cloned());
  params.extend(func.params.clone());
  lifted.push(AST::DefineFn(Function {
    name: transformed_name,
    params,
    code,
  }));
  Ok(TransformResult { lifted, captures })
}

fn transform_ast(
  module_name: &str,
  ast: &AST,
  environment: &HashSet<String>,
  locals: &mut HashSet<String>,
  captures: &mut Vec<String>,
  cell_vars: &mut HashSet<String>,
  lifted: &mut Vec<AST>,
) -> Result<AST, String> {
  //! Do closure transformations on one expression inside a function body,
  //! recording captures as they are discovered.
  match ast {
    AST::Let(name, expr) => {
      let expr = transform_ast(
        module_name,
        expr,
        environment,
        locals,
        captures,
        cell_vars,
        lifted,
      )?;
      locals.insert(name.clone());
      Ok(AST::Let(name.clone(), Box::new(expr)))
    }
    AST::DefineFn(inner_func) => {
      let mut inner_environment = environment.clone();
      inner_environment.extend(locals.iter().cloned());
      let TransformResult {
        lifted: transformed_lifted,
        captures: transformed_captures,
      } = transform_function(
        module_name,
        inner_func,
        mangle_closure_name(&inner_func.name),
        &inner_environment,
      )?;
      for capture in &transformed_captures {
        if locals.contains(capture) {
          cell_vars.insert(capture.clone());
        } else if environment.contains(capture) {
          push_unique(captures, capture.clone());
        }
      }
      lifted.extend(transformed_lifted);
      locals.insert(inner_func.name.clone());
      Ok(AST::Let(
        inner_func.name.clone(),
        Box::new(closure_expr(
          module_name,
          &mangle_closure_name(&inner_func.name),
          &transformed_captures,
        )),
      ))
    }
    AST::Variable(name) => {
      if !locals.contains(name) && environment.contains(name) {
        push_unique(captures, name.clone());
      }
      Ok(ast.clone())
    }
    AST::Call(callable, args) => {
      let callable = transform_ast(
        module_name,
        callable,
        environment,
        locals,
        captures,
        cell_vars,
        lifted,
      )?;
      let mut new_args = vec![];
      for arg in args {
        new_args.push(transform_ast(
          module_name,
          arg,
          environment,
          locals,
          captures,
          cell_vars,
          lifted,
        )?);
      }
      Ok(AST::Call(Box::new(callable), new_args))
    }
    AST::CallFixed(ident, args) => {
      if let crate::parser::Identifier::Bare(name) = ident {
        if !locals.contains(name) && environment.contains(name) {
          push_unique(captures, name.clone());
        }
      }
      let mut new_args = vec![];
      for arg in args {
        new_args.push(transform_ast(
          module_name,
          arg,
          environment,
          locals,
          captures,
          cell_vars,
          lifted,
        )?);
      }
      Ok(AST::CallFixed(ident.clone(), new_args))
    }
    AST::Cell(expr) => Ok(AST::Cell(Box::new(transform_ast(
      module_name,
      expr,
      environment,
      locals,
      captures,
      cell_vars,
      lifted,
    )?))),
    AST::DerefCell(expr) => Ok(AST::DerefCell(Box::new(transform_ast(
      module_name,
      expr,
      environment,
      locals,
      captures,
      cell_vars,
      lifted,
    )?))),
    AST::PartialApply(callable, args) => {
      let callable = transform_ast(
        module_name,
        callable,
        environment,
        locals,
        captures,
        cell_vars,
        lifted,
      )?;
      let mut new_args = vec![];
      for arg in args {
        new_args.push(transform_ast(
          module_name,
          arg,
          environment,
          locals,
          captures,
          cell_vars,
          lifted,
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

fn patch_cell_accesses(
  asts: &[AST],
  captures: &[String],
  cell_vars: &HashSet<String>,
  locals: &mut HashSet<String>,
) -> Result<Vec<AST>, String> {
  //! Replace any identifier accesses for identifiers that were replaced with
  //! cells to use DerefCell.
  asts
    .iter()
    .map(|ast| patch_cell_access(ast, captures, cell_vars, locals))
    .collect()
}

fn patch_cell_access(
  ast: &AST,
  captures: &[String],
  cell_vars: &HashSet<String>,
  locals: &mut HashSet<String>,
) -> Result<AST, String> {
  match ast {
    AST::Let(name, expr) => {
      let expr = patch_cell_access(expr, captures, cell_vars, locals)?;
      locals.insert(name.clone());
      if cell_vars.contains(name) {
        Ok(AST::Let(name.clone(), Box::new(AST::Cell(Box::new(expr)))))
      } else {
        Ok(AST::Let(name.clone(), Box::new(expr)))
      }
    }
    AST::Variable(name) => {
      if must_deref(name, captures, cell_vars, locals) {
        Ok(AST::DerefCell(Box::new(AST::Variable(name.clone()))))
      } else {
        Ok(ast.clone())
      }
    }
    AST::Call(callable, args) => {
      let callable = patch_cell_access(callable, captures, cell_vars, locals)?;
      let args = patch_cell_accesses(args, captures, cell_vars, locals)?;
      Ok(AST::Call(Box::new(callable), args))
    }
    AST::CallFixed(ident, args) => {
      let args = patch_cell_accesses(args, captures, cell_vars, locals)?;
      if let crate::parser::Identifier::Bare(name) = ident {
        if must_deref(name, captures, cell_vars, locals) {
          return Ok(AST::Call(
            Box::new(AST::DerefCell(Box::new(AST::Variable(name.clone())))),
            args,
          ));
        }
      }
      Ok(AST::CallFixed(ident.clone(), args))
    }
    AST::Cell(expr) => Ok(AST::Cell(Box::new(patch_cell_access(
      expr, captures, cell_vars, locals,
    )?))),
    AST::DerefCell(expr) => Ok(AST::DerefCell(Box::new(patch_cell_access(
      expr, captures, cell_vars, locals,
    )?))),
    AST::PartialApply(callable, args) => {
      let callable = patch_cell_access(callable, captures, cell_vars, locals)?;
      Ok(AST::PartialApply(Box::new(callable), args.clone()))
    }
    AST::DefineFn(_)
    | AST::Import(_)
    | AST::DeclareFn(_)
    | AST::Int(_)
    | AST::Float(_)
    | AST::String(_)
    | AST::FunctionRef(_, _) => Ok(ast.clone()),
  }
}

fn must_deref(
  name: &str,
  captures: &[String],
  cell_vars: &HashSet<String>,
  locals: &HashSet<String>,
) -> bool {
  (captures.iter().any(|capture| capture == name) && !locals.contains(name))
    || (cell_vars.contains(name) && locals.contains(name))
}

fn closure_expr(module_name: &str, lifted_name: &str, captures: &[String]) -> AST {
  //! Generate a PartialApply of a closure with its captures
  let func_ref = AST::FunctionRef(module_name.to_string(), lifted_name.to_string());
  if captures.is_empty() {
    func_ref
  } else {
    AST::PartialApply(
      Box::new(func_ref),
      captures.iter().cloned().map(AST::Variable).collect(),
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
          Let(
            "inner".to_string(),
            Box::new(PartialApply(
              Box::new(FunctionRef(
                "main".to_string(),
                "inner:(closure)".to_string(),
              )),
              vec![AST::Variable("a".to_string())],
            )),
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
          Let(
            "inner".to_string(),
            Box::new(PartialApply(
              Box::new(FunctionRef(
                "main".to_string(),
                "inner:(closure)".to_string(),
              )),
              vec![AST::Variable("par".to_string())],
            )),
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
        code: vec![Let(
          "inner".to_string(),
          Box::new(FunctionRef(
            "main".to_string(),
            "inner:(closure)".to_string(),
          )),
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
          Let(
            "inner".to_string(),
            Box::new(FunctionRef(
              "main".to_string(),
              "inner:(closure)".to_string(),
            )),
          ),
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
          Let(
            "inner".to_string(),
            Box::new(PartialApply(
              Box::new(FunctionRef(
                "main".to_string(),
                "inner:(closure)".to_string(),
              )),
              vec![AST::Variable("a".to_string())],
            )),
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
        code: vec![AST::Let(
          "inner".to_string(),
          Box::new(AST::PartialApply(
            Box::new(AST::FunctionRef(
              "main".to_string(),
              "inner:(closure)".to_string(),
            )),
            vec![AST::Variable("a".to_string())],
          )),
        )],
      }),
      AST::DefineFn(Function {
        name: "outer".to_string(),
        params: vec![],
        code: vec![
          AST::Let("a".to_string(), Box::new(AST::Cell(Box::new(AST::Int(1))))),
          AST::Let(
            "intermediate".to_string(),
            Box::new(AST::PartialApply(
              Box::new(AST::FunctionRef(
                "main".to_string(),
                "intermediate:(closure)".to_string(),
              )),
              vec![AST::Variable("a".to_string())],
            )),
          ),
          AST::Variable("intermediate".to_string()),
        ],
      }),
    ];
    assert_eq!(new_asts, expected);
    Ok(())
  }
}
