/*

This would convert an AST like:

(fn foo ()
  (let a 1)
  (fn inner () a)
  (inner)
)

to...

(fn foo/inner (a)
  (deref-cell a))

(fn foo ()
  (let a (cell 1))
  (let foo/inner (make-closure "foo/inner" a))
  (foo/inner))

This is then more easily compiled to:

foo/inner:
  LoadLocal 0
  DerefCell
  Return

foo:
  PushInt 1
  MakeCell
  SetLocal 0

  LoadLocal 0
  MakeFunctionRef "foo/inner"
  MakeClosure
  SetLocal 1

  LoadLocal 1
  CallDynamic
*/

use std::collections::HashSet;

use parser::{Function, AST};

pub fn transform_closures_in_module(items: &[AST]) -> Result<Vec<AST>, String> {
  //! We transform any usage of closures (i.e., nested functions that make use of
  //! variables from their lexical scope) into mostly plain functions.
  //!
  //! 1. All nested functions are lifted to the top level and have their names mangled,
  //!    regardless of whether they need closures.
  //! 2. variables used in closures have their values wrapped in Cell, and their
  //!    usage wrapped in DerefCell.
  //! 3. Free variables in closure-functions get converted to parameters, which
  //!    are expected to be passed as Cells.
  //! 4. We set a local variable with the result of a MakeClosure where the inner
  //!    function was defined.
  //!
  // TODO: Disallow any invocation or returning of a closure before all of its
  // cells have been initialized.
  // TODO: "drop" the (let cname (make-closure ...)) to the lowest point possible

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
  //! We need to do the following things:
  //! 1. look for inner functions
  //! 2. scan for their free variables
  //! 3. scan THIS function for matching variable declarations
  //! 4. If there are matches: wrap the declarations with (cell)
  //! 5. ALSO: only allow calls to the inner function if all the outer variables
  //!    have been initialized...
  let mut top_level = vec![];
  let mut locals = hashset!{};
  locals.extend(outer_func.params.iter().cloned());
  let mut all_used_free_vars = hashset!{};

  let code = transform_multi(outer_func.code.iter(), &mut |ast: &AST| {
    _closure_code_transform(ast, &mut locals, &mut all_used_free_vars, &mut top_level)
  })?;
  // TODO: transform the *outer* function to use `PartialApply` with `Cell`ed up values
  let outer_func = transform_declared_vars(outer_func, &all_used_free_vars)?;

  top_level.push(AST::DefineFn(Function {
    name: outer_func.name.clone(),
    params: outer_func.params.clone(),
    code,
  }));
  Ok(top_level)
}

fn _closure_code_transform(
  ast: &AST,
  locals: &mut HashSet<String>,
  all_used_free_vars: &mut HashSet<String>,
  top_level: &mut Vec<AST>,
) -> Result<Option<AST>, String> {
  //! This is a transformer suitable for passing to `transform`.
  match ast {
    AST::Let(name, _v) => {
      locals.insert(name.clone());
      Ok(None)
    }
    AST::DefineFn(inner_func) => {
      // TODO: uniquify the name!
      let new_name = format!("{}_closure", inner_func.name);
      let (free_vars, inner_func) = transform_free_vars(inner_func, locals)?;
      all_used_free_vars.extend(free_vars);
      // TODO: search func.params and func.code for definitions with the same names
      // TODO: If there are any matches, add them as prefixes to the parameter list
      //       of the transformed inner_func
      // TODO: transform inner_func accesses to those variables to use DerefCell
      // TODO: figure out how the hell to do this while allowing referring to the function
      //       before defining the variable...?
      //         - How the heck? We want to make sure the variables a closure uses are defined
      //           by the time the closure is *called*, or, as a compromise, by the time the
      //           closure *escapes* the defining function. that means: when it is passed to
      //           another function, or when it is returned from this function.
      //         - In the meantime, we can just require that variables are initialized before
      //           the closure is used.
      top_level.push(AST::DefineFn(Function {
        name: new_name.clone(),
        ..inner_func
      }));
      Ok(Some(AST::Variable(new_name)))
    }
    _ => Ok(None),
  }
}

fn transform_free_vars(
  func: &Function,
  environment: &HashSet<String>,
) -> Result<(HashSet<String>, Function), String> {
  //! Transform a function so that any free variables are converted to Celled parameters.
  //! Also returns the names of any free variables used in the function.
  //! * `environment` - the names that are defined in the containing function.
  let mut locals = hashset!{};
  let mut free_vars = hashset!{};
  let code = {
    let mut transformer = |ast: &AST| {
      match ast {
        AST::Let(name, _v) => {
          locals.insert(name.clone());
          Ok(None)
        }
        AST::Variable(ref name) => {
          if environment.contains(name) {
            free_vars.insert(name.clone());
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
  params.extend(free_vars.iter().cloned());
  params.extend(func.params.clone());
  Ok((
    free_vars,
    Function {
      name: func.name.clone(),
      params,
      code,
    },
  ))
}

fn transform_declared_vars(
  func: &Function,
  all_used_free_vars: &HashSet<String>,
) -> Result<Function, String> {
  Ok(func.clone())
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
        name: "inner_closure".to_string(),
        params: vec!["a".to_string()],
        code: vec![DerefCell(Box::new(Variable("a".to_string())))],
      }),
      AST::DefineFn(Function {
        name: "outer".to_string(),
        params: vec![],
        code: vec![
          Let("a".to_string(), Box::new(Cell(Box::new(Int(1))))),
          PartialApply(Box::new(Variable("inner_closure".to_string())), vec![]),
        ],
      }),
    ];

    // let expected_source = "
    // (fn inner_closure (a)
    //   (deref-cell a))
    // (fn outer ()
    //   (let a (cell 1))
    //   (partial-apply inner_closure a))";
    assert_eq!(new_asts, expected);
    Ok(())
  }
}
