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

use std::collections::{HashMap, HashSet};

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
  //! 3. Remove those inner functions from the outer function and emit them as (mangled) top-level functions.
  //! 4. then, transform the outer function to wrap those inner-used variables in Cell.
  // TODO: Handle parameters to the outer function which are used in inner functions.
  // Need to convert these to `fn outer(x) { let x = Cell(x); ...}
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
  let outer_func = transform_declared_vars(&outer_func, &all_closure_bindings)?;

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
      // TODO: uniquify the name!
      let new_name = format!("{}:[closure]", inner_func.name);
      let (free_vars, inner_func) = transform_free_vars(inner_func, locals)?;
      all_closure_bindings.insert(inner_func.name.clone(), free_vars.clone());
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
      Some(AST::Variable(inner_func.name.clone()))
      // Some(AST::PartialApply(Box::new(AST::Variable(new_name)), free_vars.iter().cloned().map(AST::Variable).collect()))
    }
    _ => None,
  };
  Ok(new_ast)
}

fn transform_free_vars(
  func: &Function,
  environment: &HashSet<String>,
) -> Result<(Vec<String>, Function), String> {
  //! Transform a function so that any free variables are converted to Celled parameters.
  //! Also returns the names of any free variables used in the function.
  //! * `environment` - the names that are defined in the containing function.
  let mut locals = hashset!{};
  let mut free_vars = vec![];
  let code = {
    let mut transformer = |ast: &AST| {
      match ast {
        AST::Let(name, _v) => {
          locals.insert(name.clone());
          Ok(None)
        }
        AST::Variable(ref name) => {
          if environment.contains(name) {
            if !free_vars.contains(name) {
              free_vars.push(name.clone());
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
  println!(
    "[RADIX] transform_declared_vars closure_bindings {:?}",
    closure_bindings
  );
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
          println!("[RADIX] wut dis var {:?}", name);
          if all_used_free_vars.contains(name) {
            Ok(Some(AST::DerefCell(Box::new(AST::Variable(name.clone())))))
          } else if let Some(params) = closure_bindings.get(name) {
            println!("[RADIX] omg referring to the closure itself");
            Ok(Some(AST::PartialApply(
              Box::new(AST::Variable(format!("{}:[closure]", name))),
              params.iter().cloned().map(AST::Variable).collect(),
            )))
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
        name: "inner:[closure]".to_string(),
        params: vec!["a".to_string()],
        code: vec![DerefCell(Box::new(Variable("a".to_string())))],
      }),
      AST::DefineFn(Function {
        name: "outer".to_string(),
        params: vec![],
        code: vec![
          Let("a".to_string(), Box::new(Cell(Box::new(Int(1))))),
          PartialApply(
            Box::new(Variable("inner:[closure]".to_string())),
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
        (fn inner () par))";
    let asts = read_multiple(source)?;
    let new_asts = transform_closures_in_module(&asts)?;
    use parser::AST::*;
    let expected = vec![
      AST::DefineFn(Function {
        name: "inner:[closure]".to_string(),
        params: vec!["par".to_string()],
        code: vec![DerefCell(Box::new(Variable("par".to_string())))],
      }),
      AST::DefineFn(Function {
        name: "outer".to_string(),
        params: vec!["par".to_string()],
        code: vec![
          Let("par".to_string(), Box::new(Cell(Box::new(Variable("par".to_string()))))),
          PartialApply(
            Box::new(Variable("inner:[closure]".to_string())),
            vec![AST::Variable("par".to_string())],
          ),
        ],
      }),
    ];
    assert_eq!(new_asts, expected);
    Ok(())
  }
}
