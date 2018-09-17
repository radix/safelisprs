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

use parser::{Function, AST};

pub fn closure_transform(items: &[AST]) -> Result<Vec<AST>, String> {
  //! We transform any usage of closures (i.e., nested functions that make use of
  //! variables from their lexical scope) into mostly plain functions.
  //!
  //! 1. nested functions (regardless of whether they use enclosing variables) are
  //!    lifted (I'm not sure if this is actually an important thing to happen at
  //!    the AST level, but it makes things easier).
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


fn closurize_function(func: &Function) -> Result<Vec<AST>, String> {
  //! We need to do the following things:
  //! 1. look for inner functions
  //! 2. scan for their free variables
  //! 3. scan THIS function for matching variable declarations
  //! 4. If there are matches: wrap the declarations with (cell)
  //! 5. ALSO: only allow calls to the inner function if all the outer variables
  //!    have been initialized...
  let mut top_level = vec![];
  let code = {
    let mut transformer = |ast: &AST| -> Result<Option<AST>, String> {
      match ast {
        AST::DefineFn(func) => {
          // TODO: mangle the name!
          top_level.push(AST::DefineFn(func.clone()));
          Ok(Some(AST::Variable(func.name.clone())))
        }
        _ => Ok(None)
      }
    };
    transform_multi(func.code.iter(), &mut transformer)?
  };
  top_level.push(AST::DefineFn(Function { name: func.name.clone(), params: func.params.clone(), code}));
  Ok(top_level)
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
  use super::transform;
  use parser::AST;
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
  fn test_transform_no_replacement_closure() {
    let ast = AST::Let("a".to_string(), Box::new(AST::Int(42)));
    let mut transformer = |a: &AST| {Ok(None)};
    assert_eq!(transform(&ast, &mut transformer).unwrap(), ast);
  }
}
