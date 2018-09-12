


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



use parser::{AST, Function, Identifier};


/// We transform any usage of closures (i.e., nested functions that make use of
/// variables from their lexical scope) into mostly plain functions.
///
/// 1. nested functions (regardless of whether they use enclosing variables) are
///    lifted (I'm not sure if this is actually an important thing to happen at
///    the AST level...).
/// 2. variables used in closures have their values wrapped in Cell, and their
///    usage wrapped in DerefCell.
/// 3. Free variables in closure-functions get converted to parameters, which
///    are expected to be passed as Cells.
/// 4. We set a local variable with the result of a MakeClosure where the inner
///    function was defined.
///
/// TODO: Disallow any invocation or returning of a closure before all of its
/// cells have been initialized.
/// TODO: "drop" the (let cname (make-closure ...)) to the lowest point possible
pub fn closure_transform(items: &[AST]) -> Result<Vec<AST>, String> {
  let mut result = vec![];
  for item in items {
    let out = match item {
      AST::DefineFn(func) => {
        closurize_function(func)?
      }
      x => vec![x.clone()],
    };
    result.extend(out);
  }
  Ok(result)
}


fn closurize_function(_func: &Function) -> Result<Vec<AST>, String> {
  // We need to do the following things:
  // 1. look for inner functions
  // 2. scan for their free variables
  // 3. scan THIS function for matching variable declarations
  // 4. If there are matches: wrap the declarations with (cell)
  // 5. ALSO: only allow calls to the inner function if all the outer variables
  //    have been initialized...
  Ok(vec![])
}
