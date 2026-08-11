use super::*;
use crate::parser::{erase_bindings, read_multiple};

fn resolve(source: &str) -> Vec<AST> {
  erase_bindings(
    &resolve_module_names(
      "main",
      &read_multiple(source).unwrap(),
      &[("std", "+")],
      &[],
    )
    .unwrap(),
  )
}

fn resolve_with_bindings(source: &str) -> Vec<AST> {
  resolve_module_names(
    "main",
    &read_multiple(source).unwrap(),
    &[("std", "+")],
    &[],
  )
  .unwrap()
}

fn resolve_err(source: &str) -> String {
  resolve_module_names(
    "main",
    &read_multiple(source).unwrap(),
    &[("std", "+")],
    &[],
  )
  .unwrap_err()
}

#[test]
fn let_cannot_rebind_an_in_scope_name() {
  let err = resolve_err("(fn main () (let a 1) (let a 2))");
  assert!(err.contains("already in scope"), "got: {err}");
}

#[test]
fn let_cannot_shadow_a_parameter() {
  let err = resolve_err("(fn main (p:Int) (let p 1))");
  assert!(err.contains("already in scope"), "got: {err}");
}

#[test]
fn assign_rejects_an_unbound_name() {
  let err = resolve_err("(fn main () (= y 1))");
  assert!(err.contains("not bound in the local scope"), "got: {err}");
}

#[test]
fn shd_introduces_a_fresh_binding_for_an_unbound_name() {
  // `shd` is a shadowing form equivalent to `let`: it introduces a fresh
  // binding even when no existing binding is present.
  resolve_with_bindings("(fn main () (shd y 1))");
}

#[test]
fn assign_rejects_a_module_level_function() {
  let err = resolve_err("(fn x () 1) (fn main () (= x 3))");
  assert!(err.contains("not bound in the local scope"), "got: {err}");
}

#[test]
fn shd_shadows_a_module_level_function() {
  // `shd` may shadow a same-module top-level function name: it creates a
  // fresh binding rather than reusing the function binding.
  resolve_with_bindings("(fn x () 1) (fn main () (shd x 3))");
}

#[test]
fn shd_shadows_a_local_nested_function_binding() {
  // A nested `fn` is a local binding, so `shd`-ing it introduces a fresh
  // binding that shadows it (even with a different type).
  resolve_with_bindings("(fn main () (fn x () 1) (shd x 3))");
}

#[test]
fn shd_shadows_a_local_let_binding() {
  // `shd` introduces a fresh binding that shadows the existing `let`.
  resolve_with_bindings("(fn main () (let x 1) (shd x 2))");
}

#[test]
fn let_in_nested_fn_shadows_enclosing_binding() {
  // A `let` inside a nested function may shadow a binding from an enclosing
  // function: it creates a fresh binding rather than reusing the outer one, so
  // the outer variable is not captured.
  resolve_with_bindings("(fn outer () (let a 1) (fn inner () (let a 2) a))");
}

#[test]
fn assign_of_enclosing_binding_is_rejected() {
  // `=` may only assign current-function locals; assigning a binding from an
  // enclosing function would require a capture, which `=` never creates.
  let err = resolve_err("(fn outer () (let a 1) (fn inner () (= a 2)))");
  assert!(err.contains("not bound in the local scope"), "got: {err}");
}

#[test]
fn shd_shadows_an_enclosing_binding() {
  // `shd` is a shadowing form: inside a nested function it may shadow a
  // binding from an enclosing function by introducing a fresh binding rather
  // than reusing the outer one, so the outer variable is not captured.
  resolve_with_bindings("(fn outer () (let a 1) (fn inner () (shd a 2) a))");
}

#[test]
fn let_shadows_a_module_level_function() {
  // A `let` may shadow a same-module top-level function name: the function
  // binding is not a current-function local, so the `let` creates a fresh
  // binding instead of erroring.
  resolve_with_bindings(
    "(fn transform (x:Int) ->Int (std::+ x x))
     (fn main () ->Int (let transform identity) transform)",
  );
}

#[test]
fn rewrites_bare_fixed_call_to_qualified_call() {
  assert_eq!(
    resolve("(fn main () ->Int (+ 1 2))")[0],
    AST::DefineFn(Function {
      name: "main".into(),
      params: vec![],
      return_type: Some(crate::parser::TypeAst::Named("Int".into())),
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
      return_type: Some(crate::parser::TypeAst::Named("Int".into())),
      bounds: vec![],
      code: vec![AST::CallFixed(
        Identifier::Qualified("main".to_string(), "+".to_string()),
        vec![],
      )],
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
      return_type: Some(crate::parser::TypeAst::Named("Int".into())),
      bounds: vec![],
      code: vec![
        AST::synthetic(ASTKind::If(
          Box::new(AST::synthetic(ASTKind::Bool(true))),
          Box::new(AST::Let("+".to_string(), Box::new(AST::Int(1)))),
          Some(Box::new(AST::Int(0))),
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
fn let_in_both_if_branches_does_not_escape_after_if() {
  // `let` inside an `if` branch is branch-local: even when both branches bind
  // the same name, the binding does not escape the conditional. The trailing
  // `+` therefore resolves to the prelude, not to either branch binding.
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
          Box::new(AST::Let("+".to_string(), Box::new(AST::Int(1)))),
          Some(Box::new(AST::Let("+".to_string(), Box::new(AST::Int(2))))),
        )),
        AST::FunctionRef("std".to_string(), "+".to_string()),
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
  let ASTKind::Call(odd_ref, _) = &even.code[0].kind else {
    panic!("expected call to odd");
  };
  let ASTKind::Variable(odd_ref) = &odd_ref.kind else {
    panic!("expected odd variable");
  };
  let ASTKind::Call(even_ref, _) = &odd.code[0].kind else {
    panic!("expected call to even");
  };
  let ASTKind::Variable(even_ref) = &even_ref.kind else {
    panic!("expected even variable");
  };
  assert_eq!(odd_ref.binding, odd.name.binding);
  assert_eq!(even_ref.binding, even.name.binding);
}

#[test]
fn assign_in_if_branches_reuses_one_binding() {
  // `=` reuses the existing binding rather than introducing a fresh one, so an
  // `=` in each branch of an `if` shares a single binding identity with the
  // outer `let` and with the reference after the conditional.
  let asts = resolve_with_bindings("(fn main () ->Int (let x 0) (if true (= x 1) (= x 2)) x)");
  let ASTKind::DefineFn(main) = &asts[0].kind else {
    panic!("expected main");
  };
  let ASTKind::Let(outer, _, _) = &main.code[0].kind else {
    panic!("expected outer let");
  };
  let ASTKind::If(_, then_branch, else_branch) = &main.code[1].kind else {
    panic!("expected if");
  };
  let ASTKind::Let(then_name, _, _) = &then_branch.kind else {
    panic!("expected = in then branch");
  };
  let Some(else_branch) = else_branch else {
    panic!("expected else branch");
  };
  let ASTKind::Let(else_name, _, _) = &else_branch.kind else {
    panic!("expected = in else branch");
  };
  let ASTKind::Variable(use_) = &main.code[2].kind else {
    panic!("expected reference after if");
  };
  assert_eq!(outer.binding, then_name.binding);
  assert_eq!(outer.binding, else_name.binding);
  assert_eq!(outer.binding, use_.binding);
}

#[test]
fn shd_in_if_branches_introduces_fresh_bindings() {
  // `shd` introduces a fresh binding in each branch rather than reusing the
  // outer one, so each branch gets its own binding identity and the reference
  // after the conditional resolves to the outer `let`.
  let asts = resolve_with_bindings("(fn main () ->Int (let x 0) (if true (shd x 1) (shd x 2)) x)");
  let ASTKind::DefineFn(main) = &asts[0].kind else {
    panic!("expected main");
  };
  let ASTKind::Let(outer, _, _) = &main.code[0].kind else {
    panic!("expected outer let");
  };
  let ASTKind::If(_, then_branch, else_branch) = &main.code[1].kind else {
    panic!("expected if");
  };
  let ASTKind::Let(then_name, _, _) = &then_branch.kind else {
    panic!("expected shd in then branch");
  };
  let Some(else_branch) = else_branch else {
    panic!("expected else branch");
  };
  let ASTKind::Let(else_name, _, _) = &else_branch.kind else {
    panic!("expected shd in else branch");
  };
  let ASTKind::Variable(use_) = &main.code[2].kind else {
    panic!("expected reference after if");
  };
  assert_ne!(outer.binding, then_name.binding);
  assert_ne!(outer.binding, else_name.binding);
  assert_ne!(then_name.binding, else_name.binding);
  assert_eq!(outer.binding, use_.binding);
}

#[test]
fn assign_reuses_existing_binding() {
  // `=` reassigns the existing binding in place: the `let` and the `=` share
  // one binding identity, which is also what the later reference resolves to.
  let asts = resolve_with_bindings("(fn main () ->Int (let x 1) (= x 2) x)");
  let ASTKind::DefineFn(main) = &asts[0].kind else {
    panic!("expected main");
  };
  let ASTKind::Let(first, _, _) = &main.code[0].kind else {
    panic!("expected first binding");
  };
  let ASTKind::Let(second, _, _) = &main.code[1].kind else {
    panic!("expected = binding");
  };
  let ASTKind::Variable(use_) = &main.code[2].kind else {
    panic!("expected reference");
  };
  assert_eq!(first.binding, second.binding);
  assert_eq!(second.binding, use_.binding);
}

#[test]
fn shd_introduces_a_fresh_binding() {
  // `shd` introduces a fresh binding: the `let` and the `shd` have distinct
  // binding identities, and the later reference resolves to the `shd` binding
  // (the one that shadows).
  let asts = resolve_with_bindings("(fn main () ->Int (let x 1) (shd x 2) x)");
  let ASTKind::DefineFn(main) = &asts[0].kind else {
    panic!("expected main");
  };
  let ASTKind::Let(first, _, _) = &main.code[0].kind else {
    panic!("expected first binding");
  };
  let ASTKind::Let(second, _, _) = &main.code[1].kind else {
    panic!("expected shd binding");
  };
  let ASTKind::Variable(use_) = &main.code[2].kind else {
    panic!("expected reference");
  };
  assert_ne!(first.binding, second.binding);
  assert_eq!(second.binding, use_.binding);
}

#[test]
fn resolved_local_call_is_dynamic() {
  let asts = resolve_with_bindings(
    "(fn main () ->Int
         (let f std::+)
         (f 1 2))",
  );
  let ASTKind::DefineFn(main) = &asts[0].kind else {
    panic!("expected main");
  };
  let ASTKind::Let(f, _, _) = &main.code[0].kind else {
    panic!("expected local function binding");
  };
  let ASTKind::Call(callable, _) = &main.code[1].kind else {
    panic!("expected dynamic local call");
  };
  let ASTKind::Variable(f_ref) = &callable.kind else {
    panic!("expected local variable as callable");
  };
  assert_eq!(f_ref.binding, f.binding);
}

#[test]
fn resolved_top_level_call_is_qualified() {
  let asts = resolve_with_bindings(
    "(fn helper () ->Int 1)
       (fn main () ->Int (helper))",
  );
  let ASTKind::DefineFn(main) = &asts[1].kind else {
    panic!("expected main");
  };
  assert!(matches!(
    &main.code[0].kind,
    ASTKind::CallFixed(Identifier::Qualified(module, name), _)
      if module == "main" && name == "helper"
  ));
}

#[test]
fn resolved_top_level_value_is_a_function_ref() {
  let asts = resolve_with_bindings(
    "(fn helper () ->Int 1)
       (fn main () ->(Fn () -> Int) helper)",
  );
  let ASTKind::DefineFn(main) = &asts[1].kind else {
    panic!("expected main");
  };
  assert!(matches!(
    &main.code[0].kind,
    ASTKind::FunctionRef(module, name) if module == "main" && name == "helper"
  ));
}
