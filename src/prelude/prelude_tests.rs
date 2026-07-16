
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
