use super::*;
use crate::builtins::default_builtins;
use crate::parser::read_multiple;
use crate::prelude::resolve_module_names;

fn check(source: &str) -> Result<(), TypeError> {
  let asts = read_multiple(source).unwrap();
  let asts = resolve_module_names("main", &asts, &[], &[]).unwrap();
  typecheck(asts, &default_builtins().specs()).map(|_| ())
}

#[test]
fn polymorphic_identity_can_be_used_at_two_types() {
  check(
    "(fn id (a:A) ->A a)
       (fn main () ->Bool (block (id 1) (std::== (id \"x\") \"x\")))",
  )
  .unwrap();
}

#[test]
fn structs_typecheck_construction_and_field_access() {
  check(
    "(struct Foo x:Int y:(Cell Int))
       (fn main () ->Int
         (let foo (new Foo y:(std::cell 2) x:3))
         foo.x)",
  )
  .unwrap();
}

#[test]
fn chained_struct_field_access_typechecks() {
  check(
    "(struct Point x:Int y:Int)
       (struct Box origin:Point size:Int)
       (fn main () ->Int
         (let b (new Box size:10 origin:(new Point x:4 y:5)))
         (std::+ b.origin.x b.origin.y))",
  )
  .unwrap();
}

#[test]
fn records_receiver_types_for_field_accesses() {
  let source = "
    (struct Point y:Int x:Int)
    (struct Box origin:Point size:Int)
    (fn main () ->Int
      (let b (new Box origin:(new Point y:3 x:4) size:5))
      b.origin.x)";
  let asts = read_multiple(source).unwrap();
  let asts = resolve_module_names("main", &asts, &[], &[]).unwrap();
  let checked = typecheck(asts, &default_builtins().specs()).unwrap();
  let asts = checked.asts();
  let info = checked.type_info();

  let ASTKind::DefineFn(main) = &asts[2].kind else {
    panic!("expected main function");
  };
  let ASTKind::FieldAccess(origin, _) = &main.code[1].kind else {
    panic!("expected outer field access");
  };
  let ASTKind::FieldAccess(_, _) = &origin.kind else {
    panic!("expected inner field access");
  };

  let origin = info.field_access(origin.id()).unwrap();
  assert_eq!(origin.receiver_type(), "Box");
  assert_eq!(origin.field_index(), 0);

  let x = info.field_access(main.code[1].id()).unwrap();
  assert_eq!(x.receiver_type(), "Point");
  assert_eq!(x.field_index(), 1);
}

#[test]
fn struct_construction_requires_known_fields() {
  let error = check(
    "(struct Foo x:Int)
       (fn main () ->Foo
         (new Foo x:1 y:2))",
  )
  .unwrap_err();
  assert!(error.message.contains("unknown field `y`"), "{error}");
}

#[test]
fn struct_construction_requires_all_fields() {
  let error = check(
    "(struct Foo x:Int y:Int)
       (fn main () ->Foo
         (new Foo x:1))",
  )
  .unwrap_err();
  assert!(
    error.message.contains("missing initializer for field `y`"),
    "{error}"
  );
}

#[test]
fn enums_typecheck_variant_construction() {
  check(
    "(enum Foo
       (Var1)
       (Var2 x:Int)
       (Var3 y:String z:(Cell Int)))
     (fn main () ->Foo
       (new Foo::Var3 y:\"hi\" z:(std::cell 2)))",
  )
  .unwrap();
}

#[test]
fn enum_construction_requires_known_fields() {
  let error = check(
    "(enum Foo (Var x:Int))
     (fn main () ->Foo
       (new Foo::Var x:1 y:2))",
  )
  .unwrap_err();
  assert!(error.message.contains("unknown field `y`"), "{error}");
}

#[test]
fn enum_construction_requires_all_variant_fields() {
  let error = check(
    "(enum Foo (Var x:Int y:Int))
     (fn main () ->Foo
       (new Foo::Var x:1))",
  )
  .unwrap_err();
  assert!(
    error.message.contains("missing initializer for field `y`"),
    "{error}"
  );
}

#[test]
fn match_typechecks_exhaustive_enum_arms() {
  check(
    "(enum Foo
       (Var1)
       (Var2 x:Int)
       (Var3 y:String z:(Cell Int)))
     (fn main (foo:Foo) ->Int
       (match foo
         (Var1) => 1
         (Var2 x) => x
         (Var3 y z) => 2))",
  )
  .unwrap();
}

#[test]
fn match_default_arm_covers_remaining_variants() {
  check(
    "(enum Foo
       (Var1)
       (Var2 x:Int))
     (fn main (foo:Foo) ->Int
       (match foo
         (Var2 x) => x
         _ => 5))",
  )
  .unwrap();
}

#[test]
fn match_requires_exhaustive_arms_without_default() {
  let error = check(
    "(enum Foo
       (Var1)
       (Var2 x:Int))
     (fn main (foo:Foo) ->Int
       (match foo
         (Var1) => 1))",
  )
  .unwrap_err();
  assert!(error.message.contains("non-exhaustive match"), "{error}");
}

#[test]
fn match_pattern_names_must_be_variant_fields() {
  let error = check(
    "(enum Foo (Var x:Int y:Int))
     (fn main (foo:Foo) ->Int
       (match foo
         (Var z) => z))",
  )
  .unwrap_err();
  assert!(error.message.contains("has no field `z`"), "{error}");
}

#[test]
fn missing_bound_is_rejected() {
  let error = check("(fn double (a:A) ->A (std::+ a a))").unwrap_err();
  assert!(error.message.contains("requires trait `Add`"), "{error}");
}

#[test]
fn occurs_check_rejects_infinite_type() {
  let mut checker = Checker::new(std::iter::empty());
  let variable = checker.fresh(Some("test".to_string()), Vec::new());
  let error = checker
    .unify(variable.clone(), Type::List(Box::new(variable)))
    .unwrap_err();
  assert!(error.message.contains("infinite type"), "{error}");
}

#[test]
fn unresolved_empty_list_is_rejected() {
  let error = check("(fn main () ->Int (let xs (std::list)) (std::len xs))").unwrap_err();
  assert!(error.message.contains("type annotation needed"), "{error}");
}

#[test]
fn variadic_builtin_can_be_called_through_local_binding() {
  check(
    "(fn main () ->(List Int)
         (let make std::list)
         (make 1 2 3))",
  )
  .unwrap();
}

#[test]
fn variadic_builtin_can_be_passed_to_annotated_parameter() {
  check(
    "(fn use-list (make:(Fn (...Int) -> (List Int))) ->(List Int)
         (make 1 2 3))
       (fn main () ->(List Int)
         (use-list std::list))",
  )
  .unwrap();
}

#[test]
fn map_accepts_top_level_function() {
  check(
    "(fn sq (x:Int) ->Int (std::+ x x))
       (fn main () ->(List Int) (std::map (std::range 0 5) sq))",
  )
  .unwrap();
}

#[test]
fn rigid_variable_cannot_be_replaced_by_a_concrete_type() {
  let error = check("(fn bad (a:A) ->A 5)").unwrap_err();
  assert!(error.message.contains("rigid type variable `A`"), "{error}");
}

#[test]
fn declared_trait_bound_allows_polymorphic_builtin_use() {
  check("(fn double (a:A) ->A where ((A Add)) (std::+ a a))").unwrap();
}

#[test]
fn unbound_variable_bounds_are_merged() {
  let mut checker = Checker::new(std::iter::empty());
  let add = checker.fresh(Some("add".to_string()), vec![Trait::Add]);
  let sub = checker.fresh(Some("sub".to_string()), vec![Trait::Sub]);
  checker.unify(add.clone(), sub).unwrap();
  let error = checker.unify(add, Type::String).unwrap_err();
  assert!(error.message.contains("does not satisfy trait"), "{error}");
}

#[test]
fn nested_function_can_capture_enclosing_type_variable() {
  check(
    "(fn outer (a:A) ->(Fn (A) -> A)
         (fn inner (ignored:A) ->A a))",
  )
  .unwrap();
}

#[test]
fn nested_declared_function_is_generalized() {
  check(
    "(fn main () ->Bool
         (let id (fn id (a:A) ->A a))
         (id 1)
         (std::== (id \"x\") \"x\"))",
  )
  .unwrap();
}

#[test]
fn void_return_discards_the_final_expression_type() {
  check("(fn main () (std::+ 1 2))").unwrap();
}

#[test]
fn if_condition_must_be_bool() {
  let error = check("(fn main () ->Int (if 1 2 3))").unwrap_err();
  assert!(
    error.message.contains("expected `Bool`, got `Int`"),
    "{error}"
  );
}

#[test]
fn unknown_bare_value_name_is_reported_as_a_name_error() {
  let error = check("(fn main () y)").unwrap_err();
  assert_eq!(error.message, "Unknown name `y`");
}

#[test]
fn bare_builtin_call_requires_prelude_resolution_before_typecheck() {
  let error = check("(fn main () ->Int (+ 1 2))").unwrap_err();
  assert_eq!(error.message, "unknown function `+`");
}

#[test]
fn binding_created_in_only_one_if_branch_is_not_available_afterward() {
  let error = check(
    "(fn main () ->Int
         (if true (let x 1) (let y 2))
         y)",
  )
  .unwrap_err();
  assert_eq!(error.message, "Unknown name `y`");
}

#[test]
fn nested_self_recursion_is_accepted() {
  check(
    "(fn main () ->Int
         (fn recurse (n:Int) ->Int
           (if (std::== n 0) 0 (recurse (std::- n 1))))
         (recurse 3))",
  )
  .unwrap();
}

#[test]
fn nested_mutual_recursion_is_accepted() {
  check(
    "(fn main () ->Bool
         (fn even (n:Int) ->Bool
           (if (std::== n 0) true (odd (std::- n 1))))
         (fn odd (n:Int) ->Bool
           (if (std::== n 0) false (even (std::- n 1))))
         (even 4))",
  )
  .unwrap();
}

#[test]
fn nested_function_is_not_visible_before_its_recursive_group() {
  let error = check(
    "(fn main () ->Int
         (later)
         (fn later () ->Int 1))",
  )
  .unwrap_err();
  assert_eq!(error.message, "unknown function `later`");
}

#[test]
fn type_errors_point_to_the_offending_argument() {
  let source = "(fn main () ->Int\n  (std::+ 1\n    \"not-an-int\"))";
  let error = check(source).unwrap_err();
  let start = source.find("\"not-an-int\"").unwrap();
  assert_eq!(error.span.as_ref().map(|span| span.start), Some(start));
  assert_eq!(
    error.render(source).lines().next(),
    Some("line 3, column 5: TypeError: expected `Int`, got `String`")
  );
}

#[test]
fn outer_context_does_not_replace_an_inner_error_span() {
  let source = "(fn main () ->Int\n  (std::+ 1\n    missing))";
  let error = check(source).unwrap_err();
  let start = source.find("missing").unwrap();
  assert_eq!(error.span.as_ref().map(|span| span.start), Some(start));
  assert!(
    error.render(source).starts_with("line 3, column 5:"),
    "{error}"
  );
}
