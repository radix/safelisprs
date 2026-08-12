use super::*;
use crate::builtins::{CustomTypeSpec, Library};
use crate::parser::read_multiple;
use crate::resolver::resolve_module_names;
use crate::types::Signature;

fn check(source: &str) -> Result<(), TypeError> {
  let asts = read_multiple(source).unwrap();
  let asts = resolve_module_names("main", &asts, &[], &[]).unwrap();
  typecheck(asts, &Library::default()).map(|_| ())
}

fn check_with(source: &str, library: &Library) -> Result<(), TypeError> {
  let asts = read_multiple(source).unwrap();
  let asts = resolve_module_names("main", &asts, &[], &[]).unwrap();
  typecheck(asts, library).map(|_| ())
}

fn host_enum_library() -> Library {
  Library::new().with_type(CustomTypeSpec::enum_(
    "host",
    "MaybeInt",
    vec![("Some", vec![("value", Signature::Int)]), ("None", vec![])],
  ))
}

fn host_struct_library() -> Library {
  Library::new().with_type(CustomTypeSpec::struct_(
    "host",
    "Box",
    vec![("value", Signature::Int)],
  ))
}

/// A library that declares a host struct `Shape::Circle` (module `Shape`,
/// name `Circle`) with a field `area`, used alongside a source enum `Shape`
/// with a `Circle` variant to exercise 2-segment disambiguation.
fn shape_library() -> Library {
  Library::new().with_type(CustomTypeSpec::struct_(
    "Shape",
    "Circle",
    vec![("area", Signature::Int)],
  ))
}

#[test]
fn polymorphic_identity_can_be_used_at_two_types() {
  check(
    "[fn id [a:A] ->A a]
       [fn main [] ->Bool [block [id 1] [std::== [id \"x\"] \"x\"]]]",
  )
  .unwrap();
}

#[test]
fn return_value_must_match_function_return_type() {
  check("[fn main [] ->Int [return 3]]").unwrap();
  check("[fn main [] [return]]").unwrap();
  check("[fn main [] ->Int [if true [return 1] \"ignored\"] 2]").unwrap();

  let error = check("[fn main [] ->Int [return false]]").unwrap_err();
  assert!(
    error.message.contains("Int") && error.message.contains("Bool"),
    "{error}"
  );

  let error = check("[fn main [] ->Int [return]]").unwrap_err();
  assert!(
    error.message.contains("Int") && error.message.contains("Void"),
    "{error}"
  );

  let error = check("[fn main [] [return 3]]").unwrap_err();
  assert!(
    error.message.contains("Void") && error.message.contains("Int"),
    "{error}"
  );
}

#[test]
fn and_and_or_require_boolean_operands() {
  check("[fn main [] ->Bool [and true false true]]").unwrap();
  check("[fn main [] ->Bool [or false true false]]").unwrap();

  let error = check("[fn main [] ->Bool [and true 1]]").unwrap_err();
  assert!(
    error.message.contains("Bool") && error.message.contains("Int"),
    "{error}"
  );
  let error = check("[fn main [] ->Bool [or false \"no\"]]").unwrap_err();
  assert!(
    error.message.contains("Bool") && error.message.contains("String"),
    "{error}"
  );
}

#[test]
fn for_requires_a_list_and_types_its_binding() {
  check("[fn main [] [for x in [std::list 1 2 3] [std::+ x 1]]]").unwrap();

  let error = check("[fn main [] [for x in 3 x]]").unwrap_err();
  assert!(
    error.message.contains("List") && error.message.contains("Int"),
    "{error}"
  );

  let error = check("[fn main [] [for x in [std::list 1] [std::concat x \"x\"]]]").unwrap_err();
  assert!(error.message.contains("Concat"), "{error}");
}

#[test]
fn for_binding_is_scoped_to_the_loop_body() {
  let error = check("[fn main [] ->Int [for x in [std::list 1] x] x]").unwrap_err();
  assert!(error.message.contains("Unknown name `x`"), "{error}");
}

#[test]
fn structs_typecheck_construction_and_field_access() {
  check(
    "[struct Foo x:Int y:[Cell Int]]
       [fn main [] ->Int
         [let foo [new Foo y:[std::cell 2] x:3]]
         foo.x]",
  )
  .unwrap();
}

#[test]
fn chained_struct_field_access_typechecks() {
  check(
    "[struct Point x:Int y:Int]
       [struct Box origin:Point size:Int]
       [fn main [] ->Int
         [let b [new Box size:10 origin:[new Point x:4 y:5]]]
         [std::+ b.origin.x b.origin.y]]",
  )
  .unwrap();
}

#[test]
fn records_receiver_types_for_field_accesses() {
  let source = "
    [struct Point y:Int x:Int]
    [struct Box origin:Point size:Int]
    [fn main [] ->Int
      [let b [new Box origin:[new Point y:3 x:4] size:5]]
      b.origin.x]";
  let asts = read_multiple(source).unwrap();
  let asts = resolve_module_names("main", &asts, &[], &[]).unwrap();
  let checked = typecheck(asts, &Library::default()).unwrap();
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
    "[struct Foo x:Int]
       [fn main [] ->Foo
         [new Foo x:1 y:2]]",
  )
  .unwrap_err();
  assert!(error.message.contains("unknown field `y`"), "{error}");
}

#[test]
fn struct_construction_requires_all_fields() {
  let error = check(
    "[struct Foo x:Int y:Int]
       [fn main [] ->Foo
         [new Foo x:1]]",
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
    "[enum Foo
       [Var1]
       [Var2 x:Int]
       [Var3 y:String z:[Cell Int]]]
     [fn main [] ->Foo
       [new Foo::Var3 y:\"hi\" z:[std::cell 2]]]",
  )
  .unwrap();
}

#[test]
fn host_defined_enum_can_be_constructed_from_source() {
  let library = host_enum_library();
  check_with(
    "[fn main [] ->host::MaybeInt
       [new host::MaybeInt::Some value:42]]",
    &library,
  )
  .unwrap();
}

#[test]
fn host_defined_enum_construction_can_match() {
  let library = host_enum_library();
  check_with(
    "[fn main [m:host::MaybeInt] ->Int
       [match m
         [Some value] => value
         [None] => 0]]",
    &library,
  )
  .unwrap();
}

#[test]
fn host_defined_enum_construction_rejects_unknown_variant() {
  let library = host_enum_library();
  let error = check_with(
    "[fn main [] ->host::MaybeInt
       [new host::MaybeInt::Nope value:1]]",
    &library,
  )
  .unwrap_err();
  assert!(error.message.contains("unknown variant `Nope`"), "{error}");
}

#[test]
fn host_defined_enum_construction_rejects_wrong_field_type() {
  let library = host_enum_library();
  let error = check_with(
    "[fn main [] ->host::MaybeInt
       [new host::MaybeInt::Some value:\"nope\"]]",
    &library,
  )
  .unwrap_err();
  assert!(error.message.contains("Int"), "{error}");
  assert!(error.message.contains("String"), "{error}");
}

#[test]
fn host_defined_enum_construction_rejects_unknown_type() {
  let library = host_enum_library();
  let error = check_with(
    "[fn main [] ->host::Nope
       [new host::Nope::Variant value:1]]",
    &library,
  )
  .unwrap_err();
  assert!(error.message.contains("unknown type"), "{error}");
}

#[test]
fn host_defined_struct_can_be_constructed_from_source() {
  let library = host_struct_library();
  check_with(
    "[fn main [] ->host::Box
       [new host::Box value:42]]",
    &library,
  )
  .unwrap();
}

#[test]
fn host_defined_struct_construction_supports_field_access() {
  let library = host_struct_library();
  check_with(
    "[fn main [] ->Int
       [let b [new host::Box value:42]]
       b.value]",
    &library,
  )
  .unwrap();
}

#[test]
fn host_defined_struct_construction_rejects_wrong_field_type() {
  let library = host_struct_library();
  let error = check_with(
    "[fn main [] ->host::Box
       [new host::Box value:false]]",
    &library,
  )
  .unwrap_err();
  assert!(error.message.contains("Int"), "{error}");
  assert!(error.message.contains("Bool"), "{error}");
}

/// A 2-segment `new Shape::Circle` resolves to the source enum variant when one
/// exists, even if a host struct with the same `module::name` is also declared.
#[test]
fn source_enum_variant_takes_precedence_over_host_struct() {
  let library = shape_library();
  // The source enum `Shape` variant `Circle` has field `r`; the host struct
  // `Shape::Circle` has field `area`. Using the enum's field succeeds.
  check_with(
    "[enum Shape [Circle r:Int]]
     [fn main [] ->Shape [new Shape::Circle r:3]]",
    &library,
  )
  .unwrap();
  // Using the host struct's field fails: the source enum takes precedence, so
  // `area` is an unknown field for the enum variant.
  let error = check_with(
    "[enum Shape [Circle r:Int]]
     [fn main [] ->Shape [new Shape::Circle area:3]]",
    &library,
  )
  .unwrap_err();
  assert!(error.message.contains("unknown field `area`"), "{error}");
}

/// A 2-segment `new` with no matching source enum falls back to a host struct.
#[test]
fn new_falls_back_to_host_struct_without_source_enum() {
  let library = shape_library();
  check_with(
    "[fn main [] ->Shape::Circle
       [new Shape::Circle area:3]]",
    &library,
  )
  .unwrap();
}

/// When a source enum exists but the named variant does not, and no library
/// struct matches, the error names the unknown variant (not an unknown
/// struct), preserving the long-standing behavior for `new Enum::Variant`.
#[test]
fn unknown_variant_of_source_enum_reports_variant_error() {
  let error = check(
    "[enum Foo [A]]
     [fn main [] ->Foo [new Foo::B x:1]]",
  )
  .unwrap_err();
  assert!(error.message.contains("unknown variant `B`"), "{error}");
}

#[test]
fn enum_construction_requires_known_fields() {
  let error = check(
    "[enum Foo [Var x:Int]]
     [fn main [] ->Foo
       [new Foo::Var x:1 y:2]]",
  )
  .unwrap_err();
  assert!(error.message.contains("unknown field `y`"), "{error}");
}

#[test]
fn enum_construction_requires_all_variant_fields() {
  let error = check(
    "[enum Foo [Var x:Int y:Int]]
     [fn main [] ->Foo
       [new Foo::Var x:1]]",
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
    "[enum Foo
       [Var1]
       [Var2 x:Int]
       [Var3 y:String z:[Cell Int]]]
     [fn main [foo:Foo] ->Int
       [match foo
         [Var1] => 1
         [Var2 x] => x
         [Var3 y z] => 2]]",
  )
  .unwrap();
}

#[test]
fn match_default_arm_covers_remaining_variants() {
  check(
    "[enum Foo
       [Var1]
       [Var2 x:Int]]
     [fn main [foo:Foo] ->Int
       [match foo
         [Var2 x] => x
         _ => 5]]",
  )
  .unwrap();
}

#[test]
fn match_requires_exhaustive_arms_without_default() {
  let error = check(
    "[enum Foo
       [Var1]
       [Var2 x:Int]]
     [fn main [foo:Foo] ->Int
       [match foo
         [Var1] => 1]]",
  )
  .unwrap_err();
  assert!(error.message.contains("non-exhaustive match"), "{error}");
}

#[test]
fn match_pattern_names_must_be_variant_fields() {
  let error = check(
    "[enum Foo [Var x:Int y:Int]]
     [fn main [foo:Foo] ->Int
       [match foo
         [Var z] => z]]",
  )
  .unwrap_err();
  assert!(error.message.contains("has no field `z`"), "{error}");
}

#[test]
fn shd_shadows_with_a_new_type_in_a_sequence() {
  // `shd` introduces a fresh binding that may have a different type from the
  // one it shadows; the new type is visible from that point onward.
  check("[fn main [] ->Int [let x 1] [shd x true] [if x 1 0]]").unwrap();
}

#[test]
fn assign_requires_the_same_type() {
  // `=` reuses the existing binding, so the new value must match the old type.
  let error = check("[fn main [] ->Int [let x 1] [= x true] x]").unwrap_err();
  assert!(
    error.message.contains("`=` cannot change the type"),
    "{error}"
  );
}

#[test]
fn assign_same_type_succeeds() {
  check("[fn main [] ->Int [let x 1] [= x 2] x]").unwrap();
}

#[test]
fn assign_in_if_both_branches_same_type_is_usable_after() {
  // `=` in both branches reassigns the outer binding with the same type, so
  // the variable remains usable after the conditional.
  check(
    "[fn main [] ->Int
       [let x 1]
       [if true [= x 5] [= x 6]]
       x]",
  )
  .unwrap();
}

#[test]
fn shd_in_if_branches_is_branch_local() {
  // `shd` introduces a fresh branch-local binding, so the outer `x` is
  // unchanged after the conditional and keeps its original type.
  check(
    "[fn main [] ->Int
       [let x 1]
       [if true [shd x true] [shd x false]]
       x]",
  )
  .unwrap();
}

#[test]
fn shd_in_if_one_branch_keeps_outer_binding_usable() {
  // `shd` in one branch is branch-local, so the outer `x` keeps its original
  // type and remains usable after the conditional.
  check(
    "[fn main [] ->Int
       [let x 1]
       [if true [shd x 5] 0]
       x]",
  )
  .unwrap();
}

#[test]
fn assign_in_if_one_branch_type_change_is_rejected() {
  // `=` requires the same type, so reassigning with a different type in one
  // branch is a type error.
  let error = check(
    "[fn main [] ->Int
       [let x 1]
       [if true [block [= x true] 0] 0]
       x]",
  )
  .unwrap_err();
  assert!(
    error.message.contains("`=` cannot change the type"),
    "{error}"
  );
}

#[test]
fn if_without_else_is_void() {
  // An `if` with no `else` branch produces no value and is typed as `Void`.
  check("[fn main [] [if true 1]]").unwrap();
  check("[fn main [] ->Void [if true 1]]").unwrap();

  // Using a no-`else` `if` as a value is a type error.
  let error = check("[fn main [] ->Int [if true 1]]").unwrap_err();
  assert!(
    error.message.contains("Int") && error.message.contains("Void"),
    "{error}"
  );
}

#[test]
fn if_without_else_keeps_outer_binding_compatible() {
  // An `=` in the then branch of a no-`else` `if` keeps a type compatible
  // with the pre-`if` binding so the variable remains usable afterward.
  check(
    "[fn main [] ->Int
       [let x 1]
       [if true [= x 5]]
       x]",
  )
  .unwrap();
}

#[test]
fn if_without_else_drops_branch_local_let() {
  // A `let` inside a no-`else` `if` branch is branch-local and does not
  // escape, so the trailing reference is an unknown name.
  let error = check(
    "[fn main [] ->Int
       [if true [let y 1]]
       y]",
  )
  .unwrap_err();
  assert!(error.message.contains("Unknown name"), "{error}");
}

#[test]
fn assign_in_for_same_type_is_usable_after() {
  // `=` inside a `for` body reassigns the outer binding with the same type,
  // so the variable remains usable after the loop.
  check(
    "[fn main [] ->Int
       [let x 0]
       [for n in [std::list 1 2 3] [= x n]]
       x]",
  )
  .unwrap();
}

#[test]
fn assign_in_for_type_change_is_rejected() {
  // `=` inside a `for` body requires the same type as the pre-loop binding.
  let error = check(
    "[fn main [] ->Int
       [let x 1]
       [for n in [std::list 1] [= x true]]
       x]",
  )
  .unwrap_err();
  assert!(
    error.message.contains("`=` cannot change the type"),
    "{error}"
  );
}

#[test]
fn shd_in_for_introduces_loop_local_binding() {
  // `shd` inside a `for` body introduces a fresh loop-local binding, so the
  // outer `x` is unchanged after the loop and keeps its original type.
  check(
    "[fn main [] ->Int
       [let x 1]
       [for n in [std::list 1] [shd x true]]
       x]",
  )
  .unwrap();
}

#[test]
fn assign_in_match_all_arms_same_type_is_usable_after() {
  // `=` in every arm reassigns the outer binding with the same type, so the
  // variable remains usable after the match.
  check(
    "[enum E
       [A]
       [B]]
     [fn main [e:E] ->Int
       [let x 1]
       [match e
         [A] => [= x 10]
         [B] => [= x 20]]
       x]",
  )
  .unwrap();
}

#[test]
fn shd_in_match_arms_is_arm_local() {
  // `shd` introduces a fresh arm-local binding, so the outer `x` is unchanged
  // after the match and keeps its original type.
  check(
    "[enum E
       [A]
       [B]]
     [fn main [e:E] ->Int
       [let x 1]
       [match e
         [A] => [shd x true]
         [B] => [shd x false]]
       x]",
  )
  .unwrap();
}

#[test]
fn assign_in_match_with_default_arm_type_change_is_rejected() {
  // `=` requires the same type; the variant arm attempts a type change, which
  // is rejected even though the default arm does not touch `x`.
  let error = check(
    "[enum E
       [A]
       [B]]
     [fn main [e:E] ->Int
       [let x 1]
       [match e
         [A] => [block [= x true] 0]
         _ => 0]
       x]",
  )
  .unwrap_err();
  assert!(
    error.message.contains("`=` cannot change the type"),
    "{error}"
  );
}

#[test]
fn missing_bound_is_rejected() {
  let error = check("[fn double [a:A] ->A [std::+ a a]]").unwrap_err();
  assert!(error.message.contains("requires trait `Add`"), "{error}");
}

#[test]
fn occurs_check_rejects_infinite_type() {
  let mut checker = Checker::empty();
  let variable = checker.fresh(Some("test".to_string()), Vec::new());
  let error = checker
    .unify(variable.clone(), Type::List(Box::new(variable)))
    .unwrap_err();
  assert!(error.message.contains("infinite type"), "{error}");
}

#[test]
fn unresolved_empty_list_is_rejected() {
  let error = check("[fn main [] ->Int [let xs [std::list]] [std::len xs]]").unwrap_err();
  assert!(error.message.contains("type annotation needed"), "{error}");
}

#[test]
fn variadic_builtin_can_be_called_through_local_binding() {
  check(
    "[fn main [] ->[List Int]
         [let make std::list]
         [make 1 2 3]]",
  )
  .unwrap();
}

#[test]
fn variadic_builtin_can_be_passed_to_annotated_parameter() {
  check(
    "[fn use-list [make:[Fn [...Int] -> [List Int]]] ->[List Int]
         [make 1 2 3]]
       [fn main [] ->[List Int]
         [use-list std::list]]",
  )
  .unwrap();
}

#[test]
fn map_accepts_top_level_function() {
  check(
    "[fn sq [x:Int] ->Int [std::+ x x]]
       [fn main [] ->[List Int] [std::map [std::range 0 5] sq]]",
  )
  .unwrap();
}

#[test]
fn filter_requires_a_bool_returning_predicate() {
  check(
    "[fn small [x:Int] ->Bool [std::< x 3]]
       [fn main [] ->[List Int] [std::filter [std::range 0 5] small]]",
  )
  .unwrap();

  let error = check(
    "[fn sq [x:Int] ->Int [std::* x x]]
       [fn main [] ->[List Int] [std::filter [std::range 0 5] sq]]",
  )
  .unwrap_err();
  assert!(error.message.contains("expected `Bool`"), "{error}");
}

#[test]
fn rigid_variable_cannot_be_replaced_by_a_concrete_type() {
  let error = check("[fn bad [a:A] ->A 5]").unwrap_err();
  assert!(error.message.contains("rigid type variable `A`"), "{error}");
}

#[test]
fn declared_trait_bound_allows_polymorphic_builtin_use() {
  check("[fn double [a:A] ->A where [[A Add]] [std::+ a a]]").unwrap();
}

#[test]
fn unbound_variable_bounds_are_merged() {
  let mut checker = Checker::empty();
  let add = checker.fresh(Some("add".to_string()), vec![Trait::Add]);
  let sub = checker.fresh(Some("sub".to_string()), vec![Trait::Sub]);
  checker.unify(add.clone(), sub).unwrap();
  let error = checker.unify(add, Type::String).unwrap_err();
  assert!(error.message.contains("does not satisfy trait"), "{error}");
}

#[test]
fn nested_function_can_capture_enclosing_type_variable() {
  check(
    "[fn outer [a:A] ->[Fn [A] -> A]
         [fn inner [ignored:A] ->A a]]",
  )
  .unwrap();
}

#[test]
fn nested_declared_function_is_generalized() {
  check(
    "[fn main [] ->Bool
         [let id [fn id [a:A] ->A a]]
         [id 1]
         [std::== [id \"x\"] \"x\"]]",
  )
  .unwrap();
}

#[test]
fn void_return_discards_the_final_expression_type() {
  check("[fn main [] [std::+ 1 2]]").unwrap();
}

#[test]
fn if_condition_must_be_bool() {
  let error = check("[fn main [] ->Int [if 1 2 3]]").unwrap_err();
  assert!(
    error.message.contains("expected `Bool`, got `Int`"),
    "{error}"
  );
}

#[test]
fn unknown_bare_value_name_is_reported_as_a_name_error() {
  let error = check("[fn main [] y]").unwrap_err();
  assert_eq!(error.message, "Unknown name `y`");
}

#[test]
fn bare_builtin_call_requires_prelude_resolution_before_typecheck() {
  let error = check("[fn main [] ->Int [+ 1 2]]").unwrap_err();
  assert_eq!(error.message, "unknown function `+`");
}

#[test]
fn binding_created_in_only_one_if_branch_is_not_available_afterward() {
  let error = check(
    "[fn main [] ->Int
         [if true [let x 1] [let y 2]]
         y]",
  )
  .unwrap_err();
  assert_eq!(error.message, "Unknown name `y`");
}

#[test]
fn nested_self_recursion_is_accepted() {
  check(
    "[fn main [] ->Int
         [fn recurse [n:Int] ->Int
           [if [std::== n 0] 0 [recurse [std::- n 1]]]]
         [recurse 3]]",
  )
  .unwrap();
}

#[test]
fn nested_mutual_recursion_is_accepted() {
  check(
    "[fn main [] ->Bool
         [fn even [n:Int] ->Bool
           [if [std::== n 0] true [odd [std::- n 1]]]]
         [fn odd [n:Int] ->Bool
           [if [std::== n 0] false [even [std::- n 1]]]]
         [even 4]]",
  )
  .unwrap();
}

#[test]
fn nested_function_is_not_visible_before_its_recursive_group() {
  let error = check(
    "[fn main [] ->Int
         [later]
         [fn later [] ->Int 1]]",
  )
  .unwrap_err();
  assert_eq!(error.message, "unknown function `later`");
}

#[test]
fn type_errors_point_to_the_offending_argument() {
  let source = "[fn main [] ->Int\n  [std::+ 1\n    \"not-an-int\"]]";
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
  let source = "[fn main [] ->Int\n  [std::+ 1\n    missing]]";
  let error = check(source).unwrap_err();
  let start = source.find("missing").unwrap();
  assert_eq!(error.span.as_ref().map(|span| span.start), Some(start));
  assert!(
    error.render(source).starts_with("line 3, column 5:"),
    "{error}"
  );
}

#[test]
fn tuple_typechecks_construction_and_field_access() {
  check(
    "[fn foo [] -> [Tuple Int String]
       [Tuple 3 \"foo\"]]
       [fn main [] -> Int
         [let result [foo]]
         result.0]",
  )
  .unwrap();
}

#[test]
fn tuple_records_receiver_kind_for_field_access() {
  let source = "
    [fn main [] -> String
      [let t [Tuple 1 \"two\"]]
      t.1]";
  let asts = read_multiple(source).unwrap();
  let asts = resolve_module_names("main", &asts, &[], &[]).unwrap();
  let checked = typecheck(asts, &Library::default()).unwrap();
  let asts = checked.asts();
  let info = checked.type_info();

  let ASTKind::DefineFn(main) = &asts[0].kind else {
    panic!("expected main function");
  };
  let field_access = &main.code[1];
  let ASTKind::FieldAccess(_, _) = &field_access.kind else {
    panic!("expected field access");
  };
  let field = info.field_access(field_access.id()).unwrap();
  assert_eq!(field.receiver_type(), "Tuple");
  assert_eq!(field.field_index(), 1);
}

#[test]
fn tuple_requires_at_least_two_elements() {
  let error = check("[fn main [] -> [Tuple Int] [Tuple 1]]").unwrap_err();
  assert!(error.message.contains("at least two"), "{error}");
}

#[test]
fn tuple_type_requires_at_least_two_arguments() {
  check("[fn main [] -> [Tuple Int Int] [Tuple 1 2]]").unwrap();
  let error = check("[fn main [] -> [Tuple Int] [Tuple 1 2]]").unwrap_err();
  assert!(error.message.contains("at least two"), "{error}");
}

#[test]
fn tuple_field_access_rejects_non_numeric_index() {
  let error = check(
    "[fn main [] -> Int
       [let t [Tuple 1 2]]
       t.x]",
  )
  .unwrap_err();
  assert!(error.message.contains("numeric index"), "{error}");
}

#[test]
fn tuple_field_access_rejects_out_of_range_index() {
  let error = check(
    "[fn main [] -> Int
       [let t [Tuple 1 2]]
       t.5]",
  )
  .unwrap_err();
  assert!(error.message.contains("out of range"), "{error}");
}

#[test]
fn tuple_field_access_rejects_non_tuple_receiver() {
  let error = check(
    "[fn main [] -> Int
       [let n [std::len [std::list 1 2]]]
       n.0]",
  )
  .unwrap_err();
  assert!(
    error
      .message
      .contains("field access expected a struct or tuple"),
    "{error}"
  );
}

#[test]
fn reserved_type_constructor_name_cannot_be_redefined() {
  let error = check("[struct Tuple a:Int b:Int]").unwrap_err();
  assert!(
    error.message.contains("reserved type constructor name"),
    "{error}"
  );
}
