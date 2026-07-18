use std::collections::HashSet;

#[cfg(test)]
use crate::parser::TypeAst;
use crate::parser::{
  try_map_ast_children, ASTKind, BindingId, Function, MatchArm, MatchPattern, ResolvedName, Span,
  AST,
};
use crate::prelude::resolve_module_names;

pub fn transform_closures_in_module(module_name: &str, items: &[AST]) -> Result<Vec<AST>, String> {
  //! There isn't technically anything called a "closure" in either the runtime or compile time of
  //! Safelisp. We represent closures as more general-purpose things: top-level functions and
  //! Partial Applications.
  //!
  //! Nested functions are lifted to top-level functions. Any variables they use from their
  //! definition environment are passed in as hidden parameters. Then, to actually represent the
  //! "closure" (i.e. the callable object which has the environment bound to it), we PartialApply
  //! the inner function with all the values that it uses.

  let resolved_items;
  let items = if items.iter().any(|item| {
    matches!(
      &item.kind,
      ASTKind::DefineFn(function) if !function.name.binding.is_resolved()
    )
  }) {
    resolved_items = resolve_module_names(module_name, items, &[], &[])?;
    resolved_items.as_slice()
  } else {
    items
  };

  let mut result = vec![];
  let mut names = ClosureNameAllocator::new(items);
  for item in items {
    let out = match &item.kind {
      ASTKind::DefineFn(func) => {
        closurize_function(module_name, func, item.span.clone(), &mut names)?
      }
      _ => vec![item.clone()],
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
  /// Variables that a function uses from the outer environment, before
  /// recursive sibling dependencies are propagated.
  captures: Vec<ResolvedName>,
  /// Functions in the current recursive group that this body needs as local
  /// closure values.
  recursive_refs: Vec<ResolvedName>,
}

struct RecursiveBinding {
  source_name: ResolvedName,
  lifted_name: ResolvedName,
}

fn closurize_function(
  module_name: &str,
  outer_func: &Function,
  source_span: Span,
  names: &mut ClosureNameAllocator,
) -> Result<Vec<AST>, String> {
  //! Transform a function into a simpler form where nested functions are lifted
  //! to the top level and their captured environment is threaded in as hidden
  //! parameters.
  let path = vec![outer_func.name.name.clone()];
  let result = transform_function(
    module_name,
    outer_func,
    outer_func.name.clone(),
    path,
    &HashSet::new(),
    source_span,
    names,
    &[],
  )?;
  Ok(result.lifted)
}

#[allow(clippy::too_many_arguments)]
fn transform_function(
  module_name: &str,
  func: &Function,
  transformed_name: ResolvedName,
  lexical_path: Vec<String>,
  environment: &HashSet<BindingId>,
  source_span: Span,
  names: &mut ClosureNameAllocator,
  recursive_bindings: &[RecursiveBinding],
) -> Result<TransformResult, String> {
  //! Rewrite a function into simpler AST that the compiler already knows how to
  //! turn into bytecode, while also returning the capture info its parent
  //! needs.
  //!
  //! During transformation:
  //! - captured variables become hidden leading parameters
  //! - nested `fn` definitions are lifted into `lifted` as separate functions
  //! - the original nested `fn` expression becomes `let name (partial-apply func [captures...])`
  let mut locals = HashSet::new();
  locals.extend(func.params.iter().map(|(name, _)| name.binding));
  locals.extend(
    recursive_bindings
      .iter()
      .map(|binding| binding.source_name.binding),
  );
  let mut lifted = vec![];
  let mut captures = vec![];
  let mut recursive_refs = vec![];
  let code = transform_sequence(
    module_name,
    &func.code,
    &lexical_path,
    environment,
    &mut locals,
    &mut captures,
    &mut lifted,
    names,
    recursive_bindings,
    &mut recursive_refs,
  )?;

  let mut params = vec![];
  params.extend(captures.iter().cloned().map(|name| (name, None)));
  params.extend(func.params.clone());
  lifted.push(AST::new(
    ASTKind::DefineFn(Function {
      name: transformed_name,
      params,
      return_type: func.return_type.clone(),
      bounds: func.bounds.clone(),
      code,
    }),
    source_span,
  ));
  Ok(TransformResult {
    lifted,
    captures,
    recursive_refs,
  })
}

#[allow(clippy::too_many_arguments)]
fn transform_sequence(
  module_name: &str,
  expressions: &[AST],
  lexical_path: &[String],
  environment: &HashSet<BindingId>,
  locals: &mut HashSet<BindingId>,
  captures: &mut Vec<ResolvedName>,
  lifted: &mut Vec<AST>,
  names: &mut ClosureNameAllocator,
  recursive_bindings: &[RecursiveBinding],
  recursive_refs: &mut Vec<ResolvedName>,
) -> Result<Vec<AST>, String> {
  let mut transformed = vec![];
  let mut index = 0;
  while index < expressions.len() {
    if matches!(expressions[index].kind, ASTKind::DefineFn(_)) {
      let end = nested_function_group_end(expressions, index);
      transformed.extend(transform_nested_function_group(
        module_name,
        &expressions[index..end],
        lexical_path,
        environment,
        locals,
        captures,
        lifted,
        names,
        recursive_bindings,
        recursive_refs,
      )?);
      index = end;
    } else {
      transformed.push(transform_ast(
        module_name,
        &expressions[index],
        lexical_path,
        environment,
        locals,
        captures,
        lifted,
        names,
        recursive_bindings,
        recursive_refs,
      )?);
      index += 1;
    }
  }
  Ok(transformed)
}

#[allow(clippy::too_many_arguments)]
fn transform_nested_function_group(
  module_name: &str,
  definitions: &[AST],
  lexical_path: &[String],
  environment: &HashSet<BindingId>,
  locals: &mut HashSet<BindingId>,
  captures: &mut Vec<ResolvedName>,
  lifted: &mut Vec<AST>,
  names: &mut ClosureNameAllocator,
  enclosing_recursive_bindings: &[RecursiveBinding],
  enclosing_recursive_refs: &mut Vec<ResolvedName>,
) -> Result<Vec<AST>, String> {
  let mut inner_environment = environment.clone();
  inner_environment.extend(locals.iter().cloned());

  let mut bindings = Vec::with_capacity(definitions.len());
  for definition in definitions {
    let ASTKind::DefineFn(function) = &definition.kind else {
      unreachable!("nested function groups contain only function definitions");
    };
    let mut inner_path = lexical_path.to_vec();
    inner_path.push(function.name.name.clone());
    bindings.push(RecursiveBinding {
      source_name: function.name.clone(),
      lifted_name: names.allocate(&inner_path),
    });
  }

  let mut results = Vec::with_capacity(definitions.len());
  for (definition, binding) in definitions.iter().zip(&bindings) {
    let ASTKind::DefineFn(function) = &definition.kind else {
      unreachable!("nested function groups contain only function definitions");
    };
    let mut inner_path = lexical_path.to_vec();
    inner_path.push(function.name.name.clone());
    results.push(transform_function(
      module_name,
      function,
      binding.lifted_name.clone(),
      inner_path,
      &inner_environment,
      definition.span.clone(),
      names,
      &bindings,
    )?);
  }

  let mut group_captures = results
    .iter()
    .map(|result| result.captures.clone())
    .collect::<Vec<_>>();
  // Partial applications are immutable, so recursive closures cannot capture
  // each other directly. Propagate ordinary captures until every function can
  // reconstruct the sibling closures it references.
  loop {
    let previous = group_captures.clone();
    for (index, result) in results.iter().enumerate() {
      for reference in &result.recursive_refs {
        let dependency = bindings
          .iter()
          .position(|binding| binding.source_name.binding == reference.binding)
          .expect("recursive references belong to the current group");
        for capture in &previous[dependency] {
          push_unique(&mut group_captures[index], capture.clone());
        }
      }
    }
    if group_captures == previous {
      break;
    }
  }

  for (index, result) in results.iter_mut().enumerate() {
    let definition = &definitions[index];
    let direct_capture_count = result.captures.len();
    let root = result
      .lifted
      .last_mut()
      .expect("transforming a function always emits its lifted definition");
    let ASTKind::DefineFn(function) = &mut root.kind else {
      unreachable!("the transformed function is emitted last");
    };

    let source_params = function.params.split_off(direct_capture_count);
    function.params = group_captures[index]
      .iter()
      .cloned()
      .map(|name| (name, None))
      .chain(source_params)
      .collect();

    let mut recursive_bindings = vec![];
    for reference in &result.recursive_refs {
      let dependency = bindings
        .iter()
        .position(|binding| binding.source_name.binding == reference.binding)
        .expect("recursive references belong to the current group");
      recursive_bindings.push(AST::new(
        ASTKind::Let(
          reference.clone(),
          None,
          Box::new(closure_expr(
            module_name,
            bindings[dependency].lifted_name.as_str(),
            &group_captures[dependency],
            definition.span.clone(),
          )),
        ),
        definition.span.clone(),
      ));
    }
    recursive_bindings.append(&mut function.code);
    function.code = recursive_bindings;
  }

  let mut transformed = Vec::with_capacity(definitions.len());
  for (((definition, binding), result), group_capture) in definitions
    .iter()
    .zip(&bindings)
    .zip(results)
    .zip(&group_captures)
  {
    let ASTKind::DefineFn(function) = &definition.kind else {
      unreachable!("nested function groups contain only function definitions");
    };
    for capture in group_capture {
      if recursive_binding(enclosing_recursive_bindings, capture.binding).is_some() {
        push_unique(enclosing_recursive_refs, capture.clone());
      } else if !locals.contains(&capture.binding) && environment.contains(&capture.binding) {
        push_unique(captures, capture.clone());
      }
    }
    lifted.extend(result.lifted);
    transformed.push(definition.with_kind(ASTKind::Let(
      function.name.clone(),
      None,
      Box::new(closure_expr(
        module_name,
        binding.lifted_name.as_str(),
        group_capture,
        definition.span.clone(),
      )),
    )));
  }
  locals.extend(
    bindings
      .into_iter()
      .map(|binding| binding.source_name.binding),
  );
  Ok(transformed)
}

#[allow(clippy::too_many_arguments)]
fn transform_ast(
  module_name: &str,
  ast: &AST,
  lexical_path: &[String],
  environment: &HashSet<BindingId>,
  locals: &mut HashSet<BindingId>,
  captures: &mut Vec<ResolvedName>,
  lifted: &mut Vec<AST>,
  names: &mut ClosureNameAllocator,
  recursive_bindings: &[RecursiveBinding],
  recursive_refs: &mut Vec<ResolvedName>,
) -> Result<AST, String> {
  //! Do closure transformations on one expression inside a function body,
  //! recording captures as they are discovered.
  match &ast.kind {
    ASTKind::Let(name, annotation, expr) => {
      let expr = transform_ast(
        module_name,
        expr,
        lexical_path,
        environment,
        locals,
        captures,
        lifted,
        names,
        recursive_bindings,
        recursive_refs,
      )?;
      locals.insert(name.binding);
      Ok(ast.with_kind(ASTKind::Let(
        name.clone(),
        annotation.clone(),
        Box::new(expr),
      )))
    }
    ASTKind::DefineFn(_) => {
      let mut transformed = transform_nested_function_group(
        module_name,
        std::slice::from_ref(ast),
        lexical_path,
        environment,
        locals,
        captures,
        lifted,
        names,
        recursive_bindings,
        recursive_refs,
      )?;
      Ok(
        transformed
          .pop()
          .expect("a singleton function group emits one binding"),
      )
    }
    ASTKind::Variable(name) => {
      if recursive_binding(recursive_bindings, name.binding).is_some()
        && locals.contains(&name.binding)
      {
        push_unique(recursive_refs, name.clone());
      }
      if !locals.contains(&name.binding) && environment.contains(&name.binding) {
        push_unique(captures, name.clone());
      }
      Ok(ast.clone())
    }
    ASTKind::Block(body) => Ok(ast.with_kind(ASTKind::Block(transform_sequence(
      module_name,
      body,
      lexical_path,
      environment,
      locals,
      captures,
      lifted,
      names,
      recursive_bindings,
      recursive_refs,
    )?))),
    ASTKind::Match(scrutinee, arms) => {
      // It's kind of unfortunate that we have to know about matches in closure
      // transformation; a better design would be to have a separate IR with
      // simpler forms. e.g. a match would be transformed to bindings +
      // conditionals + scopes and this closure would not need to treat "block
      // with a let" any different from match arms.
      let scrutinee = transform_ast(
        module_name,
        scrutinee,
        lexical_path,
        environment,
        locals,
        captures,
        lifted,
        names,
        recursive_bindings,
        recursive_refs,
      )?;
      let mut transformed_arms = Vec::with_capacity(arms.len());
      for arm in arms {
        let mut arm_locals = locals.clone();
        if let MatchPattern::Variant { fields, .. } = &arm.pattern {
          arm_locals.extend(fields.iter().map(|field| field.binding));
        }
        transformed_arms.push(MatchArm {
          pattern: arm.pattern.clone(),
          body: transform_ast(
            module_name,
            &arm.body,
            lexical_path,
            environment,
            &mut arm_locals,
            captures,
            lifted,
            names,
            recursive_bindings,
            recursive_refs,
          )?,
        });
      }
      Ok(ast.with_kind(ASTKind::Match(Box::new(scrutinee), transformed_arms)))
    }
    _ => try_map_ast_children(ast, |child| {
      transform_ast(
        module_name,
        child,
        lexical_path,
        environment,
        locals,
        captures,
        lifted,
        names,
        recursive_bindings,
        recursive_refs,
      )
    }),
  }
}

fn recursive_binding(
  bindings: &[RecursiveBinding],
  binding: BindingId,
) -> Option<&RecursiveBinding> {
  bindings
    .iter()
    .find(|candidate| candidate.source_name.binding == binding)
}

fn nested_function_group_end(expressions: &[AST], start: usize) -> usize {
  let mut names = HashSet::new();
  let mut end = start;
  while let Some(AST {
    kind: ASTKind::DefineFn(function),
    ..
  }) = expressions.get(end)
  {
    if !names.insert(function.name.as_str()) {
      break;
    }
    end += 1;
  }
  end
}

fn closure_expr(
  module_name: &str,
  lifted_name: &str,
  captures: &[ResolvedName],
  span: Span,
) -> AST {
  //! Generate a PartialApply of a closure with its captures
  let func_ref = AST::new(
    ASTKind::FunctionRef(module_name.to_string(), lifted_name.to_string()),
    span.clone(),
  );
  if captures.is_empty() {
    func_ref
  } else {
    AST::new(
      ASTKind::PartialApply(
        Box::new(func_ref),
        captures
          .iter()
          .cloned()
          .map(|name| AST::new(ASTKind::Variable(name), span.clone()))
          .collect(),
      ),
      span,
    )
  }
}

fn push_unique(items: &mut Vec<ResolvedName>, item: ResolvedName) {
  if !items
    .iter()
    .any(|existing| existing.binding == item.binding)
  {
    items.push(item);
  }
}

struct ClosureNameAllocator {
  used: HashSet<String>,
  next_binding: u32,
}

impl ClosureNameAllocator {
  fn new(items: &[AST]) -> Self {
    let used = items
      .iter()
      .filter_map(|item| match &item.kind {
        ASTKind::DefineFn(func) => Some(func.name.name.clone()),
        _ => None,
      })
      .collect();
    Self {
      used,
      next_binding: 0,
    }
  }

  fn allocate(&mut self, lexical_path: &[String]) -> ResolvedName {
    let base = mangle_closure_name(lexical_path);
    if self.used.insert(base.clone()) {
      return self.synthetic_name(base);
    }

    let mut disambiguator = 1;
    loop {
      let candidate = format!("{}:{}:(closure)", lexical_path.join(":"), disambiguator);
      if self.used.insert(candidate.clone()) {
        return self.synthetic_name(candidate);
      }
      disambiguator += 1;
    }
  }

  fn synthetic_name(&mut self, name: String) -> ResolvedName {
    let binding = BindingId::synthetic(self.next_binding);
    self.next_binding += 1;
    ResolvedName::resolved(name, binding)
  }
}

fn mangle_closure_name(lexical_path: &[String]) -> String {
  format!("{}:(closure)", lexical_path.join(":"))
}

#[cfg(test)]
mod closure_tests;
