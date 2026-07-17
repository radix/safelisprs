use std::collections::HashMap;

use crate::builtins::BuiltinSpec;
use crate::closure::transform_closures_in_module;
use crate::parser::{self, ASTKind, BindingId, Identifier, MatchPattern, ResolvedName, AST};
use crate::prelude::resolve_module_names;
use crate::typecheck::{
  hidden_impl_method_name, CheckedModule, DictionaryArg, DictionaryParam, MatchArmInfo,
  TypecheckInfo,
};

/// A Package can either represent a "program" or a "library".
/// If a `main` is provided, then it can be executed as a program directly.
#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Package {
  /// While the names here *can* be used for function lookup, they are only
  /// included for debugging purposes. In a complete "executable" package, all
  /// calls will be represented with index-based function offsets.
  pub modules: LinkedModules,
  pub main: Option<(u32, u32)>,
}

pub type LinkedCallable = Callable<(u32, u32), (u32, u32)>;
type CompiledCallable = Callable<(String, String), (String, String)>;

/// Packages contain Callables, which can either be LinkedFunctions or
/// Builtins. This is so the interpreter can know whether it should fall back to
/// the builtins when invoking a function. Builtin doesn't need a name because
/// it's already in the Package::functions data.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Callable<CallType, StructType> {
  Function(Function<CallType, StructType>),
  Builtin,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Function<CallType, StructType> {
  pub num_params: u16,
  pub num_locals: u16,
  pub instructions: Vec<Instruction<CallType, StructType>>,
}

/// Instructions are parameterized by the representation of function calls.
/// During compilation, functions are referenced by name, but they are later
/// "linked" and turned into direct offsets into the function table.
///
/// TODO: This Instruction type is BIG. I'm guessing that reducing its size down
/// to, say, 64 bits would lead to some wins?
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Instruction<CallType, StructType> {
  /// loads local variable onto the stack
  LoadLocal(u16),
  /// assigns top of the stack to local variable.
  SetLocal(u16),
  PushInt(i64),
  PushFloat(f64),
  PushString(String),
  PushBool(bool),
  PushVoid,
  /// Pop all declared fields and allocate a heap-backed struct instance.
  NewStruct(StructType),
  /// Pop all variant fields and allocate a heap-backed enum instance.
  NewEnum(StructType, u16),
  /// Pop a struct value and push the field at this declaration-order offset.
  GetField(u16),
  /// Pop an enum value and push whether it has this variant index.
  // GetEnumVariant would be more flexible but less performant...
  IsEnumVariant(u16),
  /// Pop an enum value and push the field at this variant-field offset.
  GetEnumField(u16),
  /// discards topmost stack item
  Pop,
  /// Call the function at `(module, function)` in the function table.
  /// The `u16` is the number of arguments pushed at the call site, used by the
  /// interpreter to pop the right number of args (and to arity-check). This is
  /// essential for variadic builtins, whose `BuiltinSpec::num_params` is
  /// `None`.
  Call(CallType, u16),
  /// Call a FunctionRef or a Closure at TOS. The `u16` is the number of
  /// arguments pushed at the call site (see [`Instruction::Call`]).
  CallDynamic(u16),
  /// Exit the current function, returning the TOS to the caller
  Return,
  /// Push a reference to a function
  MakeFunctionRef(CallType),
  /// Partially apply some arguments to a function.
  /// argument: how many arguments to pop from the stack and bind to the function.
  /// TOS: a function reference
  PartialApply(u16),
  /// Jump to a relative offset.
  ///
  /// The offset is relative to the instruction *following* this one (the IP
  /// has already been advanced past the `Jump` when it executes), so `Jump(0)`
  /// is a no-op.
  Jump(u32),
  /// Pop the TOS; if it is false, add `offset` to the instruction pointer.
  /// See [`Instruction::Jump`] for the relative-offset semantics.
  JumpIfFalse(u32),
}

pub type LinkedFunction = Function<(u32, u32), (u32, u32)>;
type CompiledFunction = Function<(String, String), (String, String)>;

type LinkedInstruction = Instruction<(u32, u32), (u32, u32)>;
type CompiledInstruction = Instruction<(String, String), (String, String)>;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TypeDef {
  pub name: String,
  pub constructors: Vec<ConstructorDef>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ConstructorDef {
  pub name: String,
  pub fields: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Module<CallType, StructType> {
  pub name: String,
  pub functions: Vec<(String, Callable<CallType, StructType>)>,
  pub types: Vec<TypeDef>,
}

type CompiledModule = Module<(String, String), (String, String)>;
type CompiledModules = Vec<CompiledModule>;
pub type LinkedModule = Module<(u32, u32), (u32, u32)>;
pub type LinkedModules = Vec<LinkedModule>;

struct ModuleIndexEntry {
  module: u32,
  functions: HashMap<String, u32>,
  types: HashMap<String, u32>,
}

type ModuleIndex = HashMap<String, ModuleIndexEntry>;

impl Package {
  pub fn from_modules(modules: CompiledModules, specs: &[BuiltinSpec]) -> Result<Self, String> {
    let modules = inject_builtin_specs(modules, specs)?;
    let index = index_modules(modules.iter());
    Ok(Package {
      modules: link(&index, modules)?,
      main: None,
    })
  }

  pub fn from_modules_with_main(
    compiled_modules: CompiledModules,
    path: (&str, &str),
    specs: &[BuiltinSpec],
  ) -> Result<Self, String> {
    let compiled_modules = inject_builtin_specs(compiled_modules, specs)?;
    let index = index_modules(compiled_modules.iter());
    let linked_modules = link(&index, compiled_modules)?;
    let main = find_function(&index, path.0, path.1);
    if main.is_none() {
      Err(format!("Main function {path:?} was not found!"))
    } else {
      Ok(Package {
        modules: linked_modules,
        main,
      })
    }
  }

  pub fn get_module(&self, mod_index: u32) -> Option<&LinkedModule> {
    self.modules.get(mod_index as usize)
  }

  pub fn get_function(&self, module: u32, function: u32) -> Option<&LinkedCallable> {
    self
      .modules
      .get(module as usize)
      .and_then(|m| m.functions.get(function as usize))
      .map(|(_, f)| f)
  }

  pub fn get_type(&self, module: u32, type_: u32) -> Option<&TypeDef> {
    self
      .modules
      .get(module as usize)
      .and_then(|m| m.types.get(type_ as usize))
  }

  pub fn get_struct(&self, module: u32, struct_: u32) -> Option<&ConstructorDef> {
    self
      .get_type(module, struct_)
      .and_then(|type_| type_.constructors.first())
  }

  pub fn get_enum(&self, module: u32, enum_: u32) -> Option<&TypeDef> {
    self.get_type(module, enum_)
  }
}

fn index_modules<'a>(modules: impl Iterator<Item = &'a CompiledModule>) -> ModuleIndex {
  let mut module_table = hashmap! {};
  for (mod_index, module) in modules.enumerate() {
    let mut entry = ModuleIndexEntry {
      module: mod_index as u32,
      functions: hashmap! {},
      types: hashmap! {},
    };
    for (func_index, (func_name, _)) in module.functions.iter().enumerate() {
      entry
        .functions
        .insert(func_name.to_string(), func_index as u32);
    }
    for (type_index, type_) in module.types.iter().enumerate() {
      entry
        .types
        .insert(type_.name.to_string(), type_index as u32);
    }
    module_table.insert(module.name.to_string(), entry);
  }
  module_table
}

fn link(module_table: &ModuleIndex, modules: CompiledModules) -> Result<LinkedModules, String> {
  //! In a set of modules, replace all String-based references to functions and modules with
  //! index-based references.
  let mut result = vec![];
  for module in modules {
    // consuming
    let new_functions = module
      .functions
      .into_iter()
      .map(|(func_name, callable)| {
        let new_callable = match callable {
          Callable::Function(function) => {
            Callable::Function(link_instructions(module_table, function)?)
          }
          Callable::Builtin => Callable::Builtin,
        };
        Ok((func_name, new_callable))
      })
      .collect::<Result<Vec<_>, String>>()?;
    result.push(LinkedModule {
      name: module.name,
      functions: new_functions,
      types: module.types,
    });
  }
  Ok(result)
}

fn link_instructions(
  module_table: &ModuleIndex,
  function: CompiledFunction,
) -> Result<LinkedFunction, String> {
  //! In a function, replace all string-based references to functions and modules with index-based
  //! references.
  let instructions = function
    .instructions
    .into_iter()
    .map(|i| link_instruction(module_table, i))
    .collect::<Result<Vec<_>, _>>()?;
  Ok(LinkedFunction {
    num_params: function.num_params,
    num_locals: function.num_locals,
    instructions,
  })
}

fn link_instruction(
  module_table: &ModuleIndex,
  instruction: CompiledInstruction,
) -> Result<LinkedInstruction, String> {
  Ok(match instruction {
    Instruction::Call((mod_name, func_name), arity) => {
      let (mod_idx, func_idx) = find_function(module_table, &mod_name, &func_name)
        .ok_or_else(|| format!("Call to undefined function {}::{}", mod_name, func_name))?;
      Instruction::Call((mod_idx, func_idx), arity)
    }
    Instruction::MakeFunctionRef((mod_name, func_name)) => {
      let (mod_idx, func_idx) = find_function(module_table, &mod_name, &func_name)
        .ok_or_else(|| format!("Call to undefined function {}::{}", mod_name, func_name))?;
      Instruction::MakeFunctionRef((mod_idx, func_idx))
    }
    Instruction::NewStruct((mod_name, struct_name)) => {
      let (mod_idx, struct_idx) =
        find_type(module_table, &mod_name, &struct_name).ok_or_else(|| {
          format!(
            "Construction of undefined struct {}.{}",
            mod_name, struct_name
          )
        })?;
      Instruction::NewStruct((mod_idx, struct_idx))
    }
    Instruction::NewEnum((mod_name, enum_name), variant) => {
      let (mod_idx, enum_idx) = find_type(module_table, &mod_name, &enum_name)
        .ok_or_else(|| format!("Construction of undefined enum {}.{}", mod_name, enum_name))?;
      Instruction::NewEnum((mod_idx, enum_idx), variant)
    }

    // Here's what I want to say:
    // x => Ok(x),
    // but Rust isn't smart enough to allow me. So I have to list out every variant of Instruction
    // :(
    Instruction::CallDynamic(arity) => Instruction::CallDynamic(arity),
    Instruction::LoadLocal(num) => Instruction::LoadLocal(num),
    Instruction::SetLocal(num) => Instruction::SetLocal(num),
    Instruction::PushInt(num) => Instruction::PushInt(num),
    Instruction::PushFloat(f) => Instruction::PushFloat(f),
    Instruction::PushString(string) => Instruction::PushString(string),
    Instruction::PushBool(b) => Instruction::PushBool(b),
    Instruction::PushVoid => Instruction::PushVoid,
    Instruction::GetField(field) => Instruction::GetField(field),
    Instruction::IsEnumVariant(variant) => Instruction::IsEnumVariant(variant),
    Instruction::GetEnumField(field) => Instruction::GetEnumField(field),
    Instruction::Pop => Instruction::Pop,
    Instruction::Return => Instruction::Return,
    Instruction::PartialApply(size) => Instruction::PartialApply(size),
    Instruction::Jump(target) => Instruction::Jump(target),
    Instruction::JumpIfFalse(target) => Instruction::JumpIfFalse(target),
  })
}

fn find_function(
  index: &ModuleIndex,
  module_name: &str,
  function_name: &str,
) -> Option<(u32, u32)> {
  index.get(module_name).and_then(|entry| {
    entry
      .functions
      .get(function_name)
      .map(|func_index| (entry.module, *func_index))
  })
}

fn find_type(index: &ModuleIndex, module_name: &str, type_name: &str) -> Option<(u32, u32)> {
  index.get(module_name).and_then(|entry| {
    entry
      .types
      .get(type_name)
      .map(|type_index| (entry.module, *type_index))
  })
}

fn compile_resolved_module(
  module_name: &str,
  checked: &CheckedModule,
) -> Result<CompiledModule, String> {
  let asts = transform_closures_in_module(module_name, checked.asts())?;
  ModuleCompiler::new(module_name, &asts, checked.type_info()).compile(&asts)
}

struct ModuleCompiler<'types> {
  module_name: String,
  type_indices: HashMap<String, usize>,
  type_defs: Vec<TypeDef>,
  type_info: &'types TypecheckInfo,
}

impl<'types> ModuleCompiler<'types> {
  fn new(module_name: &str, asts: &[AST], type_info: &'types TypecheckInfo) -> Self {
    let mut type_indices = HashMap::new();
    let mut type_defs = vec![];
    for ast in asts {
      match &ast.kind {
        ASTKind::DefineStruct(struct_) => {
          type_indices.insert(struct_.name.clone(), type_defs.len());
          type_defs.push(TypeDef {
            name: struct_.name.clone(),
            constructors: vec![ConstructorDef {
              name: struct_.name.clone(),
              fields: struct_
                .fields
                .iter()
                .map(|(field, _)| field.clone())
                .collect(),
            }],
          });
        }
        ASTKind::DefineEnum(enum_) => {
          type_indices.insert(enum_.name.clone(), type_defs.len());
          type_defs.push(TypeDef {
            name: enum_.name.clone(),
            constructors: enum_
              .variants
              .iter()
              .map(|variant| ConstructorDef {
                name: variant.name.clone(),
                fields: variant
                  .fields
                  .iter()
                  .map(|(field, _)| field.clone())
                  .collect(),
              })
              .collect(),
          });
        }
        _ => {}
      }
    }
    Self {
      module_name: module_name.to_string(),
      type_indices,
      type_defs,
      type_info,
    }
  }

  fn compile(self, asts: &[AST]) -> Result<CompiledModule, String> {
    let mut functions = vec![];
    for ast in asts {
      match &ast.kind {
        ASTKind::DefineStruct(_) => {}
        ASTKind::DefineEnum(_) => {}
        ASTKind::DefineTrait(_) => {}
        ASTKind::DefineImpl(impl_) => {
          functions.extend(self.compile_impl_methods(impl_)?);
        }
        ASTKind::DefineFn(func) => functions.push(self.compile_function(func)?),
        x => return Err(format!("Unexpected form at top-level: {:?}", x)),
      };
    }
    Ok(CompiledModule {
      name: self.module_name,
      functions,
      types: self.type_defs,
    })
  }

  fn compile_function(&self, f: &parser::Function) -> Result<(String, CompiledCallable), String> {
    FunctionCompiler::new(self, f).compile(f)
  }

  fn compile_impl_methods(
    &self,
    impl_: &parser::ImplDef,
  ) -> Result<Vec<(String, CompiledCallable)>, String> {
    let target_key = type_ast_key(&impl_.target);
    let mut functions = Vec::with_capacity(impl_.methods.len());
    for method in &impl_.methods {
      let hidden_name =
        hidden_impl_method_name(&impl_.trait_name, &target_key, method.name.as_str());
      let mut method = method.clone();
      method.name.name = hidden_name;
      functions.push(self.compile_function(&method)?);
    }
    Ok(functions)
  }

  fn struct_def(&self, name: &str) -> Result<&ConstructorDef, String> {
    self
      .type_indices
      .get(name)
      .and_then(|index| self.type_defs.get(*index))
      .and_then(|type_| type_.constructors.first())
      .ok_or_else(|| format!("unknown struct `{name}`"))
  }

  fn enum_variant(&self, name: &str, variant: &str) -> Result<(u16, &ConstructorDef), String> {
    let enum_ = self
      .type_indices
      .get(name)
      .and_then(|index| self.type_defs.get(*index))
      .ok_or_else(|| format!("unknown enum `{name}`"))?;
    enum_
      .constructors
      .iter()
      .enumerate()
      .find(|(_, candidate)| candidate.name == variant)
      .map(|(index, variant)| {
        u16::try_from(index)
          .map(|index| (index, variant))
          .map_err(|_| format!("enum `{name}` has too many variants"))
      })
      .transpose()?
      .ok_or_else(|| format!("unknown variant `{variant}` for enum `{name}`"))
  }
}

struct FunctionCompiler<'module, 'types> {
  module: &'module ModuleCompiler<'types>,
  locals: HashMap<BindingId, u16>,
  dictionary_params: Vec<DictionaryParam>,
  instructions: Vec<CompiledInstruction>,
}

impl<'module, 'types> FunctionCompiler<'module, 'types> {
  fn new(module: &'module ModuleCompiler<'types>, f: &parser::Function) -> Self {
    let mut locals = HashMap::new();
    let mut dictionary_params = module
      .type_info
      .function_dictionaries(&module.module_name, f.name.as_str())
      .to_vec();
    if dictionary_params.is_empty() {
      dictionary_params = dictionary_params_from_bounds(module.type_info, &f.bounds);
    }
    let hidden_params = dictionary_params.len() as u16;
    for idx in 0..hidden_params {
      locals.insert(BindingId::synthetic(u32::from(idx)), idx);
    }
    for (idx, (param, _)) in f.params.iter().enumerate() {
      locals.insert(param.binding, hidden_params + idx as u16);
    }
    Self {
      module,
      locals,
      dictionary_params,
      instructions: vec![],
    }
  }

  fn compile(mut self, f: &parser::Function) -> Result<(String, CompiledCallable), String> {
    self.compile_body(&f.code, f.returns_void())?;
    self.emit(Instruction::Return);
    Ok((
      f.name.name.clone(),
      Callable::Function(Function {
        num_params: (self.dictionary_params.len() + f.params.len()) as u16,
        num_locals: self.locals.len() as u16,
        instructions: self.instructions,
      }),
    ))
  }

  fn compile_body(&mut self, body: &[AST], returns_void: bool) -> Result<(), String> {
    let last_index = body.len().saturating_sub(1);
    for (index, expression) in body.iter().enumerate() {
      self.compile_expr(expression)?;
      // Non-final body expressions are in statement position: their value is
      // discarded, so pop it off the stack to keep the stack clean. It would be
      // really nice to avoid pushing things to the stack entirely if we know they
      // are not going to be used, but that will probably take some more
      // thought/refactoring.
      if index != last_index || returns_void {
        self.emit(Instruction::Pop);
      }
    }
    if returns_void {
      self.emit(Instruction::PushVoid);
    }
    Ok(())
  }

  fn emit(&mut self, instruction: CompiledInstruction) -> usize {
    let position = self.instructions.len();
    self.instructions.push(instruction);
    position
  }

  fn patch_jump_to_here(&mut self, position: usize) -> Result<(), String> {
    let offset = self
      .instructions
      .len()
      .checked_sub(position + 1)
      .ok_or_else(|| format!("jump position {position} is past the instruction stream"))?
      as u32;
    match &mut self.instructions[position] {
      Instruction::Jump(target) | Instruction::JumpIfFalse(target) => {
        *target = offset;
        Ok(())
      }
      instruction => Err(format!(
        "cannot patch non-jump instruction at {position}: {instruction:?}"
      )),
    }
  }

  fn ensure_local(&mut self, name: &ResolvedName) -> Result<u16, String> {
    if !self.locals.contains_key(&name.binding) {
      let index =
        u16::try_from(self.locals.len()).map_err(|_| "function has too many locals".to_string())?;
      self.locals.insert(name.binding, index);
    }
    Ok(*self.locals.get(&name.binding).expect("local was inserted"))
  }

  fn alloc_temp_local(&mut self) -> Result<u16, String> {
    let index =
      u16::try_from(self.locals.len()).map_err(|_| "function has too many locals".to_string())?;
    let synthetic = BindingId::synthetic(u32::from(index));
    self.locals.insert(synthetic, index);
    Ok(index)
  }

  /// Compile `ast` into instructions.
  fn compile_expr(&mut self, ast: &AST) -> Result<(), String> {
    match &ast.kind {
      ASTKind::Call(callable, args) => {
        // CallDynamic expects the callable above its arguments on the stack, so
        // dynamic calls intentionally evaluate arguments before the callable.
        for argument in args {
          self.compile_expr(argument)?;
        }
        self.compile_expr(callable)?;
        self.emit(Instruction::CallDynamic(args.len() as u16));
      }
      ASTKind::CallFixed(identifier, args) => {
        if let Some(trait_call) = self.module.type_info.trait_call(ast.id()) {
          for argument in args {
            self.compile_expr(argument)?;
          }
          self.compile_dictionary_arg(&trait_call.method)?;
          self.emit(Instruction::CallDynamic(args.len() as u16));
          return Ok(());
        }
        let (module_name, function_name) = match identifier {
          Identifier::Qualified(module, function) => (module.clone(), function.clone()),
          Identifier::Bare(function) => {
            return Err(format!("call to unknown function: {function}"))
          }
        };
        for dictionary in self.module.type_info.call_dictionaries(ast.id()) {
          self.compile_dictionary_arg(dictionary)?;
        }
        for argument in args {
          self.compile_expr(argument)?;
        }
        self.emit(Instruction::Call(
          (module_name, function_name),
          (args.len() + self.module.type_info.call_dictionaries(ast.id()).len()) as u16,
        ));
      }
      ASTKind::FunctionRef(module, name) => {
        self.emit(Instruction::MakeFunctionRef((module.clone(), name.clone())));
      }
      ASTKind::Let(name, _, expression) => {
        let local_index = self.ensure_local(name)?;
        self.compile_expr(expression)?;
        self.emit(Instruction::SetLocal(local_index));
        self.emit(Instruction::LoadLocal(local_index));
      }
      ASTKind::PartialApply(callable, args) => {
        // PartialApply uses the same stack layout as CallDynamic.
        for argument in args {
          self.compile_expr(argument)?;
        }
        self.compile_expr(callable)?;
        self.emit(Instruction::PartialApply(args.len() as u16));
      }
      ASTKind::NewStruct(name, fields) => {
        let field_names = self.module.struct_def(name)?.fields.clone();
        for field_name in field_names {
          let expression = fields
            .iter()
            .find(|(field, _)| field == &field_name)
            .map(|(_, expression)| expression)
            .ok_or_else(|| format!("missing initializer for field `{field_name}` of `{name}`"))?;
          self.compile_expr(expression)?;
        }
        self.emit(Instruction::NewStruct((
          self.module.module_name.clone(),
          name.clone(),
        )));
      }
      ASTKind::NewEnum(name, variant, fields) => {
        let (variant_index, variant_def) = self.module.enum_variant(name, variant)?;
        let field_names = variant_def.fields.clone();
        for field_name in field_names {
          let expression = fields
            .iter()
            .find(|(field, _)| field == &field_name)
            .map(|(_, expression)| expression)
            .ok_or_else(|| {
              format!("missing initializer for field `{field_name}` of `{name}::{variant}`")
            })?;
          self.compile_expr(expression)?;
        }
        self.emit(Instruction::NewEnum(
          (self.module.module_name.clone(), name.clone()),
          variant_index,
        ));
      }
      ASTKind::FieldAccess(receiver, _) => {
        let field_index = self
          .module
          .type_info
          .field_access(ast.id())
          .map(|field| field.field_index())
          .ok_or_else(|| format!("field access has no typechecking information: {ast:?}"))?;
        self.compile_expr(receiver)?;
        self.emit(Instruction::GetField(field_index));
      }
      ASTKind::Match(scrutinee, arms) => {
        let match_info = self
          .module
          .type_info
          .match_info(ast.id())
          .ok_or_else(|| format!("match has no typechecking information: {ast:?}"))?
          .clone();
        let scrutinee_local = self.alloc_temp_local()?;
        self.compile_expr(scrutinee)?;
        self.emit(Instruction::SetLocal(scrutinee_local));

        let mut end_jumps = Vec::new();
        for (arm, arm_info) in arms.iter().zip(match_info.arms()) {
          match (&arm.pattern, arm_info) {
            (
              MatchPattern::Variant { variant: _, fields },
              MatchArmInfo::Variant {
                variant_index,
                field_indices,
              },
            ) => {
              self.emit(Instruction::LoadLocal(scrutinee_local));
              self.emit(Instruction::IsEnumVariant(*variant_index));
              let next_arm_jump = self.emit(Instruction::JumpIfFalse(0));
              for (field, field_index) in fields.iter().zip(field_indices) {
                let local_index = self.ensure_local(field)?;
                self.emit(Instruction::LoadLocal(scrutinee_local));
                self.emit(Instruction::GetEnumField(*field_index));
                self.emit(Instruction::SetLocal(local_index));
              }
              self.compile_expr(&arm.body)?;
              end_jumps.push(self.emit(Instruction::Jump(0)));
              self.patch_jump_to_here(next_arm_jump)?;
            }
            (MatchPattern::Default, MatchArmInfo::Default) => {
              self.compile_expr(&arm.body)?;
              end_jumps.push(self.emit(Instruction::Jump(0)));
            }
            _ => {
              return Err("match pattern and typechecking metadata disagree".to_string());
            }
          }
        }
        for jump in end_jumps {
          self.patch_jump_to_here(jump)?;
        }
      }
      ASTKind::If(condition, then_branch, else_branch) => {
        // Emit placeholder jumps, then patch their relative offsets once both
        // branch lengths are known.
        self.compile_expr(condition)?;
        let jump_else = self.emit(Instruction::JumpIfFalse(0));
        self.compile_expr(then_branch)?;
        let jump_end = self.emit(Instruction::Jump(0));
        self.patch_jump_to_here(jump_else)?;
        self.compile_expr(else_branch)?;
        self.patch_jump_to_here(jump_end)?;
      }
      ASTKind::Block(body) => {
        self.compile_body(body, false)?;
      }
      ASTKind::Variable(name) => {
        let local_index = self
          .locals
          .get(&name.binding)
          .ok_or_else(|| format!("Function accesses unbound variable {name}"))?;
        self.emit(Instruction::LoadLocal(*local_index));
      }
      ASTKind::Int(value) => {
        self.emit(Instruction::PushInt(*value));
      }
      ASTKind::Float(value) => {
        self.emit(Instruction::PushFloat(*value));
      }
      ASTKind::String(value) => {
        self.emit(Instruction::PushString(value.clone()));
      }
      ASTKind::Bool(value) => {
        self.emit(Instruction::PushBool(*value));
      }
      ASTKind::DefineStruct(_) => {
        return Err("Unexpected struct definition in expression".to_string())
      }
      ASTKind::DefineEnum(_) => return Err("Unexpected enum definition in expression".to_string()),
      ASTKind::DefineTrait(_) => {
        return Err("Unexpected trait definition in expression".to_string())
      }
      ASTKind::DefineImpl(_) => return Err("Unexpected impl definition in expression".to_string()),
      kind => return Err(format!("Unexpected form at top-level: {kind:?}")),
    }
    Ok(())
  }

  fn compile_dictionary_arg(&mut self, dictionary: &DictionaryArg) -> Result<(), String> {
    match dictionary {
      DictionaryArg::ImplMethod { function } => {
        self.emit(Instruction::MakeFunctionRef((
          self.module.module_name.clone(),
          function.clone(),
        )));
      }
      DictionaryArg::BoundMethod {
        type_var,
        trait_name,
        method_slot,
      } => {
        let local = self.dictionary_local(type_var, trait_name, *method_slot)?;
        self.emit(Instruction::LoadLocal(local));
      }
    }
    Ok(())
  }

  fn dictionary_local(
    &self,
    type_var: &str,
    trait_name: &str,
    method_slot: u16,
  ) -> Result<u16, String> {
    self
      .dictionary_params
      .iter()
      .position(|param| {
        param.type_var == type_var
          && param.trait_name == trait_name
          && param.method_slot == method_slot
      })
      .map(|index| index as u16)
      .ok_or_else(|| {
        format!("missing dictionary parameter for `{type_var} {trait_name}` slot {method_slot}")
      })
  }
}

fn type_ast_key(ty: &parser::TypeAst) -> String {
  match ty {
    parser::TypeAst::Named(name) => name.clone(),
    parser::TypeAst::Apply(name, args) => {
      let mut key = name.clone();
      for arg in args {
        key.push('_');
        key.push_str(&type_ast_key(arg));
      }
      key
    }
    parser::TypeAst::Fn(_, _, _) => "Fn".to_string(),
  }
}

fn dictionary_params_from_bounds(
  type_info: &TypecheckInfo,
  bounds: &[parser::Bound],
) -> Vec<DictionaryParam> {
  let mut params = Vec::new();
  for bound in bounds {
    for trait_name in &bound.traits {
      let Some(trait_) = type_info
        .traits()
        .iter()
        .find(|trait_| trait_.name == *trait_name)
      else {
        continue;
      };
      for (slot, _) in trait_.methods.iter().enumerate() {
        if let Ok(method_slot) = u16::try_from(slot) {
          params.push(DictionaryParam {
            type_var: bound.var.clone(),
            trait_name: trait_name.clone(),
            method_slot,
          });
        }
      }
    }
  }
  params
}

pub fn compile_executable_from_source(
  module_source: &str,
  main: (&str, &str),
  specs: &[BuiltinSpec],
  prelude: &[(&str, &str)],
) -> Result<Package, String> {
  Package::from_modules_with_main(
    _compile_from_source(module_source, specs, prelude)?,
    main,
    specs,
  )
}

pub fn compile_from_source(
  module_source: &str,
  specs: &[BuiltinSpec],
  prelude: &[(&str, &str)],
) -> Result<Package, String> {
  Package::from_modules(_compile_from_source(module_source, specs, prelude)?, specs)
}

fn _compile_from_source(
  module_source: &str,
  specs: &[BuiltinSpec],
  prelude: &[(&str, &str)],
) -> Result<CompiledModules, String> {
  let asts = parser::read_multiple(module_source)?;
  let module_symbols = specs
    .iter()
    .filter(|spec| spec.module == "main")
    .map(|spec| spec.name)
    .collect::<Vec<_>>();
  let asts = resolve_module_names("main", &asts, prelude, &module_symbols)?;
  let checked =
    crate::typecheck::typecheck(asts, specs).map_err(|error| error.render(module_source))?;
  Ok(vec![compile_resolved_module("main", &checked)?])
}

/// Inject a `Callable::Builtin` entry for each `BuiltinSpec` into the named
/// module's function table, creating the module if it doesn't already exist
/// (e.g. the `std` module, which has no source file). Must run before
/// [`index_modules`] so the slots are resolvable by `Call` instructions.
fn inject_builtin_specs(
  mut modules: CompiledModules,
  specs: &[BuiltinSpec],
) -> Result<CompiledModules, String> {
  for spec in specs {
    let module = modules
      .iter_mut()
      .find(|module| module.name == spec.module)
      .map(|module| &mut module.functions);
    let module = match module {
      Some(funcs) => funcs,
      None => {
        modules.push(CompiledModule {
          name: spec.module.to_string(),
          functions: vec![],
          types: vec![],
        });
        &mut modules.last_mut().unwrap().functions
      }
    };
    if module.iter().any(|(name, _)| name == spec.name) {
      return Err(format!(
        "Builtin {}.{} collides with an existing function",
        spec.module, spec.name
      ));
    }
    module.push((spec.name.to_string(), Callable::Builtin));
  }
  Ok(modules)
}

#[cfg(test)]
mod compiler_tests;
