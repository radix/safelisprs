use atoms::{ParseError, Parser, Value as AValue};

type Expr = AValue<String>;

#[derive(Debug, PartialEq, Clone)]
pub enum AST {
  Let(String, Box<AST>),
  DefineFn(Function),
  Call(Box<AST>, Vec<AST>),
  CallFixed(Identifier, Vec<AST>),
  Variable(String),
  Int(i64),
  Float(f64),
  String(String),
  Bool(bool),

  // The following variants aren't represented in the syntax, but are produced
  // by transformations on the previous variants.
  /// A Cell wraps a value in a box. This is used to provide closures with
  /// access to values in outer variables.
  Cell(Box<AST>),
  /// And we can deref these cells to get their inner value.
  DerefCell(Box<AST>),
  /// Set the contents of a Cell. The first expression must evaluate to a
  /// Cell; the second is the new value to store in it. Evaluates to the new
  /// value.
  SetCell(Box<AST>, Box<AST>),
  /// Bind up some arguments with a callable. (this is used for passing cells to closures!)
  PartialApply(Box<AST>, Vec<AST>),
  /// Get a reference to a function.
  FunctionRef(String, String),
  /// Conditional: evaluate `cond`; if truthy, evaluate `then`, else evaluate `els`.
  If(Box<AST>, Box<AST>, Box<AST>),
  /// A sequence: evaluate each sub-expression in order, discarding all but the
  /// last, and return the last. Lets a single-expression position (e.g. an `if`
  /// branch) evaluate multiple expressions for side effects.
  Block(Vec<AST>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Identifier {
  Bare(String),
  Qualified(String, String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
  pub name: String,
  pub params: Vec<String>,
  pub code: Vec<AST>,
}

pub fn read_multiple(s: &str) -> Result<Vec<AST>, String> {
  let mut p = Parser::new(&s);
  let mut result = vec![];
  loop {
    let form = p.read();
    match form {
      Ok(form) => result.push(AST::from_atoms(&form)?),
      Err(ParseError::EndOfFile(_, _)) => break,
      Err(e) => return Err(format!("{:?}", e)),
    }
  }
  Ok(result)
}

impl AST {
  pub fn from_atoms(form: &Expr) -> Result<Self, String> {
    match form {
      AValue::Str(s) => Ok(AST::String(s.clone())),
      AValue::Int(i) => Ok(AST::Int(*i)),
      AValue::Float(f) => Ok(AST::Float(*f)),
      AValue::Symbol(s) => match s.as_ref() {
        "true" => Ok(AST::Bool(true)),
        "false" => Ok(AST::Bool(false)),
        _ => match parse_identifier(s) {
          Identifier::Bare(name) => Ok(AST::Variable(name)),
          Identifier::Qualified(module, name) => Ok(AST::FunctionRef(module, name)),
        },
      },
      AValue::Cons(left, right) => {
        if let AValue::Symbol(ref s) = **left {
          match s.as_ref() {
            "let" => return parse_let(right),
            "fn" => return parse_fn(right),
            "if" => return parse_if(right),
            "block" => return parse_block(right),
            _ => {}
          }
        }
        parse_call(form)
      }
      _ => Err(format!("Sorry, didn't implement {:?} yet", form)),
    }
  }
}

fn parse_if(right: &Expr) -> Result<AST, String> {
  let forms = flatten_list(right)?;
  if forms.len() != 3 {
    return Err("`if` must have exactly three arguments: cond, then, else".to_string());
  }
  Ok(AST::If(
    Box::new(AST::from_atoms(&forms[0])?),
    Box::new(AST::from_atoms(&forms[1])?),
    Box::new(AST::from_atoms(&forms[2])?),
  ))
}

fn parse_block(right: &Expr) -> Result<AST, String> {
  let forms = flatten_list(right)?;
  if forms.is_empty() {
    return Err("`block` must have at least one expression".to_string());
  }
  let body: Result<Vec<AST>, _> = forms.iter().map(AST::from_atoms).collect();
  Ok(AST::Block(body?))
}

fn parse_call(form: &Expr) -> Result<AST, String> {
  let form = flatten_list(form)?;
  if form.is_empty() {
    return Err("Empty call".to_string());
  }
  let args: Result<Vec<AST>, _> = form[1..].iter().map(AST::from_atoms).collect();
  if let AValue::Symbol(ref name) = form[0] {
    Ok(AST::CallFixed(parse_identifier(name), args?))
  } else {
    Ok(AST::Call(Box::new(AST::from_atoms(&form[0])?), args?))
  }
}

fn parse_identifier(name: &str) -> Identifier {
  let parts: Vec<&str> = name.splitn(2, '.').collect();
  if parts.len() == 1 {
    Identifier::Bare(name.to_string())
  } else {
    Identifier::Qualified(parts[0].to_string(), parts[1].to_string())
  }
}

fn parse_let(right: &Expr) -> Result<AST, String> {
  match right {
    AValue::Cons(ref var, ref box_cons_expr) => match **var {
      AValue::Symbol(ref variable_name) => match **box_cons_expr {
        AValue::Cons(ref box_expr, ref box_hopeful_nil) => match **box_hopeful_nil {
          AValue::Nil => Ok(AST::Let(
            variable_name.clone(),
            Box::new(AST::from_atoms(box_expr)?),
          )),
          _ => Err("`let` must have two arguments.".to_string()),
        },
        _ => Err("`let` must have two arguments. Don't use a cons-dot.".to_string()),
      },
      _ => Err("first argument to `let` must be a symbol".to_string()),
    },
    _ => Err("`let` must have two arguments, a symbol and an expression.".to_string()),
  }
}

fn parse_fn(right: &Expr) -> Result<AST, String> {
  match *right {
    AValue::Cons(ref name, ref box_cons_params_and_body) => match **name {
      AValue::Symbol(ref name) => match **box_cons_params_and_body {
        AValue::Cons(ref params, ref box_body) => {
          let flattened_params = flatten_list(params)?;
          let mut params_vec = vec![];
          for param_expr in flattened_params {
            if let AValue::Symbol(p) = param_expr {
              params_vec.push(p);
            } else {
              return Err("Parameters must be symbols".to_string());
            }
          }
          let body_forms = flatten_list(&*box_body)?;
          let body_asts: Result<Vec<AST>, _> = body_forms.iter().map(AST::from_atoms).collect();
          Ok(AST::DefineFn(Function {
            name: name.clone(),
            params: params_vec,
            code: body_asts?,
          }))
        }
        _ => Err("`fn` must take parameters after the name.".to_string()),
      },
      _ => Err("`fn` first argument must be a symbol.".to_string()),
    },
    _ => Err("`fn` must take parameters. And don't use a dot.".to_string()),
  }
}

fn flatten_list(mut cons: &AValue<String>) -> Result<Vec<Expr>, String> {
  let mut result = vec![];
  loop {
    match cons {
      AValue::Cons(l, r) => {
        result.push(*l.clone());
        cons = &**r;
      }
      AValue::Nil => return Ok(result),
      _ => return Err("Attempted to flatten a non-cons".to_string()),
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_read_multiple() {
    let result = read_multiple("5 3").unwrap();
    assert_eq!(result, vec![AST::Int(5), AST::Int(3)]);
  }

  #[test]
  fn dotted_symbol_in_value_position_is_a_function_ref() {
    let result = read_multiple("std.len").unwrap();
    assert_eq!(
      result,
      vec![AST::FunctionRef("std".to_string(), "len".to_string())]
    );
  }

  #[test]
  fn set_is_parsed_as_an_ordinary_call() {
    let result = read_multiple("(set! x 2)").unwrap();
    assert_eq!(
      result,
      vec![AST::CallFixed(
        Identifier::Bare("set!".to_string()),
        vec![AST::Variable("x".to_string()), AST::Int(2)],
      )]
    );
  }
}
