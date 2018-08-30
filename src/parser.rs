use atoms::{ParseError, Parser, Value as AValue};

type Expr = AValue<String>;

#[derive(Debug, PartialEq, Clone)]
pub enum AST {
  Let(String, Box<AST>),
  DeclareFn(FunctionDecl),
  DefineFn(Function),
  Call(Box<AST>, Vec<AST>),
  Variable(String),
  Int(i64),
  Float(f64),
  String(String),
}

/// A FunctionDecl is a signature-only declaration of a function. A program that
/// provides builtins for executing a SafeLisp program must provide function
/// declarations to describe the signatures of those functions, which will be
/// used during compilation.
#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDecl {
  pub name: String,
  /// TODO: params should be Vec<Type>, not Vec<String>. We don't need parameter
  /// names here.
  pub params: Vec<String>,
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
      AValue::Int(i) => Ok(AST::Int(i.clone())),
      AValue::Float(f) => Ok(AST::Float(f.clone())),
      AValue::Symbol(s) => Ok(AST::Variable(s.clone())),
      AValue::Cons(left, right) => {
        match **left {
          AValue::Symbol(ref s) => match s.as_ref() {
            "let" => return parse_let(right),
            "fn" => return parse_fn(right),
            "decl" => return parse_decl(right),
            _ => {}
          },
          _ => {}
        }
        let function_ast = AST::from_atoms(left)?;
        let args: Result<Vec<AST>, String> = flatten_list(right)?
          .iter()
          .map(|form| AST::from_atoms(form))
          .collect();
        let args = args?;
        Ok(AST::Call(Box::new(function_ast), args))
      }
      _ => Err(format!("Sorry, didn't implement {:?} yet", form)),
    }
  }
}

fn parse_let(right: &Box<Expr>) -> Result<AST, String> {
  match **right {
    AValue::Cons(ref var, ref box_cons_expr) => match **var {
      AValue::Symbol(ref variable_name) => match **box_cons_expr {
        AValue::Cons(ref box_expr, ref box_hopeful_nil) => match **box_hopeful_nil {
          AValue::Nil => Ok(AST::Let(
            variable_name.clone(),
            Box::new(AST::from_atoms(box_expr)?),
          )),
          _ => Err("`let` must have two arguments.".to_string()),
        },
        _ => Err("`let` must have two arguments. Don't use a dot.".to_string()),
      },
      _ => Err("first argument to `let` must be a symbol".to_string()),
    },
    _ => Err("`let` must have two arguments, a symbol and an expression.".to_string()),
  }
}

fn parse_decl(right: &Box<Expr>) -> Result<AST, String> {
  match **right {
    AValue::Cons(ref name, ref box_cons_params) => match **name {
      AValue::Symbol(ref name) => match **box_cons_params {
        AValue::Cons(ref params, box_nil) => {
          let flattened_params = flatten_list(params)?;
          let mut params_vec = vec![];
          for param_expr in flattened_params {
            if let AValue::Symbol(p) = param_expr {
              params_vec.push(p);
            } else {
              return Err("Parameters must be symbols".to_string());
            }
          }
          Ok(AST::DeclareFn(FunctionDecl {
            name: name.clone(),
            params: params_vec,
          }))
        }
        _ => Err("`fn` must take parameters after the name.".to_string()),
      },
      _ => Err("`fn` first argument must be a symbol.".to_string()),
    },
    _ => Err("`fn` must take parameters. And don't use a dot.".to_string()),
  }
}

fn parse_fn(right: &Box<Expr>) -> Result<AST, String> {
  match **right {
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
}
