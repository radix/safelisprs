extern crate atoms;
use atoms::Value as AValue;

use std::collections::HashMap;
use std::rc::Rc; // TODO: use Manishearth/rust-gc

type Expr = AValue<String>;
type Env = HashMap<String, Rc<SLVal>>;

#[derive(Debug, PartialEq)]
pub enum SLVal {
    Int(i64),
    Float(f64),
    String(String),
    Symbol(String),
    List(Vec<SLVal>),
    Void,
}

// pub enum AST {
//     Let(Variable, AST),
//     Call(Expr, Vec<Expr>),
//     Variable(String),
//     Int, Float, String, List,
// }

pub fn eval(form: &Expr, env: &mut Env) -> Result<Rc<SLVal>, String> {
    match form {
        AValue::Str(s) => Ok(Rc::new(SLVal::String(s.clone()))),
        AValue::Int(i) => Ok(Rc::new(SLVal::Int(i.clone()))),
        AValue::Float(f) => Ok(Rc::new(SLVal::Float(f.clone()))),
        AValue::Symbol(s) => Ok(env
            .get(s)
            .ok_or_else(|| "Variable not found".to_string())?
            .clone()),
        AValue::Cons(left, right) => match **left {
            AValue::Symbol(ref s) => match s.as_ref() {
                "let" => match **right {
                    AValue::Cons(ref var, ref box_cons_expr) => match **var {
                        AValue::Symbol(ref variable_name) => match **box_cons_expr {
                            AValue::Cons(ref box_expr, ref box_hopeful_nil) => {
                                match **box_hopeful_nil {
                                    AValue::Nil => {
                                        let result = eval(box_expr, env)?;
                                        env.insert(variable_name.clone(), result);
                                        Ok(Rc::new(SLVal::Void))
                                    }
                                    _ => Err("`let` must have two arguments.".to_string()),
                                }
                            }
                            _ => Err("`let` must have two arguments. Don't use a dot.".to_string()),
                        },
                        _ => Err("first argument to `let` must be a symbol".to_string()),
                    },
                    _ => Err(
                        "`let` must have two arguments, a symbol and an expression.".to_string()
                    ),
                },
                _ => Err("Unknown special form".to_string()),
            },
            _ => Err("callables must be symbols for now".to_string()),
        },
        _ => Err("Sorry unimplement".to_string()),
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use atoms::Parser;

    fn form(inp: &str) -> Expr {
        Parser::new(&inp).parse_basic().unwrap()
    }

    #[test]
    fn atoms() {
        let mut empty = HashMap::new();

        assert_eq!(
            eval(&form("\"foo\""), &mut empty).unwrap(),
            Rc::new(SLVal::String("foo".to_string()))
        );
        assert_eq!(
            eval(&form("64"), &mut empty).unwrap(),
            Rc::new(SLVal::Int(64))
        );
        assert_eq!(
            eval(&form("64.0"), &mut empty).unwrap(),
            Rc::new(SLVal::Float(64.0))
        );
    }

    #[test]
    fn variable() {
        let mut env = HashMap::new();
        env.insert(
            "myvar".to_string(),
            Rc::new(SLVal::String("my value".to_string())),
        );
        assert_eq!(
            eval(&form("myvar"), &mut env).unwrap(),
            Rc::new(SLVal::String("my value".to_string()))
        );
    }

    #[test]
    fn let_var() {
        let mut env = HashMap::new();
        env.insert(
            "myvar".to_string(),
            Rc::new(SLVal::String("my value".to_string())),
        );
        eval(&form("(let x 5)"), &mut env).unwrap();
        assert_eq!(env.get("x").unwrap(), &Rc::new(SLVal::Int(5)));
        assert_eq!(eval(&form("x"), &mut env).unwrap(), Rc::new(SLVal::Int(5)));
    }
}
