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
    Function(String, Vec<String>, Expr),
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
                "let" => special_let(right, env),
                "fn" => special_fn(right, env),
                _ => {
                    let val = env.get(s).ok_or_else(|| format!("Unknown invocation: {}", s))?.clone();
                    if let SLVal::Function(_, _, ref body) = *val {
                        eval(body, env)
                    } else {
                        Err(format!("{} is not a function", s))
                    }
                }
            },
            _ => Err("callables must be symbols for now".to_string()),
        },
        _ => Err("Sorry unimplement".to_string()),
    }
}

fn flatten_list(mut cons: &AValue<String>) -> Result<Vec<AValue<String>>, String> {
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

fn special_fn(right: &Box<Expr>, env: &mut Env) -> Result<Rc<SLVal>, String> {
    match **right {
        AValue::Cons(ref name, ref box_cons_params_and_body) => match **name {
            AValue::Symbol(ref name) => match **box_cons_params_and_body {
                AValue::Cons(ref params, ref box_cons_body_and_nil) => {
                    match **box_cons_body_and_nil {
                        AValue::Cons(ref body, _) => {
                            let flattened_params = flatten_list(params)?;
                            let mut params_vec = vec![];
                            for param_expr in flattened_params {
                                if let AValue::Symbol(p) = param_expr {
                                    params_vec.push(p);
                                } else {
                                    return Err("Parameters must be symbols".to_string());
                                }
                            }
                            let func = SLVal::Function(name.clone(), params_vec, *body.clone());
                            env.insert(name.clone(), Rc::new(func));
                            return Ok(Rc::new(SLVal::Void));
                        }
                        _ => Err("bad `fn`".to_string()),
                    }
                }
                _ => Err("`fn` must take parameters after the name.".to_string()),
            },
            _ => Err("`fn` first argument must be a symbol.".to_string()),
        },
        _ => Err("`fn` must take parameters. And don't use a dot.".to_string()),
    }
}

// why is this box a reference
fn special_let(right: &Box<Expr>, env: &mut Env) -> Result<Rc<SLVal>, String> {
    // Would if_chain! help here? It would mean we would have less specific
    // error messages. I probably need a custom pattern-matcher.
    match **right {
        AValue::Cons(ref var, ref box_cons_expr) => match **var {
            AValue::Symbol(ref variable_name) => match **box_cons_expr {
                AValue::Cons(ref box_expr, ref box_hopeful_nil) => match **box_hopeful_nil {
                    AValue::Nil => {
                        let result = eval(box_expr, env)?;
                        env.insert(variable_name.clone(), result);
                        Ok(Rc::new(SLVal::Void))
                    }
                    _ => Err("`let` must have two arguments.".to_string()),
                },
                _ => Err("`let` must have two arguments. Don't use a dot.".to_string()),
            },
            _ => Err("first argument to `let` must be a symbol".to_string()),
        },
        _ => Err("`let` must have two arguments, a symbol and an expression.".to_string()),
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
        eval(&form("(let x 5)"), &mut env).unwrap();
        assert_eq!(env.get("x").unwrap(), &Rc::new(SLVal::Int(5)));
        assert_eq!(eval(&form("x"), &mut env).unwrap(), Rc::new(SLVal::Int(5)));
    }

    #[test]
    fn functions() {
        let mut env = HashMap::new();
        eval(&form("(fn hello-world () 5)"), &mut env).unwrap();
        println!("{:?}", env);
        assert_eq!(
            eval(&form("(hello-world)"), &mut env).unwrap(),
            Rc::new(SLVal::Int(5))
        );
    }
}
