extern crate atoms;
use atoms::Value as AValue;

use std::collections::HashMap;
use std::rc::Rc;

type Expr = AValue<String>;
type Env = HashMap<String, Rc<SLVal>>;

#[derive(Debug, PartialEq)]
pub enum SLVal {
    Int(i64),
    Float(f64),
    String(String),
    Symbol(String),
    List(Vec<SLVal>),
}

pub fn eval(form: Expr, env: &mut Env) -> Result<Rc<SLVal>, String> {
    match form {
        AValue::Str(s) => Ok(Rc::new(SLVal::String(s))),
        AValue::Int(i) => Ok(Rc::new(SLVal::Int(i))),
        AValue::Float(f) => Ok(Rc::new(SLVal::Float(f))),
        AValue::Symbol(s) =>
            Ok(env.get(&s).ok_or_else(|| "Variable not found".to_string())?.clone()),
        _ => Err("Sorry unimplement".to_string()),
    }
}

#[cfg(test)]
mod test {

    use atoms::Parser;
    use super::*;

    fn form(inp: &str) -> Expr {
        Parser::new(&inp).parse_basic().unwrap()
    }

    #[test]
    fn atoms() {
        let mut empty = HashMap::new();

        let form = form("\"foo\"");
        assert_eq!(
            eval(form, &mut empty).unwrap(),
            Rc::new(SLVal::String("foo".to_string()))
        );
        let form = Parser::new(&"64").parse_basic().unwrap();
        assert_eq!(eval(form, &mut empty).unwrap(), Rc::new(SLVal::Int(64)));
        let form = Parser::new(&"64.0").parse_basic().unwrap();
        assert_eq!(eval(form, &mut empty).unwrap(), Rc::new(SLVal::Float(64.0)));
    }

    #[test]
    fn variable() {
        let mut env = HashMap::new();
        env.insert("myvar".to_string(), Rc::new(SLVal::String("my value".to_string())));
        let form = form("myvar");
        assert_eq!(eval(form, &mut env).unwrap(), Rc::new(SLVal::String("my value".to_string())));
    }
}
