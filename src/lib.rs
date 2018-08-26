extern crate atoms;
use atoms::Value as AValue;

type Expr = AValue<String>;

#[derive(Debug, PartialEq)]
pub enum SLVal {
    Int(i64),
    Float(f64),
    String(String),
    Symbol(String),
    List(Vec<SLVal>),
}

pub fn eval(form: Expr) -> Result<SLVal, String> {
    match form {
        AValue::Str(s) => Ok(SLVal::String(s)),
        // AValue::Symbol(s) => Ok(SLVal::Symbol(s)),
        AValue::Int(i) => Ok(SLVal::Int(i)),
        AValue::Float(f) => Ok(SLVal::Float(f)),
        _ => Err("Sorry unimplement".to_string()),
    }
}


#[test]
fn atoms() {
    use atoms::Parser;
    let form = Parser::new(&"\"foo\"").parse_basic().unwrap();
    assert_eq!(eval(form).unwrap(), SLVal::String("foo".to_string()));
    let form = Parser::new(&"64").parse_basic().unwrap();
    assert_eq!(eval(form).unwrap(), SLVal::Int(64));
    let form = Parser::new(&"64.0").parse_basic().unwrap();
    assert_eq!(eval(form).unwrap(), SLVal::Float(64.0));
}
