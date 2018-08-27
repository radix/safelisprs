extern crate atoms;

use std::collections::HashMap;
use std::rc::Rc; // TODO: use Manishearth/rust-gc

mod parser;
use parser::AST;

#[derive(Debug)]
pub struct Env {
    frames: Vec<Frame>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            frames: vec![Frame::new()],
        }
    }

    pub fn get(&self, name: &str) -> Option<Rc<SLVal>> {
        for frame in self.frames.iter().rev() {
            let v = frame.bindings.get(name).map(|x| x.clone());
            if v.is_some() {
                return v;
            }
        }
        return None;
    }

    pub fn set(&mut self, name: String, val: Rc<SLVal>) {
        self.frames
            .last_mut()
            .expect("Invariant failed: No frame found!")
            .bindings
            .insert(name, val);
    }

    fn push(&mut self) {
        self.frames.push(Frame::new())
    }

    fn pop(&mut self) -> Result<Frame, String> {
        if self.frames.len() == 1 {
            Err("Can't pop last frame".to_string())
        } else {
            Ok(self.frames
                .pop()
                .expect("Invariant failed: no last frame found"))
        }
    }
}

#[derive(Debug)]
struct Frame {
    bindings: HashMap<String, Rc<SLVal>>,
}

impl Frame {
    fn new() -> Self {
        Frame {
            bindings: HashMap::new(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum SLVal {
    Int(i64),
    Float(f64),
    String(String),
    Symbol(String),
    List(Vec<SLVal>),
    Void,
    Function(Function),
}

#[derive(Debug, PartialEq)]
pub struct Function {
    name: String,
    params: Vec<String>,
    body: AST,
}

pub fn eval(form: &AST, env: &mut Env) -> Result<Rc<SLVal>, String> {
    match form {
        AST::String(s) => Ok(Rc::new(SLVal::String(s.clone()))),
        AST::Int(i) => Ok(Rc::new(SLVal::Int(i.clone()))),
        AST::Float(f) => Ok(Rc::new(SLVal::Float(f.clone()))),
        AST::Variable(s) => Ok(env.get(s)
            .ok_or_else(|| "Variable not found".to_string())?
            .clone()),
        AST::Let(name, expr) => special_let(name, expr, env),
        AST::DefineFn(name, params, body) => special_fn(name, params, body, env),
        AST::Call(func, args) => call_fn(func, args, env),
    }
}

fn call_fn(func: &AST, args: &[AST], env: &mut Env) -> Result<Rc<SLVal>, String> {
    let func = eval(func, env)?;
    if let SLVal::Function(ref func) = *func {
        if args.len() != func.params.len() {
            return Err(format!(
                "{} requires {} arguments, found {}",
                func.name,
                func.params.len(),
                args.len()
            ));
        }
        let evaled: Vec<_> = args.iter().map(|a| eval(a, env)).collect();
        // 3. bind arguments to parameters
        env.push();
        for (name, arg) in func.params.iter().zip(evaled) {
            env.set(name.to_string(), arg?);
        }
        let result = eval(&func.body, env);
        env.pop()?;
        result
    } else {
        Err(format!("Tried to call a non-function: {:?}", func))
    }
}

fn special_fn(
    name: &str,
    params: &[String],
    body: &AST,
    env: &mut Env,
) -> Result<Rc<SLVal>, String> {
    let func = SLVal::Function(Function {
        name: name.to_string(),
        params: params.to_vec(),
        body: body.clone(),
    });
    env.set(name.to_string(), Rc::new(func));
    return Ok(Rc::new(SLVal::Void));
}

// why is this box a reference
fn special_let(name: &str, expr: &AST, env: &mut Env) -> Result<Rc<SLVal>, String> {
    let result = eval(expr, env)?;
    env.set(name.to_string(), result);
    Ok(Rc::new(SLVal::Void))
}

#[cfg(test)]
mod test {

    use super::*;
    use atoms::Parser;

    fn form(inp: &str) -> AST {
        AST::from_atoms(&Parser::new(&inp).parse_basic().unwrap()).unwrap()
    }

    #[test]
    fn atoms() {
        let mut empty = Env::new();

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
        let mut env = Env::new();
        env.set(
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
        let mut env = Env::new();
        eval(&form("(let x 5)"), &mut env).unwrap();
        assert_eq!(env.get("x").unwrap(), Rc::new(SLVal::Int(5)));
        assert_eq!(eval(&form("x"), &mut env).unwrap(), Rc::new(SLVal::Int(5)));
    }

    #[test]
    fn function_const() {
        let mut env = Env::new();
        eval(&form("(fn hello-world () 5)"), &mut env).unwrap();
        println!("{:?}", env);
        assert_eq!(
            eval(&form("(hello-world)"), &mut env).unwrap(),
            Rc::new(SLVal::Int(5))
        );
    }

    #[test]
    fn function_param() {
        let mut env = Env::new();
        eval(&form("(fn id (a) a)"), &mut env).unwrap();
        assert_eq!(
            eval(&form("(id 5)"), &mut env).unwrap(),
            Rc::new(SLVal::Int(5))
        );
    }
}
