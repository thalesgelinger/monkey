use std::{
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::{
    ast::{BlockStatement, Identifier, Node},
    environment::Env,
};

#[derive(Debug, Clone)]
pub enum Object {
    Integer(isize),
    Boolean(bool),
    String(String),
    Array(Array),
    Error(String),
    Return(Box<Object>),
    Function(Function),
    Bultin(BultinFunction),
    Null,
}

#[derive(Debug, Clone)]
pub enum BultinFunction {
    Len,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Rc<Env>,
}

#[derive(Debug, Clone)]
pub struct Array {
    pub elements: Vec<Object>,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(value) => value.to_string(),
            Object::Boolean(value) => value.to_string(),
            Object::Null => "null".to_string(),
            Object::Return(value) => value.inspect(),
            Object::Error(msg) => format!("ERROR: {}", msg),
            Object::Function(function) => {
                let mut out = String::new();

                let mut params = vec![];
                for param in &function.parameters {
                    params.push(param.string());
                }

                out.push_str("fn");
                out.push_str("(");
                out.push_str(&params.join(", "));
                out.push_str(") {\n");
                out.push_str(&function.body.string());
                out.push_str("\n}");

                out
            }
            Object::String(string) => string.to_string(),
            Object::Bultin(_) => "builtin function".to_string(),
            Object::Array(arr) => {
                let mut out = String::from("");

                let mut elements: Vec<String> = vec![];
                for element in &arr.elements {
                    elements.push(element.inspect());
                }

                out.push_str("[");
                out.push_str(&elements.join(", "));
                out.push_str("]");

                out
            }
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(_) => write!(f, "INTEGER"),
            Object::Boolean(_) => write!(f, "BOOLEAN"),
            Object::Error(_) => write!(f, "ERROR"),
            Object::Return(_) => write!(f, "RETURN"),
            Object::Null => write!(f, "NULL"),
            Object::Function(_) => write!(f, "FUNCTION"),
            Object::String(_) => write!(f, "STRING"),
            Object::Bultin(_) => write!(f, "BULTIN"),
            Object::Array(_) => write!(f, "ARRAY"),
        }
    }
}
