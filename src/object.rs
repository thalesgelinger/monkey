use std::fmt::{Debug, Display};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(isize),
    Boolean(bool),
    Error(String),
    Return(Box<Object>),
    Null,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(value) => value.to_string(),
            Object::Boolean(value) => value.to_string(),
            Object::Null => "null".to_string(),
            Object::Return(value) => value.inspect(),
            Object::Error(msg) => format!("ERROR: {}", msg),
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
        }
    }
}
