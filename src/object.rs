use std::fmt::Debug;

#[derive(Debug)]
pub enum Object {
    Integer(isize),
    Boolean(bool),
    Null,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(value) => value.to_string(),
            Object::Boolean(value) => value.to_string(),
            Object::Null => "null".to_string(),
        }
    }
}
