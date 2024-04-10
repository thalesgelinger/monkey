use std::usize;

pub enum ObjectType {
    Integer,
    Boolean,
    Null,
}

trait Object {
    // Object type
    fn t() -> ObjectType;
    fn inspect() -> String;
}

#[derive(Debug)]
struct Integer {
    value: usize,
}

impl Object for Integer {
    fn t() -> ObjectType {
        ObjectType::Integer
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Debug)]
struct Boolean {
    value: bool,
}

impl Object for Boolean {
    fn t() -> ObjectType {
        ObjectType::Boolean
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Debug)]
struct Null {}

impl Object for Null {
    fn t() -> ObjectType {
        ObjectType::Null
    }

    fn inspect(&self) -> String {
        "null".to_string()
    }
}
