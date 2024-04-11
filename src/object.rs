use std::{fmt::Debug, usize};

#[derive(Debug)]
pub enum ObjectType {
    Integer,
    Boolean,
    Null,
}

pub trait Object {
    fn t(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

impl Debug for dyn Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Type: {:?}, Value: {:?}", self.t(), self.inspect())
    }
}

#[derive(Debug)]
pub struct Integer {
    pub value: usize,
}

impl Object for Integer {
    fn t(&self) -> ObjectType {
        ObjectType::Integer
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Debug)]
pub struct Boolean {
    value: bool,
}

impl Object for Boolean {
    fn t(&self) -> ObjectType {
        ObjectType::Boolean
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Debug)]
pub struct Null;

impl Object for Null {
    fn t(&self) -> ObjectType {
        ObjectType::Null
    }

    fn inspect(&self) -> String {
        "null".to_string()
    }
}
