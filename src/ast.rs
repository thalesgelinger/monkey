use crate::token::Token;

pub trait Node {
    fn token_literal(&self) -> String;
}

pub trait Statement: Node {
    fn statement_node(&self);
}

pub trait Expression: Node {
    fn expression_node(&self);
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Option<Box<dyn Expression>>,
}

pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Box<dyn Expression>>,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        match &self.token {
            Token::Return => "return".to_string(),
            _ => panic!("Return statement invalid"),
        }
    }
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        match &self.name.token {
            Token::Ident(v) => v.to_string(),
            _ => panic!("Let statement invalid"),
        }
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) {}
}

impl Statement for ReturnStatement {
    fn statement_node(&self) {}
}

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        todo!("Implemente for Identifier")
    }
}

impl Expression for Identifier {
    fn expression_node(&self) {}
}
