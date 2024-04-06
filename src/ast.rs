use crate::token::Token;
use core::fmt::Debug;
use std::{any::Any, fmt::Display};

pub trait AnyNode: 'static {
    fn as_any(&self) -> &dyn Any;
}

impl<T: 'static> AnyNode for T {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub trait Node: AnyNode {
    fn token_literal(&self) -> String;
    fn string(&self) -> String;
}

pub trait Statement: Node {
    fn statement_node(&self);
}

pub trait Expression: Node {
    fn expression_node(&self);
}

impl Debug for dyn Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.token_literal())
    }
}

impl Display for dyn Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Program {
    pub fn string(&self) -> String {
        let mut out = String::new();

        for statement in &self.statements {
            out.push_str(&statement.string())
        }

        out
    }
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

pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Box<dyn Expression>>,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        match &self.token {
            Token::Return => "return".to_string(),
            _ => panic!("Return statement invalid"),
        }
    }

    fn string(&self) -> String {
        let mut out = String::new();

        out.push_str(&self.token_literal());
        out.push_str(" ");

        if let Some(expression) = &self.return_value {
            out.push_str(&expression.string());
        }

        out.push_str(";");

        out
    }
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        match &self.token {
            Token::Let => "let".to_string(),
            _ => panic!("Let statement invalid"),
        }
    }

    fn string(&self) -> String {
        let mut out = String::new();

        out.push_str(&self.token_literal());
        out.push_str(" ");
        out.push_str(&self.name.string());
        out.push_str(" = ");

        if let Some(expression) = &self.value {
            out.push_str(&expression.string());
        }

        out.push_str(";");

        out
    }
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }

    fn string(&self) -> String {
        match &self.expression {
            Some(expression) => expression.to_string(),
            None => panic!("Fail stringifying expression"),
        }
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) {}
}

impl Statement for ReturnStatement {
    fn statement_node(&self) {}
}

impl Statement for ExpressionStatement {
    fn statement_node(&self) {}
}

pub struct Identifier {
    pub token: Token,
}

impl Debug for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.token_literal())
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        match &self.token {
            Token::Ident(ident) => ident.to_string(),
            _ => panic!("This token should be an Ident"),
        }
    }

    fn string(&self) -> String {
        match &self.token {
            Token::Ident(ident) => ident.to_string(),
            _ => panic!("This token should be an Ident"),
        }
    }
}

impl Expression for Identifier {
    fn expression_node(&self) {}
}

pub struct IntegerLiteral {
    pub token: Token,
}

impl Debug for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.token_literal())
    }
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }

    fn string(&self) -> String {
        format!("{:?}", self.token)
    }
}

impl Expression for IntegerLiteral {
    fn expression_node(&self) {}
}

pub struct PrefixExpression {
    pub token: Token,
    pub right: Option<Box<dyn Expression>>,
    pub operator: Token,
}

impl Debug for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.token_literal())
    }
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }

    fn string(&self) -> String {
        if let Some(right) = &self.right {
            return format!("({:?}, {:?})", self.operator, right.string());
        };
        format!("({:?})", self.operator)
    }
}

impl Expression for PrefixExpression {
    fn expression_node(&self) {}
}

pub struct InfixExpression {
    pub token: Token,
    pub right: Option<Box<dyn Expression>>,
    pub operator: Token,
    pub left: Option<Box<dyn Expression>>,
}

impl Debug for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.token_literal())
    }
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }

    fn string(&self) -> String {
        let right = &self.right.as_ref().expect("To have a right");
        let left = &self.left.as_ref().expect("To have a left");
        format!(
            "({:?}, {:?}, {:?})",
            left.to_string(),
            self.operator,
            right.string(),
        )
    }
}

impl Expression for InfixExpression {
    fn expression_node(&self) {}
}

#[cfg(test)]
mod ast_tests {

    use super::{Identifier, LetStatement, Program};
    use crate::token::Token;

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![Box::new(LetStatement {
                token: Token::Let,
                name: Identifier {
                    token: Token::Ident("myVar".to_string()),
                },
                value: Some(Box::new(Identifier {
                    token: Token::Ident("anotherVar".to_string()),
                })),
            })],
        };

        assert_eq!(program.string(), "let myVar = anotherVar;");
    }
}
