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

pub trait StatementClone {
    fn clone_statement(&self) -> Box<dyn Statement>;
}

impl<T> StatementClone for T
where
    T: 'static + Statement + Clone,
{
    fn clone_statement(&self) -> Box<dyn Statement> {
        Box::new(self.clone())
    }
}

pub trait Statement: Node + StatementClone {
    fn statement_node(&self) -> &Token;
}

impl Clone for Box<dyn Statement> {
    fn clone(&self) -> Self {
        self.clone_statement()
    }
}

impl Debug for dyn Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.token_literal())
    }
}

pub trait ExpressionClone {
    fn clone_expression(&self) -> Box<dyn Expression>;
}

impl<T> ExpressionClone for T
where
    T: 'static + Expression + Clone,
{
    fn clone_expression(&self) -> Box<dyn Expression> {
        Box::new(self.clone())
    }
}

pub trait Expression: Node + ExpressionClone {
    fn expression_node(&self);
}

impl Clone for Box<dyn Expression> {
    fn clone(&self) -> Self {
        self.clone_expression()
    }
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

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Program {
    #[warn(dead_code)]
    pub fn string(&self) -> String {
        let mut out = String::new();

        for statement in &self.statements {
            out.push_str(&statement.string())
        }

        out
    }
}

impl Node for Program {
    fn token_literal(&self) -> String {
        "".to_string()
    }

    fn string(&self) -> String {
        let mut out = String::new();

        for statement in &self.statements {
            out.push_str(&statement.string())
        }

        out
    }
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Option<Box<dyn Expression>>,
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Box<dyn Expression>>,
}

#[derive(Debug, Clone)]
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
            Some(expression) => expression.string(),
            None => "".to_string(),
        }
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) -> &Token {
        &self.token
    }
}

impl Statement for ReturnStatement {
    fn statement_node(&self) -> &Token {
        &self.token
    }
}

impl Statement for ExpressionStatement {
    fn statement_node(&self) -> &Token {
        &self.token
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Identifier {
    pub token: Token,
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

#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub token: Token,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }

    fn string(&self) -> String {
        let int_string = match &self.token {
            Token::Int(value) => value.to_string(),
            _ => panic!("Fail stringifying expression"),
        };

        int_string
    }
}

impl Expression for IntegerLiteral {
    fn expression_node(&self) {}
}

#[derive(Debug, Clone)]
pub struct PrefixExpression {
    pub token: Token,
    pub right: Option<Box<dyn Expression>>,
    pub operator: Token,
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }

    fn string(&self) -> String {
        if let Some(right) = &self.right {
            return format!("({}{})", self.operator.string(), right.string());
        };
        format!("({:?})", self.operator)
    }
}

impl Expression for PrefixExpression {
    fn expression_node(&self) {}
}

#[derive(Debug, Clone)]
pub struct InfixExpression {
    pub token: Token,
    pub right: Option<Box<dyn Expression>>,
    pub operator: Token,
    pub left: Option<Box<dyn Expression>>,
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }

    fn string(&self) -> String {
        let right = &self.right.as_ref().expect("To have a right");
        let left = &self.left.as_ref().expect("To have a left");
        format!(
            "({} {} {})",
            left.string(),
            self.operator.string(),
            right.string(),
        )
    }
}

impl Expression for InfixExpression {
    fn expression_node(&self) {}
}

#[derive(Debug, Clone)]
pub struct Boolean {
    pub token: Token,
}

impl Node for Boolean {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }

    fn string(&self) -> String {
        let bool_string = match &self.token {
            Token::True => true.to_string(),
            Token::False => false.to_string(),
            _ => panic!("Fail stringifying expression"),
        };

        bool_string
    }
}

impl Expression for Boolean {
    fn expression_node(&self) {}
}

#[derive(Debug, Clone)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<dyn Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }

    fn string(&self) -> String {
        let mut out = String::new();

        out.push_str("if");
        out.push_str(&self.condition.string());
        out.push_str(" ");
        out.push_str(&self.consequence.string());

        if let Some(alternative) = &self.alternative {
            out.push_str("else ");
            out.push_str(&alternative.string());
        }

        out
    }
}

impl Expression for IfExpression {
    fn expression_node(&self) {}
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Box<dyn Statement>>,
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }

    fn string(&self) -> String {
        let mut out = String::new();

        for stmt in &self.statements {
            out.push_str(&stmt.string());
        }

        out
    }
}

impl Expression for BlockStatement {
    fn expression_node(&self) {}
}

#[derive(Debug, Clone)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }

    fn string(&self) -> String {
        let mut out = String::new();

        let mut params: Vec<String> = vec![];

        for param in &self.parameters {
            params.push(param.string());
        }

        out.push_str(&self.token_literal());
        out.push_str("(");
        out.push_str(&params.join(","));
        out.push_str(") ");
        out.push_str(&self.body.string());

        out
    }
}

impl Expression for FunctionLiteral {
    fn expression_node(&self) {}
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<dyn Expression>,
    pub arguments: Vec<Box<dyn Expression>>,
}

impl Node for CallExpression {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }

    fn string(&self) -> String {
        let mut out = String::new();

        let mut args: Vec<String> = vec![];

        for arg in &self.arguments {
            args.push(arg.string());
        }

        out.push_str(&self.function.string());
        out.push_str("(");
        out.push_str(&args.join(", "));
        out.push_str(")");

        out
    }
}

impl Expression for CallExpression {
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
