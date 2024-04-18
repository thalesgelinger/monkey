use crate::token::Token;
use core::fmt::Debug;

pub trait Node {
    fn token_literal(&self) -> String;
    fn string(&self) -> String;
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::Let(let_stmt) => let_stmt.token_literal(),
            Statement::Return(return_stmt) => return_stmt.token_literal(),
            Statement::Expression(exp) => exp.token_literal(),
        }
    }

    fn string(&self) -> String {
        match self {
            Statement::Let(let_stmt) => let_stmt.string(),
            Statement::Return(return_stmt) => return_stmt.string(),
            Statement::Expression(exp) => exp.string(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(Identifier),
    Int(IntegerLiteral),
    String(StringLiteral),
    Array(ArrayLiteral),
    Index(IndexExpression),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Boolean(Boolean),
    If(IfExpression),
    Function(FunctionLiteral),
    Call(CallExpression),
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::Identifier(ident) => ident.token_literal(),
            Expression::Int(integer) => integer.token_literal(),
            Expression::Prefix(prefix) => prefix.token_literal(),
            Expression::Infix(infix) => infix.token_literal(),
            Expression::Boolean(boolean) => boolean.token_literal(),
            Expression::If(if_stmt) => if_stmt.token_literal(),
            Expression::Function(function) => function.token_literal(),
            Expression::Call(call) => call.token_literal(),
            Expression::String(string) => string.token_literal(),
            Expression::Array(array) => array.token_literal(),
            Expression::Index(index_exp) => index_exp.token_literal(),
        }
    }

    fn string(&self) -> String {
        match self {
            Expression::Identifier(ident) => ident.string(),
            Expression::Int(integer) => integer.string(),
            Expression::Prefix(prefix) => prefix.string(),
            Expression::Infix(infix) => infix.string(),
            Expression::Boolean(boolean) => boolean.string(),
            Expression::If(if_stmt) => if_stmt.string(),
            Expression::Function(function) => function.string(),
            Expression::Call(call) => call.string(),
            Expression::String(string) => string.string(),
            Expression::Array(array) => array.string(),
            Expression::Index(index_exp) => index_exp.string(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
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
    pub value: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Expression>,
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

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub token: Token,
}

impl Node for StringLiteral {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }

    fn string(&self) -> String {
        match &self.token {
            Token::String(value) => value.to_string(),
            _ => panic!("Fail stringifying expression"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ArrayLiteral {
    pub token: Token,
    pub elements: Vec<Expression>,
}

impl Node for ArrayLiteral {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }

    fn string(&self) -> String {
        let mut out = String::from("");

        let mut elements: Vec<String> = vec![];
        for element in &self.elements {
            elements.push(element.string());
        }

        out.push_str("[");
        out.push_str(&elements.join(", "));
        out.push_str("]");

        out
    }
}

#[derive(Debug, Clone)]
pub struct IndexExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub index: Box<Expression>,
}

impl Node for IndexExpression {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }

    fn string(&self) -> String {
        let mut out = String::from("");

        out.push_str("(");
        out.push_str(&self.left.string());
        out.push_str("[");
        out.push_str(&self.index.string());
        out.push_str("])");

        out
    }
}

#[derive(Debug, Clone)]
pub struct PrefixExpression {
    pub token: Token,
    pub right: Option<Box<Expression>>,
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

#[derive(Debug, Clone)]
pub struct InfixExpression {
    pub token: Token,
    pub right: Option<Box<Expression>>,
    pub operator: Token,
    pub left: Option<Box<Expression>>,
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

#[derive(Debug, Clone)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
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

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
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

#[derive(Debug, Clone)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
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

#[cfg(test)]
mod ast_tests {

    use super::{Expression, Identifier, LetStatement, Program};
    use crate::{ast::Statement, token::Token};

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![Statement::Let(LetStatement {
                token: Token::Let,
                name: Identifier {
                    token: Token::Ident("myVar".to_string()),
                },
                value: Some(Expression::Identifier(Identifier {
                    token: Token::Ident("anotherVar".to_string()),
                })),
            })],
        };

        assert_eq!(program.string(), "let myVar = anotherVar;");
    }
}
