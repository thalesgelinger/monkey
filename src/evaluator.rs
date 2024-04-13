use core::panic;

use crate::ast::{
    self, AnyNode, BlockStatement, Expression, ExpressionStatement, IfExpression, InfixExpression,
    IntegerLiteral, PrefixExpression, Program, Statement,
};
use crate::object::Object;
use crate::token::Token;

pub trait Eval: AnyNode {
    fn eval(&self) -> Object;
}

impl Eval for Program {
    fn eval(&self) -> Object {
        let mut result: Object = Object::Null;

        for stmt in &self.statements {
            result = stmt.eval();
        }

        result
    }
}

impl Eval for dyn Statement {
    fn eval(&self) -> Object {
        if let Some(exp) = self.as_any().downcast_ref::<ExpressionStatement>() {
            exp.expression
                .as_ref()
                .expect("error missing expression")
                .eval()
        } else {
            Object::Null
        }
    }
}

impl Eval for dyn Expression {
    fn eval(&self) -> Object {
        if let Some(integer) = self.as_any().downcast_ref::<IntegerLiteral>() {
            let integer = match integer.token {
                Token::Int(value) => Object::Integer(value),
                _ => panic!("error should be a Int"),
            };
            integer
        } else if let Some(boolean) = self.as_any().downcast_ref::<ast::Boolean>() {
            let integer = match boolean.token {
                Token::True => Object::Boolean(true),
                Token::False => Object::Boolean(false),
                _ => panic!("error should be a Int"),
            };
            integer
        } else if let Some(exp) = self.as_any().downcast_ref::<PrefixExpression>() {
            let right = exp
                .right
                .as_ref()
                .expect("error missing right expression")
                .eval();

            match exp.operator {
                Token::Bang => eval_bang(right),
                Token::Minus => eval_minus_prefix(right),
                _ => Object::Null,
            }
        } else if let Some(exp) = self.as_any().downcast_ref::<InfixExpression>() {
            let right = exp
                .right
                .as_ref()
                .expect("error missing right expression")
                .eval();

            let left = exp
                .left
                .as_ref()
                .expect("error missing left expression")
                .eval();

            match (left, right) {
                (Object::Integer(left), Object::Integer(right)) => match exp.operator {
                    Token::Plus => Object::Integer(left + right),
                    Token::Minus => Object::Integer(left - right),
                    Token::Asterisk => Object::Integer(left * right),
                    Token::Slash => Object::Integer(left / right),
                    Token::Lt => Object::Boolean(left < right),
                    Token::Gt => Object::Boolean(left > right),
                    Token::Eq => Object::Boolean(left == right),
                    Token::NotEq => Object::Boolean(left != right),
                    _ => Object::Null,
                },

                (Object::Boolean(left), Object::Boolean(right)) => match exp.operator {
                    Token::Eq => Object::Boolean(left == right),
                    Token::NotEq => Object::Boolean(left != right),
                    _ => Object::Null,
                },
                _ => Object::Null,
            }
        } else if let Some(exp) = self.as_any().downcast_ref::<IfExpression>() {
            let is_truthy = match exp.condition.eval() {
                Object::Boolean(boolean) => boolean,
                Object::Null => false,
                _ => true,
            };

            if is_truthy {
                eval_statements(&exp.consequence.statements)
            } else if let Some(aternative) = &exp.alternative {
                eval_statements(&aternative.statements)
            } else {
                Object::Null
            }
        } else if let Some(exp) = self.as_any().downcast_ref::<BlockStatement>() {
            eval_statements(&exp.statements)
        } else {
            Object::Null
        }
    }
}

fn eval_statements(statements: &Vec<Box<dyn Statement>>) -> Object {
    let mut result: Object = Object::Null;

    for stmt in statements {
        result = stmt.eval();
    }

    result
}

fn eval_minus_prefix(right: Object) -> Object {
    match right {
        Object::Integer(integer) => Object::Integer(-integer),
        _ => Object::Null,
    }
}

fn eval_bang(right: Object) -> Object {
    match right {
        Object::Boolean(boolean) => Object::Boolean(!boolean),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    }
}

#[cfg(test)]
mod evaluator_test {

    use super::Eval;
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::Parser;

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input.into());
            assert_eq!(evaluated.inspect(), expected.to_string())
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input.into());
            assert_eq!(evaluated.inspect(), expected.to_string())
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input.into());
            assert_eq!(evaluated.inspect(), expected.to_string())
        }
    }

    #[test]
    fn test_if_else_expressions() {
        let tests = vec![
            ("if (true) { 10 }", Some(10)),
            ("if (false) { 10 }", None),
            ("if (1) { 10 }", Some(10)),
            ("if (1 < 2) { 10 }", Some(10)),
            ("if (1 > 2) { 10 }", None),
            ("if (1 > 2) { 10 } else { 20 }", Some(20)),
            ("if (1 < 2) { 10 } else { 20 }", Some(10)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input.into());

            match expected {
                Some(value) => assert_eq!(evaluated.inspect(), value.to_string()),
                None => assert_eq!(evaluated.inspect(), "null"),
            }
        }
    }

    fn test_eval(input: String) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        program.eval()
    }
}
