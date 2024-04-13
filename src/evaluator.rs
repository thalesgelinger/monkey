use core::panic;

use crate::ast::{
    self, AnyNode, Expression, ExpressionStatement, InfixExpression, IntegerLiteral,
    PrefixExpression, Program, Statement,
};
use crate::object::{Boolean, Integer, Null, Object, ObjectType};
use crate::token::Token;

pub trait Eval: AnyNode {
    fn eval(&self) -> Box<dyn Object>;
}

impl Eval for Program {
    fn eval(&self) -> Box<dyn Object> {
        let mut result: Box<dyn Object> = Box::new(Null);

        for stmt in &self.statements {
            result = stmt.eval();
        }

        result
    }
}

impl Eval for dyn Statement {
    fn eval(&self) -> Box<dyn Object> {
        if let Some(exp) = self.as_any().downcast_ref::<ExpressionStatement>() {
            exp.expression
                .as_ref()
                .expect("error missing expression")
                .eval()
        } else {
            Box::new(Null)
        }
    }
}

impl Eval for dyn Expression {
    fn eval(&self) -> Box<dyn Object> {
        if let Some(integer) = self.as_any().downcast_ref::<IntegerLiteral>() {
            let integer = match integer.token {
                Token::Int(value) => Integer { value },
                _ => panic!("error should be a Int"),
            };
            Box::new(integer)
        } else if let Some(boolean) = self.as_any().downcast_ref::<ast::Boolean>() {
            let integer = match boolean.token {
                Token::True => Boolean { value: true },
                Token::False => Boolean { value: false },
                _ => panic!("error should be a Int"),
            };
            Box::new(integer)
        } else if let Some(exp) = self.as_any().downcast_ref::<PrefixExpression>() {
            let right = exp
                .right
                .as_ref()
                .expect("error missing right expression")
                .eval();

            match exp.operator {
                Token::Bang => eval_bang(right),
                Token::Minus => eval_minus_prefix(right),
                _ => Box::new(Null),
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

            match (left.t(), right.t()) {
                (ObjectType::Integer(left), ObjectType::Integer(right)) => match exp.operator {
                    Token::Plus => Box::new(Integer {
                        value: left.value + right.value,
                    }),
                    Token::Minus => Box::new(Integer {
                        value: left.value - right.value,
                    }),
                    Token::Asterisk => Box::new(Integer {
                        value: left.value * right.value,
                    }),
                    Token::Slash => Box::new(Integer {
                        value: left.value / right.value,
                    }),
                    _ => Box::new(Null),
                },
                _ => Box::new(Null),
            }
        } else {
            Box::new(Null)
        }
    }
}

fn eval_minus_prefix(right: Box<dyn Object>) -> Box<dyn Object> {
    match right.t() {
        ObjectType::Integer(integer) => Box::new(Integer {
            value: -integer.value,
        }),
        _ => Box::new(Null),
    }
}

fn eval_bang(right: Box<dyn Object>) -> Box<dyn Object> {
    match right.t() {
        ObjectType::Boolean(boolean) => Box::new(Boolean {
            value: !boolean.value,
        }),
        ObjectType::Null => Box::new(Boolean { value: true }),
        _ => Box::new(Boolean { value: false }),
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
        let tests = vec![("true", true), ("false", false)];

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

    fn test_eval(input: String) -> Box<dyn Object> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        program.eval()
    }
}
