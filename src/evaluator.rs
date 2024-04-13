use core::panic;

use crate::ast::{
    self, AnyNode, BlockStatement, Expression, ExpressionStatement, Identifier, IfExpression,
    InfixExpression, IntegerLiteral, LetStatement, PrefixExpression, Program, ReturnStatement,
    Statement,
};
use crate::environment::Env;
use crate::object::Object;
use crate::token::Token;

pub trait Eval: AnyNode {
    fn eval(&self, env: &mut Env) -> Object;
}

impl Eval for Program {
    fn eval(&self, env: &mut Env) -> Object {
        let mut result: Object = Object::Null;

        for stmt in &self.statements {
            result = stmt.eval(env);

            match result {
                Object::Return(value) => return *value,
                Object::Error(_) => return result,
                _ => continue,
            }
        }

        result
    }
}

impl Eval for dyn Statement {
    fn eval(&self, env: &mut Env) -> Object {
        if let Some(exp) = self.as_any().downcast_ref::<ExpressionStatement>() {
            exp.expression
                .as_ref()
                .expect("error missing expression")
                .eval(env)
        } else if let Some(exp) = self.as_any().downcast_ref::<LetStatement>() {
            let val = exp
                .value
                .as_ref()
                .expect("error missing expression")
                .eval(env);

            match val {
                Object::Error(_) => return val,
                _ => (),
            }
            let token = exp.name.token.clone();

            match token {
                Token::Ident(name) => env.set(name.into(), val),
                _ => panic!("error should be an ident"),
            }

            // TODO: add env
            Object::Null
        } else if let Some(exp) = self.as_any().downcast_ref::<ReturnStatement>() {
            let result = exp
                .return_value
                .as_ref()
                .expect("error missing expression")
                .eval(env);

            match result {
                Object::Error(_) => result,
                _ => Object::Return(Box::new(result)),
            }
        } else {
            Object::Null
        }
    }
}

impl Eval for dyn Expression {
    fn eval(&self, env: &mut Env) -> Object {
        if let Some(ident) = self.as_any().downcast_ref::<Identifier>() {
            match &ident.token {
                Token::Ident(key) => match env.get(key.to_string()) {
                    Some(val) => val,
                    None => Object::Error(format!("identifier not found: {}", key)),
                },
                _ => panic!("error should be an ident"),
            }
        } else if let Some(integer) = self.as_any().downcast_ref::<IntegerLiteral>() {
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
                .eval(env);

            match right {
                Object::Error(_) => return right,
                _ => (),
            };

            match exp.operator {
                Token::Bang => eval_bang(right),
                Token::Minus => eval_minus_prefix(right),
                _ => Object::Error(format!(
                    "unknown operator: {}{}",
                    exp.operator.string(),
                    right.inspect()
                )),
            }
        } else if let Some(exp) = self.as_any().downcast_ref::<InfixExpression>() {
            let left = exp
                .left
                .as_ref()
                .expect("error missing left expression")
                .eval(env);

            match left {
                Object::Error(_) => return left,
                _ => (),
            };

            let right = exp
                .right
                .as_ref()
                .expect("error missing right expression")
                .eval(env);

            match right {
                Object::Error(_) => return right,
                _ => (),
            };

            match (&left, &right) {
                (Object::Integer(left), Object::Integer(right)) => match exp.operator {
                    Token::Plus => Object::Integer(left + right),
                    Token::Minus => Object::Integer(left - right),
                    Token::Asterisk => Object::Integer(left * right),
                    Token::Slash => Object::Integer(left / right),
                    Token::Lt => Object::Boolean(left < right),
                    Token::Gt => Object::Boolean(left > right),
                    Token::Eq => Object::Boolean(left == right),
                    Token::NotEq => Object::Boolean(left != right),
                    _ => Object::Error(format!(
                        "unknown operator: {} {} {}",
                        left,
                        exp.operator.string(),
                        right
                    )),
                },

                (Object::Boolean(left), Object::Boolean(right)) => match exp.operator {
                    Token::Eq => Object::Boolean(left == right),
                    Token::NotEq => Object::Boolean(left != right),
                    _ => Object::Error(format!(
                        "unknown operator: BOOLEAN {} BOOLEAN",
                        exp.operator.string(),
                    )),
                },
                _ => match left != right {
                    true => Object::Error(format!(
                        "type mismatch: {} {} {}",
                        left,
                        exp.operator.string(),
                        right
                    )),
                    false => Object::Error(format!(
                        "unknown operator: {} {} {}",
                        left,
                        exp.operator.string(),
                        right
                    )),
                },
            }
        } else if let Some(exp) = self.as_any().downcast_ref::<IfExpression>() {
            let condition = exp.condition.eval(env);
            match condition {
                Object::Error(_) => return condition,
                _ => (),
            }

            let is_truthy = match condition {
                Object::Boolean(boolean) => boolean,
                Object::Null => false,
                _ => true,
            };

            if is_truthy {
                eval_block_statements(&exp.consequence.statements, env)
            } else if let Some(aternative) = &exp.alternative {
                eval_block_statements(&aternative.statements, env)
            } else {
                Object::Null
            }
        } else if let Some(exp) = self.as_any().downcast_ref::<BlockStatement>() {
            eval_block_statements(&exp.statements, env)
        } else {
            Object::Null
        }
    }
}

fn eval_block_statements(statements: &Vec<Box<dyn Statement>>, env: &mut Env) -> Object {
    let mut result: Object = Object::Null;

    for stmt in statements {
        result = stmt.eval(env);

        match result {
            Object::Return(_) | Object::Error(_) => return result,
            _ => continue,
        }
    }

    result
}

fn eval_minus_prefix(right: Object) -> Object {
    match right {
        Object::Integer(integer) => Object::Integer(-integer),
        _ => Object::Error(format!("unknown operator: -{}", right)),
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
    use crate::environment::Env;
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

    #[test]
    fn test_return_statements() {
        let tests = vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            ("if (10 > 1) { if (10 > 1) { return 10; } return 1; }", 10),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input.into());
            assert_eq!(evaluated.inspect(), expected.to_string())
        }
    }

    #[test]
    fn test_error_handling() {
        let tests = vec![
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                " if (10 > 1) { if (10 > 1) { return true + false; } return 1; } ",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            ("foobar", "identifier not found: foobar"),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input.into());
            assert_eq!(
                evaluated.inspect(),
                format!("ERROR: {}", expected.to_string())
            )
        }
    }

    #[test]

    fn test_let_statements() {
        let tests = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input.into());

            assert_eq!(evaluated.inspect(), expected.to_string())
        }
    }

    fn test_eval(input: String) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let mut env = Env::new();

        program.eval(&mut env)
    }
}
