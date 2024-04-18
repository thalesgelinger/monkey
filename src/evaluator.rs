use core::panic;
use std::mem::discriminant;
use std::rc::Rc;

use crate::ast::{Expression, Program, Statement};
use crate::environment::Env;
use crate::object::{Function, Object};
use crate::token::Token;

pub trait Eval {
    fn eval(&self, env: &Rc<Env>) -> Object;
}

impl Eval for Program {
    fn eval(&self, env: &Rc<Env>) -> Object {
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

impl Eval for Statement {
    fn eval(&self, env: &Rc<Env>) -> Object {
        match self {
            Statement::Let(exp) => {
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
            }
            Statement::Return(exp) => {
                let result = exp
                    .return_value
                    .as_ref()
                    .expect("error missing expression")
                    .eval(env);

                match result {
                    Object::Error(_) => result,
                    _ => Object::Return(Box::new(result)),
                }
            }
            Statement::Expression(exp) => exp
                .expression
                .as_ref()
                .expect("error missing expression")
                .eval(env),
        }
    }
}

impl Eval for Expression {
    fn eval(&self, env: &Rc<Env>) -> Object {
        match self {
            Expression::Identifier(ident) => match &ident.token {
                Token::Ident(key) => match env.get(key.to_string()) {
                    Some(val) => val,
                    None => Object::Error(format!("identifier not found: {}", key)),
                },
                _ => panic!("error should be an ident"),
            },
            Expression::Int(integer) => {
                let integer = match integer.token {
                    Token::Int(value) => Object::Integer(value),
                    _ => panic!("error should be a Int"),
                };
                integer
            }
            Expression::Boolean(boolean) => {
                let boolean = match boolean.token {
                    Token::True => Object::Boolean(true),
                    Token::False => Object::Boolean(false),
                    _ => panic!("error should be a Int"),
                };
                boolean
            }
            Expression::Prefix(exp) => {
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
            }
            Expression::Infix(exp) => {
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
                    _ => match discriminant(&left) != discriminant(&right) {
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
            }
            Expression::If(exp) => {
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
            }
            Expression::BlockStatement(exp) => eval_block_statements(&exp.statements, env),
            Expression::Function(func) => {
                let function = Function {
                    body: func.body.clone(),
                    env: Rc::clone(env),
                    parameters: func.parameters.clone(),
                };
                Object::Function(function)
            }
            Expression::Call(call) => {
                let function = call.function.eval(env);
                match function {
                    Object::Error(_) => return function,
                    _ => (),
                };

                let args = eval_expressions(&call.arguments, env);

                match args.first() {
                    Some(value) => match value {
                        Object::Error(_) => value.clone(),
                        _ => apply_function(&function, &args),
                    },
                    None => apply_function(&function, &args),
                }
            }
        }
    }
}

fn apply_function(function: &Object, args: &Vec<Object>) -> Object {
    let function = match function {
        Object::Function(f) => f,
        _ => panic!("This is not a function"),
    };
    let extended_env = extended_function_env(function, args);
    let evaluated = eval_block_statements(&function.body.statements, &extended_env);

    match evaluated {
        Object::Return(value) => *value,
        _ => evaluated,
    }
}

fn extended_function_env(function: &Function, args: &Vec<Object>) -> Rc<Env> {
    let env = Env::new_enclosed(&function.env);

    for (i, param) in function.parameters.iter().enumerate() {
        match (param.token.clone(), args.get(i)) {
            (Token::Ident(key), Some(value)) => env.set(key, value.clone()),
            _ => (),
        }
    }

    env
}

fn eval_expressions(expressions: &Vec<Box<Expression>>, env: &Rc<Env>) -> Vec<Object> {
    let mut result: Vec<Object> = vec![];

    for exp in expressions {
        let evaluated = exp.eval(env);

        match evaluated {
            Object::Error(_) => return vec![evaluated],
            _ => (),
        }

        result.push(evaluated);
    }

    result
}

fn eval_block_statements(statements: &Vec<Statement>, env: &Rc<Env>) -> Object {
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
    use crate::ast::Node;
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

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";

        let evaluated = test_eval(input.into());

        match evaluated {
            Object::Function(function) => {
                assert_eq!(function.parameters.len(), 1);
                assert_eq!(
                    function
                        .parameters
                        .get(0)
                        .expect("missing parameter")
                        .string(),
                    "x"
                );
                let expected_body = "(x + 2)";
                assert_eq!(function.body.string(), expected_body);
            }
            _ => panic!("Object is not a function"),
        }
    }

    #[test]
    fn test_function_application() {
        let tests = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        for (input, expected) in tests {
            assert_eq!(test_eval(input.into()).inspect(), expected.to_string())
        }
    }

    #[test]
    fn test_closures() {
        let input =
            "let newAdder = fn(x) { fn(y) { x + y }; }; let addTwo = newAdder(2); addTwo(2);";
        assert_eq!(test_eval(input.into()).inspect(), 4.to_string())
    }

    #[test]
    fn test_recursive() {
        let input =
            "let counter = fn(x) { if (x > 100) { return true; } else { let foobar = 9999; counter(x + 1); } }; counter(0);";
        assert_eq!(test_eval(input.into()).inspect(), true.to_string())
    }

    fn test_eval(input: String) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let mut env = Env::new();

        program.eval(&mut env)
    }
}
