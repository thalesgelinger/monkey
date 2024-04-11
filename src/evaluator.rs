use core::panic;

use crate::ast::{IntegerLiteral, Node, Program, Statement};
use crate::object::{Integer, Null, Object};
use crate::token::Token;

#[warn(dead_code)]
fn eval<T>(node: &Box<T>) -> Box<dyn Object>
where
    T: Node + Sized,
{
    if let Some(program) = node.as_any().downcast_ref::<Program>() {
        eval_statements(&program.statements)
    } else if let Some(integer) = node.as_any().downcast_ref::<IntegerLiteral>() {
        match integer.token {
            Token::Int(value) => Box::new(Integer { value }),
            _ => panic!("Error, token should be an Int"),
        }
    } else {
        Box::new(Null)
    }
}

fn eval_statements<T>(statements: &Vec<Box<T>>) -> Box<dyn Object>
where
    T: Statement + Sized,
{
    let mut result: Box<dyn Object>;

    for stmt in statements {
        result = eval(stmt);
    }

    result
}

#[cfg(test)]
mod evaluator_test {

    use std::usize;

    use super::eval;
    use crate::ast::AnyNode;
    use crate::lexer::Lexer;
    use crate::object::{Integer, Object};
    use crate::parser::Parser;

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![("5", 5), ("10", 10)];

        for (input, expected) in tests {
            let evaluated = test_eval(input.into());
            test_integer_object(evaluated, expected);
        }
    }

    fn test_eval(input: String) -> Box<dyn Object> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        println!("Program: {:?}", program);

        eval(Box::new(program))
    }

    fn test_integer_object(obj: Box<dyn Object>, expected: usize) {
        println!("Obj: {:?}", obj);

        let result = match obj.as_any().downcast_ref::<Integer>() {
            Some(statement) => Box::new(statement),
            None => panic!("Object is not an integer"),
        };
        assert_eq!(result.value, expected);
    }
}
