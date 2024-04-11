use crate::ast::{AnyNode, Program, Statement};
use crate::object::{Boolean, Integer, Null, Object};
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
        match &self.statement_node() {
            Token::Int(value) => Box::new(Integer { value: *value }),
            Token::True => Box::new(Boolean { value: true }),
            Token::False => Box::new(Boolean { value: false }),
            _ => {
                println!("Missing eval implement for: {:?}", self.token_literal());
                Box::new(Null)
            }
        }
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
        let tests = vec![("5", 5), ("10", 10)];

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
