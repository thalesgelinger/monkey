use crate::ast::{
    Expression, ExpressionStatement, Identifier, LetStatement, Program, ReturnStatement, Statement,
};
use crate::lexer::Lexer;
use crate::token::Token;

enum Precedence {
    Lowest,
    Equals,
    Lessgreater,
    Sum,
    Product,
    Prefix,
    Call,
}

pub struct Parser {
    lexer: Lexer,
    pub current_token: Token,
    pub peek_token: Token,
    pub errors: Vec<String>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            current_token: Token::Illegal,
            peek_token: Token::Illegal,
            errors: vec![],
        };
        parser.next_token();
        parser.next_token();

        parser
    }

    fn next_token(&mut self) {
        self.current_token = std::mem::replace(&mut self.peek_token, self.lexer.next_token());
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        while self.current_token != Token::Eof {
            println!("Getting statement");
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt)
            };

            self.next_token();
        }
        program
    }

    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Box<dyn Statement>> {
        let let_token = self.current_token.clone();

        let name = match self.peek_token.clone() {
            Token::Ident(value) => {
                let ident = Identifier {
                    token: self.peek_token.clone(),
                    value,
                };
                self.next_token();
                ident
            }
            _ => {
                self.peek_error(Token::Ident("".to_string()));
                return None;
            }
        };

        match self.peek_token {
            Token::Assign => self.next_token(),
            _ => return None,
        }

        // TODO: We're skipping the expressions until we
        // encounter a semicolon

        while self.current_token != Token::Semicolon {
            self.next_token()
        }

        let statement = LetStatement {
            token: let_token,
            name,
            value: None,
        };

        Some(Box::new(statement))
    }

    fn peek_error(&mut self, token: Token) {
        let msg = format!(
            "expected next token to be {:?}, got {:?} instead",
            token, self.peek_token
        );
        self.errors.push(msg)
    }

    fn parse_return_statement(&mut self) -> Option<Box<dyn Statement>> {
        let return_token = self.current_token.clone();

        self.next_token();

        // TODO: We're skipping the expressions until we
        // encounter a semicolon

        while self.current_token != Token::Semicolon {
            self.next_token()
        }

        let statement = ReturnStatement {
            token: return_token,
            return_value: None,
        };

        Some(Box::new(statement))
    }

    fn parse_expression_statement(&mut self) -> Option<Box<dyn Statement>> {
        let _expression = match &self.parse_expression(Precedence::Lowest) {
            Some(e) => {
                println!("Expression: {:?}", e);
            }
            None => todo!(),
        };

        None

        // let statement = ExpressionStatement {
        //     token: self.current_token.clone(),
        //     expression,
        // };

        // if self.peek_token == Token::Semicolon {
        //     self.next_token()
        // }

        // Some(Box::new(statement))
    }

    fn parse_expression(&self, precedence: Precedence) -> Option<Box<dyn Expression>> {
        let left_exp = self.prefix_parse_fns().unwrap();
        println!("Left Exp: {:?}", left_exp);

        Some(left_exp)
    }

    fn prefix_parse_fns(&self) -> Option<Box<dyn Expression>> {
        let token = &self.current_token;
        match token {
            Token::Ident(value) => {
                let ident = Box::new(Identifier {
                    token: token.clone(),
                    value: value.to_string(),
                });
                println!("Identifier: {:?}", ident);
                Some(ident)
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod parser_tests {

    use super::Parser;
    use crate::ast::{ExpressionStatement, Identifier};
    use crate::lexer::Lexer;
    use crate::token::Token;

    #[test]
    fn test_let_statements() {
        let input = r"
            let x = 5;
            let y = 10;
            let foobar = 838383;
        ";

        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parse_errors(parser.errors);

        assert_eq!(program.statements.len(), 3);

        let expected_idents = vec!["x", "y", "foobar"];

        for (i, _ident) in expected_idents.iter().enumerate() {
            assert_eq!("let", program.statements[i].token_literal());
        }
    }

    #[test]
    fn test_return_statements() {
        let input = r"
            return 5;
            return 10;
            return 993322;
        ";

        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(program.statements.len(), 3);

        for stmt in program.statements {
            assert_eq!("return", stmt.token_literal())
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parse_errors(parser.errors);
        assert_eq!(program.statements.len(), 1);

        // let stmt = match program.statements.first() {
        //     Some(s) => match s.as_any().downcast_ref::<ExpressionStatement>() {
        //         Some(statement) => Box::new(statement),
        //         None => panic!("Error creating ExpressionStatement"),
        //     },
        //     None => panic!("There's no statement here"),
        // };

        // let ident = match stmt.expression.as_ref() {
        //     Some(e) => match e.as_any().downcast_ref::<Identifier>() {
        //         Some(expression) => Box::new(expression),
        //         None => panic!("Error casting expression identifier"),
        //     },
        //     None => panic!("There's no expression"),
        // };

        // assert_eq!(ident.value, "foobar");
        // assert_eq!(ident.token, Token::Ident(String::from("foobar")));
    }

    fn check_parse_errors(errors: Vec<String>) {
        for msg in errors {
            eprintln!("Parser error: {:?}", msg);
        }
    }
}
