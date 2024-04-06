use crate::ast::{
    Expression, ExpressionStatement, Identifier, InfixExpression, IntegerLiteral, LetStatement,
    PrefixExpression, Program, ReturnStatement, Statement,
};
use crate::lexer::Lexer;
use crate::token::Token;

#[derive(PartialEq, PartialOrd)]
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
            Token::Ident(_) => {
                let ident = Identifier {
                    token: self.peek_token.clone(),
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
        let expression = self.parse_expression(Precedence::Lowest);

        let statement = ExpressionStatement {
            token: self.current_token.clone(),
            expression,
        };

        if self.peek_token == Token::Semicolon {
            self.next_token()
        }

        Some(Box::new(statement))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Box<dyn Expression>> {
        let mut left_exp = match self.prefix_parse_fns() {
            Some(expression) => Some(expression),
            None => {
                self.no_prefix_parse_fn_error(self.current_token.clone());
                return None;
            }
        };

        println!("Left exp: {:?}", left_exp);

        while self.peek_token != Token::Semicolon && precedence < self.peek_precendence() {
            left_exp = self.infix_parse_fns(left_exp)
        }

        left_exp
    }

    fn prefix_parse_fns(&mut self) -> Option<Box<dyn Expression>> {
        let token = &self.current_token;
        match token {
            Token::Ident(_) => {
                let ident = Box::new(Identifier {
                    token: token.clone(),
                });
                Some(ident)
            }
            Token::Int(_) => {
                let ident = Box::new(IntegerLiteral {
                    token: token.clone(),
                });
                Some(ident)
            }
            Token::Bang => self.parse_prefix_expression(),
            Token::Minus => self.parse_prefix_expression(),

            _ => None,
        }
    }

    fn infix_parse_fns(
        &mut self,
        left: Option<Box<dyn Expression>>,
    ) -> Option<Box<dyn Expression>> {
        let token = &self.peek_token;
        match token {
            Token::Plus => {
                self.next_token();
                self.parse_infix_expression(left)
            }
            Token::Minus => {
                self.next_token();
                self.parse_infix_expression(left)
            }
            Token::Slash => {
                self.next_token();
                self.parse_infix_expression(left)
            }
            Token::Asterisk => {
                self.next_token();
                self.parse_infix_expression(left)
            }

            Token::Eq => {
                self.next_token();
                self.parse_infix_expression(left)
            }

            Token::NotEq => {
                self.next_token();
                self.parse_infix_expression(left)
            }

            Token::Lt => {
                self.next_token();
                self.parse_infix_expression(left)
            }

            Token::Gt => {
                self.next_token();
                self.parse_infix_expression(left)
            }
            _ => left,
        }
    }

    fn no_prefix_parse_fn_error(&mut self, token: Token) {
        let msg = format!("no prefix parse function for {:?} found", token);
        self.errors.push(msg)
    }

    fn parse_prefix_expression(&mut self) -> Option<Box<dyn Expression>> {
        let mut expression = PrefixExpression {
            token: self.current_token.clone(),
            operator: self.current_token.clone(),
            right: None,
        };

        self.next_token();

        expression.right = self.parse_expression(Precedence::Prefix);

        Some(Box::new(expression))
    }

    fn peek_precendence(&self) -> Precedence {
        match self.peek_token {
            Token::Eq => Precedence::Equals,
            Token::NotEq => Precedence::Equals,
            Token::Lt => Precedence::Lessgreater,
            Token::Gt => Precedence::Lessgreater,
            Token::Plus => Precedence::Sum,
            Token::Minus => Precedence::Sum,
            Token::Slash => Precedence::Product,
            Token::Asterisk => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }

    fn current_precence(&self) -> Precedence {
        match self.current_token {
            Token::Eq => Precedence::Equals,
            Token::NotEq => Precedence::Equals,
            Token::Lt => Precedence::Lessgreater,
            Token::Gt => Precedence::Lessgreater,
            Token::Plus => Precedence::Sum,
            Token::Minus => Precedence::Sum,
            Token::Slash => Precedence::Product,
            Token::Asterisk => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }

    fn parse_infix_expression(
        &mut self,
        left: Option<Box<dyn Expression>>,
    ) -> Option<Box<dyn Expression>> {
        let mut expression = InfixExpression {
            token: self.current_token.clone(),
            operator: self.current_token.clone(),
            left,
            right: None,
        };

        let precedence = self.current_precence();
        self.next_token();
        expression.right = self.parse_expression(precedence);

        Some(Box::new(expression))
    }
}

#[cfg(test)]
mod parser_tests {

    use super::Parser;
    use crate::ast::{
        ExpressionStatement, Identifier, InfixExpression, IntegerLiteral, PrefixExpression,
    };
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

        let stmt = match program.statements.first() {
            Some(s) => match s.as_any().downcast_ref::<ExpressionStatement>() {
                Some(statement) => Box::new(statement),
                None => panic!("Error creating ExpressionStatement"),
            },
            None => panic!("There's no statement here"),
        };

        let ident = match stmt.expression.as_ref() {
            Some(e) => match e.as_any().downcast_ref::<Identifier>() {
                Some(expression) => Box::new(expression),
                None => panic!("Error casting expression identifier"),
            },
            None => panic!("There's no expression"),
        };

        assert_eq!(ident.token, Token::Ident(String::from("foobar")));
    }
    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parse_errors(parser.errors);

        assert_eq!(program.statements.len(), 1);

        let stmt = match program.statements.first() {
            Some(s) => match s.as_any().downcast_ref::<ExpressionStatement>() {
                Some(statement) => Box::new(statement),
                None => panic!("Error creating ExpressionStatement"),
            },
            None => panic!("There's no statement here"),
        };

        let ident = match stmt.expression.as_ref() {
            Some(e) => match e.as_any().downcast_ref::<IntegerLiteral>() {
                Some(expression) => Box::new(expression),
                None => panic!("Error casting expression identifier"),
            },
            None => panic!("There's no expression"),
        };

        assert_eq!(ident.token, Token::Int(5));
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let prefix_tests = vec![
            ("!5;", Token::Bang, Token::Int(5)),
            ("-15;", Token::Minus, Token::Int(15)),
        ];
        for (input, operator, value) in prefix_tests {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parse_errors(parser.errors);

            assert_eq!(program.statements.len(), 1);

            let stmt = match program.statements.first() {
                Some(s) => match s.as_any().downcast_ref::<ExpressionStatement>() {
                    Some(statement) => Box::new(statement),
                    None => panic!("Error creating ExpressionStatement"),
                },
                None => panic!("There's no statement here"),
            };

            let expression = match stmt.expression.as_ref() {
                Some(e) => match e.as_any().downcast_ref::<PrefixExpression>() {
                    Some(expression) => Box::new(expression),
                    None => panic!("Error casting expression identifier"),
                },
                None => panic!("There's no expression"),
            };

            assert_eq!(expression.operator, operator);

            let right = match expression.right.as_ref() {
                Some(e) => match e.as_any().downcast_ref::<IntegerLiteral>() {
                    Some(expression) => Box::new(expression),
                    None => panic!("Error casting expression identifier"),
                },
                None => panic!("There's no expression"),
            };

            assert_eq!(right.token, value);
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let infix_tests = vec![
            ("5 + 5;", Token::Int(5), Token::Plus, Token::Int(5)),
            ("5 - 5;", Token::Int(5), Token::Minus, Token::Int(5)),
            ("5 * 5;", Token::Int(5), Token::Asterisk, Token::Int(5)),
            ("5 / 5;", Token::Int(5), Token::Slash, Token::Int(5)),
            ("5 > 5;", Token::Int(5), Token::Gt, Token::Int(5)),
            ("5 < 5;", Token::Int(5), Token::Lt, Token::Int(5)),
            ("5 == 5;", Token::Int(5), Token::Eq, Token::Int(5)),
            ("5 != 5;", Token::Int(5), Token::NotEq, Token::Int(5)),
        ];

        for (input, left_token, operator, right_token) in infix_tests {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parse_errors(parser.errors);

            assert_eq!(program.statements.len(), 1);

            let stmt = match program.statements.first() {
                Some(s) => match s.as_any().downcast_ref::<ExpressionStatement>() {
                    Some(statement) => Box::new(statement),
                    None => panic!("Error creating ExpressionStatement"),
                },
                None => panic!("There's no statement here"),
            };

            let expression = match stmt.expression.as_ref() {
                Some(e) => match e.as_any().downcast_ref::<InfixExpression>() {
                    Some(expression) => Box::new(expression),
                    None => panic!("Error casting expression identifier"),
                },
                None => panic!("There's no expression"),
            };

            assert_eq!(expression.operator, operator);

            let right = match expression.right.as_ref() {
                Some(e) => match e.as_any().downcast_ref::<IntegerLiteral>() {
                    Some(expression) => Box::new(expression),
                    None => panic!("Error casting expression identifier"),
                },
                None => panic!("There's no expression"),
            };

            let left = match expression.left.as_ref() {
                Some(e) => match e.as_any().downcast_ref::<IntegerLiteral>() {
                    Some(expression) => Box::new(expression),
                    None => panic!("Error casting expression identifier"),
                },
                None => panic!("There's no expression"),
            };

            assert_eq!(right.token, right_token);
            assert_eq!(left.token, left_token);
        }
    }

    fn check_parse_errors(errors: Vec<String>) {
        for msg in errors {
            eprintln!("Parser error: {:?}", msg);
        }
    }
}
