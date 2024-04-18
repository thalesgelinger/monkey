use crate::ast::{
    BlockStatement, Boolean, CallExpression, Expression, ExpressionStatement, FunctionLiteral,
    Identifier, IfExpression, InfixExpression, IntegerLiteral, LetStatement, PrefixExpression,
    Program, ReturnStatement, Statement, StringLiteral,
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

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
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

        if !self.expect_peek(Token::Assign) {
            return None;
        }

        self.next_token();

        let value = match self.parse_expression(Precedence::Lowest) {
            Some(value) => Some(*value),
            None => None,
        };

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        let statement = LetStatement {
            token: let_token,
            name,
            value,
        };

        Some(Statement::Let(statement))
    }

    fn peek_error(&mut self, token: Token) {
        let msg = format!(
            "expected next token to be {}, got {} instead",
            token.string(),
            self.peek_token.string()
        );
        self.errors.push(msg)
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let return_token = self.current_token.clone();

        self.next_token();

        let return_value = match self.parse_expression(Precedence::Lowest) {
            Some(value) => Some(*value),
            None => None,
        };

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        let statement = ReturnStatement {
            token: return_token,
            return_value,
        };

        Some(Statement::Return(statement))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expression = match self.parse_expression(Precedence::Lowest) {
            Some(value) => Some(*value),
            None => None,
        };

        if self.peek_token == Token::Semicolon {
            self.next_token()
        }

        let statement = ExpressionStatement {
            token: self.current_token.clone(),
            expression,
        };

        Some(Statement::Expression(statement))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Box<Expression>> {
        let mut left_exp = match self.prefix_parse_fns() {
            Some(expression) => Some(Box::new(expression)),
            None => {
                self.no_prefix_parse_fn_error(self.current_token.clone());
                return None;
            }
        };

        while self.peek_token != Token::Semicolon && precedence < self.peek_precendence() {
            left_exp = self.infix_parse_fns(left_exp);
        }

        left_exp
    }

    fn prefix_parse_fns(&mut self) -> Option<Expression> {
        let token = &self.current_token;
        match token {
            Token::Ident(_) => {
                let ident = Identifier {
                    token: token.clone(),
                };
                Some(Expression::Identifier(ident))
            }
            Token::Int(_) => {
                let ident = IntegerLiteral {
                    token: token.clone(),
                };
                Some(Expression::Int(ident))
            }
            Token::Bang => self.parse_prefix_expression(),
            Token::Minus => self.parse_prefix_expression(),
            Token::True => {
                let boolean = Boolean { token: Token::True };
                Some(Expression::Boolean(boolean))
            }
            Token::False => {
                let boolean = Boolean {
                    token: Token::False,
                };
                Some(Expression::Boolean(boolean))
            }
            Token::Lparen => self.parse_grouped_expression(),
            Token::If => self.parse_if_expression(),
            Token::Function => self.parse_function_literal(),
            Token::String(_) => {
                let string = StringLiteral {
                    token: token.clone(),
                };
                Some(Expression::String(string))
            }
            _ => None,
        }
    }

    fn infix_parse_fns(&mut self, left: Option<Box<Expression>>) -> Option<Box<Expression>> {
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
            Token::Lparen => {
                self.next_token();
                self.parse_call_expression(left)
            }
            _ => left,
        }
    }

    fn no_prefix_parse_fn_error(&mut self, token: Token) {
        let msg = format!("no prefix parse function for {} found", token.string());
        self.errors.push(msg)
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let mut expression = PrefixExpression {
            token: self.current_token.clone(),
            operator: self.current_token.clone(),
            right: None,
        };

        self.next_token();

        expression.right = self.parse_expression(Precedence::Prefix);

        Some(Expression::Prefix(expression))
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
            Token::Lparen => Precedence::Call,
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

    fn parse_infix_expression(&mut self, left: Option<Box<Expression>>) -> Option<Box<Expression>> {
        let mut expression = InfixExpression {
            token: self.current_token.clone(),
            operator: self.current_token.clone(),
            left,
            right: None,
        };

        let precedence = self.current_precence();
        self.next_token();
        expression.right = self.parse_expression(precedence);

        Some(Box::new(Expression::Infix(expression)))
    }

    fn expect_peek(&mut self, token: Token) -> bool {
        if self.peek_token == token {
            self.next_token();
            true
        } else {
            self.peek_error(token);
            false
        }
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(Token::Rparen) {
            return None;
        }

        match exp {
            Some(e) => Some(*e),
            None => None,
        }
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        let token = &self.current_token.clone();

        if !self.expect_peek(Token::Lparen) {
            return None;
        }

        self.next_token();

        let condition = match self.parse_expression(Precedence::Lowest) {
            Some(condition) => condition,
            None => panic!("Missing condition for if statement"),
        };

        if !self.expect_peek(Token::Rparen) {
            return None;
        }

        if !self.expect_peek(Token::Lbrace) {
            return None;
        }

        let consequence = self.parse_block_statement();

        if self.peek_token == Token::Else {
            self.next_token();

            if !self.expect_peek(Token::Lbrace) {
                return None;
            }

            let alternative = Some(self.parse_block_statement());

            let expression = IfExpression {
                token: token.clone(),
                condition,
                consequence,
                alternative,
            };
            return Some(Expression::If(expression));
        }

        let expression = IfExpression {
            token: token.clone(),
            condition,
            consequence,
            alternative: None,
        };
        Some(Expression::If(expression))
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let token = &self.current_token.clone();
        let mut statements: Vec<Statement> = vec![];

        self.next_token();

        while self.current_token != Token::Rbrace && self.current_token != Token::Eof {
            match self.parse_statement() {
                Some(stmt) => statements.push(stmt),
                None => continue,
            };
            self.next_token();
        }

        let block = BlockStatement {
            token: token.clone(),
            statements,
        };

        block
    }

    fn parse_function_literal(&mut self) -> Option<Expression> {
        let token = self.current_token.clone();

        if !self.expect_peek(Token::Lparen) {
            return None;
        }

        let parameters = self
            .parse_function_parameters()
            .expect("Syntax error on funciton declaration");

        if !self.expect_peek(Token::Lbrace) {
            return None;
        }

        let body = self.parse_block_statement();

        let lit = FunctionLiteral {
            token,
            parameters,
            body,
        };

        Some(Expression::Function(lit))
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Identifier>> {
        let mut identifiers: Vec<Identifier> = vec![];

        if self.peek_token == Token::Rparen {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();

        let ident = Identifier {
            token: self.current_token.clone(),
        };
        identifiers.push(ident);

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            let ident = Identifier {
                token: self.current_token.clone(),
            };
            identifiers.push(ident);
        }

        if !self.expect_peek(Token::Rparen) {
            return None;
        }

        Some(identifiers)
    }

    fn parse_call_expression(
        &mut self,
        function: Option<Box<Expression>>,
    ) -> Option<Box<Expression>> {
        let token = self.current_token.clone();
        let arguments = self
            .parse_call_arguments()
            .expect("Erro on getting arguments");
        let call = CallExpression {
            token: token.clone(),
            function: function.expect("Missing function"),
            arguments,
        };
        Some(Box::new(Expression::Call(call)))
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<Box<Expression>>> {
        let mut args = vec![];

        if self.peek_token == Token::Rparen {
            self.next_token();
            return Some(args);
        }

        self.next_token();

        args.push(
            self.parse_expression(Precedence::Lowest)
                .expect("Missing expression on parse call arguments"),
        );

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            args.push(
                self.parse_expression(Precedence::Lowest)
                    .expect("Missing expression on parse call arguments"),
            );
        }

        if !self.expect_peek(Token::Rparen) {
            return None;
        }

        Some(args)
    }
}

#[cfg(test)]
mod parser_tests {

    use super::Parser;
    use crate::ast::{Expression, Identifier, Node, Statement};
    use crate::lexer::Lexer;
    use crate::token::Token;

    #[test]
    fn test_let_statements() {
        let tests = vec![
            ("let x = 5;", Token::Ident("x".to_string()), Token::Int(5)),
            ("let y = true;", Token::Ident("y".to_string()), Token::True),
            (
                "let foobar = y;",
                Token::Ident("foobar".to_string()),
                Token::Ident("y".to_string()),
            ),
        ];

        for (input, ident, expected_value) in tests {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parse_errors(parser.errors);

            assert_eq!(program.statements.len(), 1);

            let let_stmt = match program.statements.first() {
                Some(s) => match s {
                    Statement::Let(stmt) => stmt,
                    _ => panic!("Error creating Let Statement"),
                },
                None => panic!("There's no statement here"),
            };

            assert_eq!(let_stmt.name, Identifier { token: ident });

            match (
                &expected_value,
                let_stmt
                    .value
                    .as_ref()
                    .expect("Error getting let statemnet value"),
            ) {
                (Token::Ident(_), Expression::Identifier(function)) => {
                    assert_eq!(function.token, expected_value);
                }
                (Token::Int(_), Expression::Int(function)) => {
                    assert_eq!(function.token, expected_value);
                }
                (Token::True | Token::False, Expression::Boolean(boolean)) => {
                    assert_eq!(boolean.token, expected_value);
                }
                _ => panic!("No test implement yet"),
            };
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = vec![
            ("return 5;", Token::Return, Token::Int(5)),
            ("return a", Token::Return, Token::Ident("a".to_string())),
            ("return true", Token::Return, Token::True),
        ];

        for (input, ident, expected_value) in tests {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parse_errors(parser.errors);

            assert_eq!(program.statements.len(), 1);

            let stmt = match program.statements.first() {
                Some(s) => match s {
                    Statement::Return(statement) => statement,
                    _ => panic!("Error creating ReturnStatement"),
                },
                None => panic!("There's no statement here"),
            };

            assert_eq!(stmt.token, ident);

            match (
                &expected_value,
                stmt.return_value
                    .as_ref()
                    .expect("Error getting return value"),
            ) {
                (Token::Ident(_), Expression::Identifier(function)) => {
                    assert_eq!(function.token, expected_value);
                }
                (Token::Int(_), Expression::Int(function)) => {
                    assert_eq!(function.token, expected_value);
                }
                (Token::True | Token::False, Expression::Boolean(boolean)) => {
                    assert_eq!(boolean.token, expected_value);
                }
                _ => panic!("No test implement yet"),
            };
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

        let stmt = match program
            .statements
            .first()
            .expect("There's no statement here")
        {
            Statement::Expression(statement) => Box::new(statement),
            _ => panic!("Error creating ExpressionStatement"),
        };

        let ident = match stmt.expression.as_ref().expect("There's no expression") {
            Expression::Identifier(expression) => expression,
            _ => panic!("Error casting expression identifier"),
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

        let stmt = match program
            .statements
            .first()
            .expect("There's no statement here")
        {
            Statement::Expression(statement) => Box::new(statement),
            _ => panic!("Error creating ExpressionStatement"),
        };

        let ident = match stmt.expression.as_ref().expect("There's no expression") {
            Expression::Int(expression) => expression,
            _ => panic!("Error casting expression identifier"),
        };

        assert_eq!(ident.token, Token::Int(5));
    }

    #[test]
    fn test_boolean_expression() {
        let tests = vec![("true;", Token::True), ("false;", Token::False)];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parse_errors(parser.errors);
            assert_eq!(program.statements.len(), 1);

            let stmt = match program
                .statements
                .first()
                .expect("There's no statement here")
            {
                Statement::Expression(statement) => Box::new(statement),
                _ => panic!("Error creating ExpressionStatement"),
            };

            let ident = match stmt.expression.as_ref().expect("There's no expression") {
                Expression::Boolean(expression) => expression,
                _ => panic!("Error casting expression identifier"),
            };

            assert_eq!(ident.token, expected);
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let prefix_tests = vec![
            ("!5;", Token::Bang, Token::Int(5)),
            ("-15;", Token::Minus, Token::Int(15)),
            ("!true;", Token::Bang, Token::True),
            ("!false;", Token::Bang, Token::False),
        ];

        for (input, operator, value) in prefix_tests {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parse_errors(parser.errors);

            assert_eq!(program.statements.len(), 1);

            let stmt = match program
                .statements
                .first()
                .expect("There's no statement here")
            {
                Statement::Expression(statement) => Box::new(statement),
                _ => panic!("Error creating ExpressionStatement"),
            };

            let expression = match stmt.expression.as_ref().expect("There's no expression") {
                Expression::Prefix(expression) => expression,
                _ => panic!("Error casting expression identifier"),
            };

            assert_eq!(expression.operator, operator);

            test_expression(
                &*expression.right.as_ref().expect("Missing right expression"),
                &value,
            );
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
            ("true == true", Token::True, Token::Eq, Token::True),
            ("true != false", Token::True, Token::NotEq, Token::False),
            ("false == false", Token::False, Token::Eq, Token::False),
        ];

        for (input, left_token, operator, right_token) in infix_tests {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parse_errors(parser.errors);

            assert_eq!(program.statements.len(), 1);

            let stmt = match program
                .statements
                .first()
                .expect("There's no statement here")
            {
                Statement::Expression(statement) => Box::new(statement),
                _ => panic!("Error creating ExpressionStatement"),
            };

            let expression = match stmt.expression.as_ref().expect("There's no expression") {
                Expression::Infix(expression) => expression,
                _ => panic!("Error casting expression identifier"),
            };

            assert_eq!(expression.operator, operator);

            test_expression(
                &*expression.right.as_ref().expect("Missing right expression"),
                &right_token,
            );
            test_expression(
                &*expression.left.as_ref().expect("Missing left expression"),
                &left_token,
            );
        }
    }

    fn test_expression(expression: &Expression, expected_token: &Token) {
        match (expected_token, expression) {
            (Token::Ident(_), Expression::Identifier(function)) => {
                assert_eq!(&function.token, expected_token);
            }
            (Token::Int(_), Expression::Int(function)) => {
                assert_eq!(&function.token, expected_token);
            }
            (Token::True | Token::False, Expression::Boolean(boolean)) => {
                assert_eq!(&boolean.token, expected_token);
            }
            _ => panic!("No test implement yet"),
        };
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parse_errors(parser.errors);

            let actual = program.string();
            assert_eq!(actual, expected)
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";

        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parse_errors(parser.errors);

        assert_eq!(program.statements.len(), 1);

        let stmt = match program
            .statements
            .first()
            .expect("There's no statement here")
        {
            Statement::Expression(statement) => Box::new(statement),
            _ => panic!("Error creating ExpressionStatement"),
        };

        let expression = match stmt.expression.as_ref().expect("Tere's no expression") {
            Expression::If(expression) => expression,
            _ => panic!("Error casting expression identifier"),
        };

        let condition = expression.condition.clone();

        assert_eq!(condition.string(), "(x < y)");

        assert_eq!(expression.consequence.statements.len(), 1);

        let consequence = match expression.consequence.statements.first().unwrap() {
            Statement::Expression(s) => s,
            _ => panic!("There's no statement here"),
        };

        assert_eq!(consequence.token, Token::Ident("x".to_string()));

        if let Some(_) = &expression.alternative {
            panic!("Alternative should be null");
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parse_errors(parser.errors);

        assert_eq!(program.statements.len(), 1);

        let stmt = match program
            .statements
            .first()
            .expect("There's no statement here")
        {
            Statement::Expression(statement) => Box::new(statement),
            _ => panic!("Error creating ExpressionStatement"),
        };

        let expression = match stmt.expression.as_ref().expect("There's no expression") {
            Expression::If(expression) => expression,
            _ => panic!("Error casting expression identifier"),
        };

        assert_eq!(expression.condition.string(), "(x < y)");

        assert_eq!(expression.consequence.statements.len(), 1);

        let consequence = match expression.consequence.statements.first().unwrap() {
            Statement::Expression(s) => s,
            _ => panic!("There's no statement here"),
        };

        assert_eq!(consequence.token, Token::Ident("x".to_string()));

        let alternative = expression
            .alternative
            .as_ref()
            .expect("Missing alternative");

        assert_eq!(alternative.statements.len(), 1);

        let alternative_stmt = match alternative.statements.first().unwrap() {
            Statement::Expression(s) => s,
            _ => panic!("There's no statement here"),
        };

        let alternative = match alternative_stmt.expression.as_ref().unwrap() {
            Expression::Identifier(e) => e,
            _ => panic!("There's no expression"),
        };

        assert_eq!(alternative.token, Token::Ident("y".to_string()));
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }";

        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parse_errors(parser.errors);

        assert_eq!(program.statements.len(), 1);

        let stmt = match program
            .statements
            .first()
            .expect("There's no statement here")
        {
            Statement::Expression(statement) => Box::new(statement),
            _ => panic!("Error creating ExpressionStatement"),
        };

        let function = match stmt.expression.as_ref().expect("Missing expression") {
            Expression::Function(e) => e,
            _ => panic!("There's no expression"),
        };

        assert_eq!(function.parameters.len(), 2);
        assert_eq!(
            function
                .parameters
                .get(0)
                .expect("There's no first parameter"),
            &Identifier {
                token: Token::Ident("x".to_string())
            }
        );
        assert_eq!(
            function
                .parameters
                .get(1)
                .expect("There's no first parameter"),
            &Identifier {
                token: Token::Ident("y".to_string())
            }
        );

        assert_eq!(function.body.statements.len(), 1);
        let first = function.body.statements.first();

        test_infix_expression(
            first.as_ref().expect("There''"),
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
        );
    }

    fn test_infix_expression(first: &Statement, left: Token, operator: Token, right: Token) {
        let body_stmt = match first {
            Statement::Expression(expression) => expression,
            _ => panic!("Error casting expression function expression"),
        };

        let infix = match body_stmt.expression.as_ref().expect("Missing expression") {
            Expression::Infix(e) => e,
            _ => panic!("There's no expression"),
        };

        test_expression(
            &infix.left.as_ref().expect("Missing left expression"),
            &left,
        );
        assert_eq!(infix.operator, operator);
        test_expression(
            &infix.right.as_ref().expect("Missin right expression"),
            &right,
        );
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parse_errors(parser.errors);

        assert_eq!(program.statements.len(), 1);

        let stmt = match program
            .statements
            .first()
            .expect("There's no statement here")
        {
            Statement::Expression(statement) => Box::new(statement),
            _ => panic!("Error creating ExpressionStatement"),
        };

        let call = match stmt.expression.as_ref().expect("Missin call expression") {
            Expression::Call(e) => e,
            _ => panic!("There's no expression"),
        };

        assert_eq!(call.function.token_literal(), "add".to_string());

        assert_eq!(call.arguments.len(), 3);

        assert_eq!(
            call.arguments
                .get(0)
                .expect("There's no argumento 0")
                .token_literal(),
            format!("{:?}", Token::Int(1))
        );

        let second = match *call.arguments.get(1).unwrap().clone() {
            Expression::Infix(e) => e,
            _ => panic!("There's no expression"),
        };
        test_expression(
            &second.left.expect("Missing left expression"),
            &Token::Int(2),
        );
        assert_eq!(second.operator, Token::Asterisk);
        test_expression(
            &second.right.expect("Missing right expression"),
            &Token::Int(3),
        );

        let third = match *call.arguments.get(2).unwrap().clone() {
            Expression::Infix(e) => e,
            _ => panic!("There's no expression"),
        };
        test_expression(
            &third.left.expect("Missing right expression"),
            &Token::Int(4),
        );
        assert_eq!(third.operator, Token::Plus);
        test_expression(
            &third.right.expect("Missing right expression"),
            &Token::Int(5),
        );
    }

    #[test]
    fn test_string_literal_expression() {
        let input = "\"hello world\";";

        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parse_errors(parser.errors);

        let stmt = match program
            .statements
            .first()
            .expect("There's no statement here")
        {
            Statement::Expression(statement) => Box::new(statement),
            _ => panic!("Error creating ExpressionStatement"),
        };

        let literal = match stmt.expression.as_ref().expect("Missin call expression") {
            Expression::String(e) => e,
            _ => panic!("There's no expression"),
        };

        assert_eq!(literal.token, Token::String("hello world".to_string()))
    }

    fn check_parse_errors(errors: Vec<String>) {
        for msg in errors {
            eprintln!("Parser error: {:?}", msg);
        }
    }
}
