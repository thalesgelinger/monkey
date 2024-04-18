use crate::token::Token;

pub struct Lexer {
    input: Vec<u8>,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut lexer = Lexer {
            input: input.into(),
            position: 0,
            read_position: 0,
            ch: 0,
        };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let token = match self.ch {
            b'=' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::Eq
                } else {
                    Token::Assign
                }
            }
            b'!' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::NotEq
                } else {
                    Token::Bang
                }
            }
            b'+' => Token::Plus,
            b'-' => Token::Minus,
            b'/' => Token::Slash,
            b'*' => Token::Asterisk,
            b'<' => Token::Lt,
            b'>' => Token::Gt,
            b';' => Token::Semicolon,
            b'(' => Token::Lparen,
            b')' => Token::Rparen,
            b',' => Token::Comma,
            b'{' => Token::Lbrace,
            b'}' => Token::Rbrace,
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                return match self.read_identifier().as_str() {
                    "fn" => Token::Function,
                    "let" => Token::Let,
                    "true" => Token::True,
                    "false" => Token::False,
                    "if" => Token::If,
                    "else" => Token::Else,
                    "return" => Token::Return,
                    ident => Token::Ident(ident.to_string()),
                }
            }
            b'0'..=b'9' => return Token::Int(self.read_number()),
            b'"' => self.read_string(),
            0 => Token::Eof,
            _ => unreachable!("Character not supported!"),
        };
        self.read_char();
        token
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;

        while self.ch.is_ascii_alphabetic() || self.ch == b'_' {
            self.read_char();
        }

        match String::from_utf8(self.input[position..self.position].to_vec()) {
            Ok(identifier) => identifier,
            Err(_) => panic!("Error getting identifier"),
        }
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn read_number(&mut self) -> isize {
        let position = self.position;

        while self.ch.is_ascii_digit() {
            self.read_char();
        }

        match String::from_utf8(self.input[position..self.position].to_vec()) {
            Ok(number) => number.parse::<isize>().unwrap(),
            Err(_) => panic!("Error getting identifier"),
        }
    }

    fn peek_char(&self) -> u8 {
        if self.read_position > self.input.len() {
            0
        } else {
            self.input[self.read_position]
        }
    }

    fn read_string(&mut self) -> Token {
        let position = self.position + 1;

        loop {
            self.read_char();
            if self.ch == b'"' || self.ch == 0 {
                break;
            }
        }

        let value = match String::from_utf8(self.input[position..self.position].to_vec()) {
            Ok(string) => string,
            Err(_) => panic!("Error getting string"),
        };
        Token::String(value)
    }
}

#[cfg(test)]
mod lexer_tests {

    use crate::token::Token;

    use super::Lexer;

    #[test]
    fn get_next_token() {
        let input = "=+(){},;";
        let mut lexer = Lexer::new(input.into());

        let tokens = vec![
            Token::Assign,
            Token::Plus,
            Token::Lparen,
            Token::Rparen,
            Token::Lbrace,
            Token::Rbrace,
            Token::Comma,
            Token::Semicolon,
        ];

        for token in tokens {
            let next_token = lexer.next_token();
            assert_eq!(token, next_token);
        }
    }

    #[test]
    fn test_next_token() {
        let input = r#"
        let five = 5;
        let ten = 10;

        let add = fn(x, y) {
          x + y;
        };

        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;

        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;
        "foobar"
        "foo bar"
        "#;

        let tests = vec![
            Token::Let,
            Token::Ident(String::from("five")),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("ten")),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("add")),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Ident(String::from("x")),
            Token::Comma,
            Token::Ident(String::from("y")),
            Token::Rparen,
            Token::Lbrace,
            Token::Ident(String::from("x")),
            Token::Plus,
            Token::Ident(String::from("y")),
            Token::Semicolon,
            Token::Rbrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("result")),
            Token::Assign,
            Token::Ident(String::from("add")),
            Token::Lparen,
            Token::Ident(String::from("five")),
            Token::Comma,
            Token::Ident(String::from("ten")),
            Token::Rparen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::Gt,
            Token::Int(5),
            Token::Semicolon,
            Token::If,
            Token::Lparen,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::Rparen,
            Token::Lbrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::Rbrace,
            Token::Else,
            Token::Lbrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::Rbrace,
            Token::Int(10),
            Token::Eq,
            Token::Int(10),
            Token::Semicolon,
            Token::Int(10),
            Token::NotEq,
            Token::Int(9),
            Token::Semicolon,
            Token::String("foobar".to_string()),
            Token::String("foo bar".to_string()),
            Token::Eof,
        ];

        let mut lexer = Lexer::new(input.into());

        for test_token in tests {
            let tok = lexer.next_token();
            assert_eq!(tok, test_token)
        }
    }
}
