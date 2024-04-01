use crate::lexer::Lexer;
use crate::token::Token;
use std::io;

pub fn repl() {
    loop {
        let mut input = String::new();

        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");

        let mut lexer = Lexer::new(input);

        let mut tok = lexer.next_token();

        while tok != Token::Eof {
            println!("{:?}", tok);
            tok = lexer.next_token();
        }
    }
}
