mod ast;
mod environment;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;

use std::env;

use lexer::Lexer;
use parser::Parser;

use crate::{environment::Env, evaluator::Eval, repl::start_repl};

fn main() {
    let args: Vec<_> = env::args().collect();

    match args.get(1) {
        Some(file) => match file.ends_with(".mnk") {
            true => {
                let mut env = Env::new();
                let input = std::fs::read_to_string(file).expect("error could't find file");
                let lexer = Lexer::new(input);
                let mut parser = Parser::new(lexer);
                let program = parser.parse_program();
                println!("{}", program.eval(&mut env).inspect())
            }
            false => panic!("not valid monkey file: {}", file),
        },
        None => start_repl(),
    }
}
