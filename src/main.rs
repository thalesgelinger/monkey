mod ast;
mod lexer;
mod parser;
mod repl;
mod token;

use crate::repl::start_repl;

fn main() {
    println!("Hello! This is the Monkey programming language!");
    println!("Feel free to type in commands");

    start_repl();
}
