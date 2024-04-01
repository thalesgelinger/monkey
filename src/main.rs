mod lexer;
mod repl;
mod token;

use crate::repl::repl;

fn main() {
    println!("Hello Monkey");
    repl()
}
