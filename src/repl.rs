use crate::{environment::Env, evaluator::Eval, lexer::Lexer, parser::Parser};
use std::io::{self, Write};

const MONKEY_FACE: &str = r#"
            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
"#;

pub fn start_repl() {
    println!("Hello! This is the Monkey programming language!");
    println!("Feel free to type in commands");

    let mut env = Env::new();
    loop {
        let mut input = String::new();
        print!(">> ");

        io::stdout().flush().unwrap();

        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        if parser.errors.len() > 0 {
            println!("{}", MONKEY_FACE);
            println!("Woops! We ran into some monkey business here!");
            println!("  parser errors:");
            for msg in parser.errors {
                println!("      {:?}", msg);
            }
        } else {
            let evaluated = program.eval(&mut env);
            println!("{}", evaluated.inspect());
        }
    }
}
