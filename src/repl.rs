use std::io::{self, Write};

use crate::{evaluator, lexer::Lexer, parser::Parser};

const PROMPT: &str = ">> ";

pub fn start() {
    let mut input = String::new();

    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();

        io::stdin()
            .read_line(&mut input)
            .expect("could not read input");

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        if !parser.errors().is_empty() {
            for error in parser.errors() {
                eprintln!("error: {}", error);
            }
            continue;
        }

        let evaluated = evaluator::eval_program(program);
        println!("{}", evaluated);

        input.clear();
    }
}
