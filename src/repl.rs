use std::io::{self, Write};

use crate::{evaluator, lexer::Lexer, object::Environment, parser::Parser};

const PROMPT: &str = ">> ";

pub fn start() {
    let mut input = String::new();
    let mut environment = Environment::new();

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
                eprintln!("parser error: {}", error);
            }
            input.clear();
            continue;
        }

        match evaluator::eval_program(program, &mut environment) {
            Ok(result) => println!("{}", result),
            Err(err) => println!("error: {}", err),
        };

        input.clear();
    }
}
