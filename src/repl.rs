use std::io::{self, Write};

use crate::{lexer::Lexer, parser::Parser};

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

        println!("{:#?}", program);

        for error in parser.errors() {
            eprintln!("error: {}", error);
        }

        input.clear();
    }
}
