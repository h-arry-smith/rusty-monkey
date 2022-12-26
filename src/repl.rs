use std::io::{self, Write};

use crate::lexer::Lexer;

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

        for token in lexer {
            println!("{:?}", token);
        }

        input.clear();
    }
}
