use std::{error::Error, fmt::Display};

use crate::{
    ast::*,
    lexer::Lexer,
    token::{Token, TokenType},
};

pub struct Parser<'src> {
    lexer: Lexer<'src>,
    current_token: Option<Token>,
    peek_token: Option<Token>,
}

impl<'src> Parser<'src> {
    pub fn new(lexer: Lexer<'src>) -> Self {
        let mut parser = Self {
            lexer,
            current_token: None,
            peek_token: None,
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.take();
        self.peek_token = Some(self.lexer.next_token());
    }

    // TODO: Propogate parser error
    fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while let Some(ref token) = self.current_token {
            if token.ttype == TokenType::EOF {
                break;
            }

            if let Ok(statement) = self.parse_statement() {
                program.statements.push(statement);
            }

            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParserError> {
        if let Some(token) = &self.current_token {
            match token.ttype {
                TokenType::Let => self.parse_let_statement(),
                _ => Err(ParserError {}),
            }
        } else {
            Err(ParserError {})
        }
    }

    fn parse_let_statement(&mut self) -> Result<Stmt, ParserError> {
        let token = self.current_token.take();
        self.expect_peek(TokenType::Ident)?;
        let name = Identifier(self.current_token.as_ref().unwrap().literal.clone());

        self.expect_peek(TokenType::Assign)?;

        // TODO: We're skipping the expressions until we encounter a semicolon
        while self.current_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Ok(Stmt::Let(token.unwrap(), name, Expr::Temp))
    }

    fn current_token_is(&self, ttype: TokenType) -> bool {
        if let Some(token) = &self.current_token {
            token.ttype == ttype
        } else {
            false
        }
    }

    fn peek_token_is(&self, ttype: TokenType) -> bool {
        if let Some(token) = &self.peek_token {
            token.ttype == ttype
        } else {
            false
        }
    }

    fn expect_peek(&mut self, ttype: TokenType) -> Result<(), ParserError> {
        if self.peek_token_is(ttype) {
            self.next_token();
            Ok(())
        } else {
            Err(ParserError {})
        }
    }
}

// TODO: More parser error information
#[derive(Debug)]
struct ParserError {}
impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Parser Error!")
    }
}
impl Error for ParserError {}

#[cfg(test)]
mod tests {
    use crate::{ast::Stmt, lexer::Lexer, parser::Parser};

    macro_rules! assert_let_statement {
        ($token:ident, $ttype:ident, $identifier:ident, $expected:expr) => {
            assert_eq!($token.ttype, $crate::token::TokenType::$ttype);
            assert_eq!($identifier.0, $expected);
        };
    }

    #[test]
    fn let_stmt() {
        let input = r#"
        let x = 5;
        let y = 10;
        let foobar = 838383;
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(program.statements.len(), 3);

        let expected_identifiers = ["x", "y", "foobar"];
        for (i, statement) in program.statements.iter().enumerate() {
            dbg!(&statement);
            match statement {
                Stmt::Let(token, identifier, _) => {
                    assert_let_statement!(token, Let, identifier, expected_identifiers[i]);
                }
                _ => panic!("expected a let statement"),
            }
        }
    }
}
