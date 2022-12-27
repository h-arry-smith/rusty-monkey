use std::{error::Error, fmt::Display};

use crate::{
    ast::*,
    lexer::Lexer,
    token::{Token, TokenType},
};

#[derive(Debug)]
pub struct Parser<'src> {
    lexer: Lexer<'src>,
    current_token: Option<Token>,
    peek_token: Option<Token>,
    errors: Vec<ParserError>,
}

impl<'src> Parser<'src> {
    pub fn new(lexer: Lexer<'src>) -> Self {
        let mut parser = Self {
            lexer,
            current_token: None,
            peek_token: None,
            errors: Vec::new(),
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn errors(&self) -> &Vec<ParserError> {
        &self.errors
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

            let stmt = self.parse_statement();
            match stmt {
                Ok(stmt) => program.statements.push(stmt),
                Err(err) => self.errors.push(err),
            }

            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParserError> {
        if let Some(token) = &self.current_token {
            match token.ttype {
                TokenType::Let => self.parse_let_statement(),
                TokenType::Return => self.parse_return_statement(),
                _ => Err(ParserError(format!("Unexepcted token: {:?}", token.ttype))),
            }
        } else {
            Err(ParserError(format!("Unexpected Error!")))
        }
    }

    fn parse_let_statement(&mut self) -> Result<Stmt, ParserError> {
        let token = self.current_token.take();
        self.expect_peek(TokenType::Ident)?;
        let name = Identifier(self.current_token.as_ref().unwrap().literal.clone());

        self.expect_peek(TokenType::Assign)?;

        // TODO: We're skipping the expressions until we encounter a semicolon
        while !self.current_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Ok(Stmt::Let(token.unwrap(), name, Expr::Temp))
    }

    fn parse_return_statement(&mut self) -> Result<Stmt, ParserError> {
        self.next_token();

        // TODO: We're skipping the expressions until we encounter a semicolon
        while !self.current_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Ok(Stmt::Return(Expr::Temp))
    }

    fn current_token_is(&self, ttype: &TokenType) -> bool {
        if let Some(token) = &self.current_token {
            token.ttype == *ttype
        } else {
            false
        }
    }

    fn peek_token_is(&self, ttype: &TokenType) -> bool {
        if let Some(token) = &self.peek_token {
            token.ttype == *ttype
        } else {
            false
        }
    }

    fn expect_peek(&mut self, ttype: TokenType) -> Result<(), ParserError> {
        if self.peek_token_is(&ttype) {
            self.next_token();
            Ok(())
        } else {
            Err(self.peek_error(&ttype))
        }
    }

    fn peek_error(&self, ttype: &TokenType) -> ParserError {
        ParserError(format!(
            "expected next token to be {:?}, got {:?} instead",
            ttype,
            self.peek_token.as_ref().unwrap().ttype
        ))
    }
}

// TODO: More parser error information
#[derive(Debug)]
pub struct ParserError(String);
impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
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

    macro_rules! parse_program {
        ($input:ident) => {{
            let lexer = $crate::lexer::Lexer::new($input);
            let mut parser = $crate::parser::Parser::new(lexer);

            let program = parser.parse_program();
            check_parser_errors(&parser);
            program
        }};
    }

    #[test]
    fn let_stmt() {
        let input = r#"
        let x = 5;
        let y = 10;
        let foobar = 838383;
        "#;

        let program = parse_program!(input);

        assert_eq!(program.statements.len(), 3);

        let expected_identifiers = ["x", "y", "foobar"];
        for (i, statement) in program.statements.iter().enumerate() {
            match statement {
                Stmt::Let(token, identifier, _) => {
                    assert_let_statement!(token, Let, identifier, expected_identifiers[i]);
                }
                _ => panic!("expected a let statement"),
            }
        }
    }

    #[test]
    fn return_stmt() {
        let input = r#"
        return 5;
        return 10;
        return 993322;
        "#;

        let program = parse_program!(input);

        for statement in program.statements.iter() {
            match statement {
                Stmt::Return(_) => {}
                _ => panic!("expected a return statement"),
            }
        }
    }

    fn check_parser_errors(parser: &Parser) {
        if parser.errors().is_empty() {
            return;
        }

        eprintln!("Parser had {} errors", parser.errors().len());
        for error in parser.errors().iter() {
            eprintln!("parser error: {}", error)
        }
        panic!();
    }
}
