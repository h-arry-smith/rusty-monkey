use std::{collections::HashMap, error::Error, fmt::Display};

use crate::{
    ast::*,
    lexer::Lexer,
    token::{Token, TokenType},
};

type PrefixParseFn = dyn Fn(&mut Parser) -> Expr;
type InfixParseFn = dyn Fn(&mut Parser, Expr) -> Expr;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

pub struct Parser<'src> {
    lexer: Lexer<'src>,
    current_token: Token,
    peek_token: Token,
    errors: Vec<ParserError>,
}

impl<'src> Parser<'src> {
    pub fn new(lexer: Lexer<'src>) -> Self {
        let mut parser = Self {
            lexer,
            current_token: Token::new(TokenType::EOF, "".to_string()),
            peek_token: Token::new(TokenType::EOF, "".to_string()),
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
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    // TODO: Propogate parser error
    fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        loop {
            if self.current_token.ttype == TokenType::EOF {
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
        match self.current_token.ttype {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Stmt, ParserError> {
        let token = self.current_token.clone();
        self.expect_peek(TokenType::Ident)?;
        let name = Identifier(self.current_token.literal.clone());

        self.expect_peek(TokenType::Assign)?;

        // TODO: We're skipping the expressions until we encounter a semicolon
        while !self.current_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Ok(Stmt::Let(token, name, Expr::Temp))
    }

    fn parse_return_statement(&mut self) -> Result<Stmt, ParserError> {
        self.next_token();

        // TODO: We're skipping the expressions until we encounter a semicolon
        while !self.current_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Ok(Stmt::Return(Expr::Temp))
    }

    fn parse_expression_statement(&mut self) -> Result<Stmt, ParserError> {
        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Ok(Stmt::Expr(expression))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expr, ParserError> {
        match self.current_token.ttype {
            TokenType::Ident => self.parse_ident_expr(),
            TokenType::Int => self.parse_integer_literal(),
            TokenType::Bang => self.parse_prefix_expr(),
            TokenType::Minus => self.parse_prefix_expr(),
            _ => Err(ParserError("no prefix function for expression".to_string())),
        }
    }

    fn parse_ident_expr(&mut self) -> Result<Expr, ParserError> {
        Ok(Expr::Identifier(self.current_token.literal.clone()))
    }

    fn parse_integer_literal(&mut self) -> Result<Expr, ParserError> {
        match self.current_token.literal.parse::<i32>() {
            Ok(integer) => Ok(Expr::IntegerLiteral(integer)),
            Err(_) => Err(ParserError(format!(
                "could not parse {:?} as integer",
                self.current_token
            ))),
        }
    }

    fn parse_prefix_expr(&mut self) -> Result<Expr, ParserError> {
        let operator = self.current_token.literal.clone();
        self.next_token();
        let right = self.parse_expression(Precedence::Prefix)?;

        Ok(Expr::Prefix(operator, Box::new(right)))
    }

    fn current_token_is(&self, ttype: &TokenType) -> bool {
        self.current_token.ttype == *ttype
    }

    fn peek_token_is(&self, ttype: &TokenType) -> bool {
        self.peek_token.ttype == *ttype
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
            ttype, self.peek_token.ttype
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
    use crate::{ast::*, parser::Parser};

    macro_rules! assert_let_statement {
        ($token:ident, $ttype:ident, $identifier:ident, $expected:expr) => {
            assert_eq!($token.ttype, $crate::token::TokenType::$ttype);
            assert_eq!($identifier.0, $expected);
        };
    }

    macro_rules! assert_int_literal {
        ($expr:expr, $expected:expr) => {{
            match **$expr {
                $crate::ast::Expr::IntegerLiteral(int) => assert_eq!(int, $expected),
                _ => panic!("not an int literal"),
            }
        }};
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

    #[test]
    fn ident_expr() {
        let input = "foobar;";

        let program = parse_program!(input);

        let stmt = program
            .statements
            .first()
            .expect("should have on statement");
        match stmt {
            Stmt::Expr(expr) => match expr {
                Expr::Identifier(ident) => assert_eq!(ident, "foobar"),
                _ => panic!("not an identifier expression"),
            },
            _ => panic!("not an expression"),
        }
    }

    #[test]
    fn int_literal_expr() {
        let input = "5;";

        let program = parse_program!(input);

        let stmt = program
            .statements
            .first()
            .expect("should have on statement");
        match stmt {
            Stmt::Expr(expr) => match expr {
                Expr::IntegerLiteral(integer) => assert_eq!(*integer, 5),
                _ => panic!("not an identifier expression"),
            },
            _ => panic!("not an expression"),
        }
    }

    #[test]
    fn prefix_operators() {
        let tests = [("!5;", "!", 5), ("-15;", "-", 15)];

        for (input, operator, literal) in tests {
            let program = parse_program!(input);

            let stmt = program.statements.first().expect("should have a statement");

            match stmt {
                Stmt::Expr(expr) => match expr {
                    Expr::Prefix(op, right) => {
                        assert_eq!(op, operator);
                        assert_int_literal!(right, literal)
                    }
                    _ => panic!("not a prefix expression"),
                },
                _ => panic!("not an expression"),
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
