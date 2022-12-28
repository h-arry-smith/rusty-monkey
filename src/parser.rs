use std::{error::Error, fmt::Display};

use crate::{
    ast::*,
    lexer::Lexer,
    token::{Token, TokenType},
};

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

impl Precedence {
    fn from_token_type(ttype: &TokenType) -> Precedence {
        match ttype {
            TokenType::Eq => Precedence::Equals,
            TokenType::NotEq => Precedence::Equals,
            TokenType::LessThan => Precedence::LessGreater,
            TokenType::GreaterThan => Precedence::LessGreater,
            TokenType::Plus => Precedence::Sum,
            TokenType::Minus => Self::Sum,
            TokenType::Slash => Precedence::Product,
            TokenType::Asterisk => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }
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
    pub fn parse_program(&mut self) -> Program {
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
        let mut left = self.parse_prefix_expression()?;

        while !self.peek_token_is(&TokenType::Semicolon) && precedence < self.peek_precedence() {
            self.next_token();
            left = self.parse_infix_expression(left)?;
        }

        Ok(left)
    }

    fn parse_prefix_expression(&mut self) -> Result<Expr, ParserError> {
        match self.current_token.ttype {
            TokenType::Ident => self.parse_ident_expr(),
            TokenType::Int => self.parse_integer_literal(),
            TokenType::Bang => self.parse_prefix_expr(),
            TokenType::Minus => self.parse_prefix_expr(),
            TokenType::True => self.parse_boolean(),
            TokenType::False => self.parse_boolean(),
            _ => Err(ParserError(format!(
                "no prefix function for expression: {:?}",
                self.current_token
            ))),
        }
    }

    fn parse_infix_expression(&mut self, left: Expr) -> Result<Expr, ParserError> {
        match self.current_token.ttype {
            TokenType::Plus => self.parse_infix_expr(left),
            TokenType::Minus => self.parse_infix_expr(left),
            TokenType::Slash => self.parse_infix_expr(left),
            TokenType::Asterisk => self.parse_infix_expr(left),
            TokenType::Eq => self.parse_infix_expr(left),
            TokenType::NotEq => self.parse_infix_expr(left),
            TokenType::LessThan => self.parse_infix_expr(left),
            TokenType::GreaterThan => self.parse_infix_expr(left),
            _ => Ok(left),
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

    fn parse_infix_expr(&mut self, left: Expr) -> Result<Expr, ParserError> {
        let operator = self.current_token.literal.clone();
        let precedence = self.current_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;

        Ok(Expr::Infix(Box::new(left), operator, Box::new(right)))
    }

    fn parse_boolean(&mut self) -> Result<Expr, ParserError> {
        Ok(Expr::Boolean(self.current_token_is(&TokenType::True)))
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

    fn current_precedence(&self) -> Precedence {
        Precedence::from_token_type(&self.current_token.ttype)
    }

    fn peek_precedence(&self) -> Precedence {
        Precedence::from_token_type(&self.peek_token.ttype)
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
            match $expr {
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
            Stmt::Expr(expr) => assert_int_literal!(*expr, 5),
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
                        assert_int_literal!(**right, literal)
                    }
                    _ => panic!("not a prefix expression"),
                },
                _ => panic!("not an expression"),
            }
        }
    }

    #[test]
    fn infix_operators() {
        let tests = [
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 == 5;", 5, "==", 5),
            ("5 != 5;", 5, "!=", 5),
        ];

        for (input, left_literal, operator, right_literal) in tests {
            let program = parse_program!(input);

            let stmt = program.statements.first().expect("should have a statement");

            match stmt {
                Stmt::Expr(expr) => match expr {
                    Expr::Infix(left, op, right) => {
                        assert_int_literal!(**left, left_literal);
                        assert_eq!(op, operator);
                        assert_int_literal!(**right, right_literal);
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
