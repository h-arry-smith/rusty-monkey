use crate::token::Token;

#[derive(Debug)]
pub struct Identifier(pub String);

#[derive(Debug)]
pub enum Stmt {
    Let(Token, Identifier, Expr),
    Return(Expr),
    Expr(Expr),
}

#[derive(Debug)]
pub enum Expr {
    Temp,
    Identifier(String),
    IntegerLiteral(i32),
    Prefix(String, Box<Expr>),
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Stmt>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            statements: Vec::new(),
        }
    }
}
