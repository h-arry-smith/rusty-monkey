use crate::token::Token;

#[derive(Debug)]
pub struct Identifier(pub String);

#[derive(Debug)]
pub enum Stmt {
    Let(Token, Identifier, Expr),
    Return(Expr),
}

#[derive(Debug)]
pub enum Expr {
    Temp,
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
