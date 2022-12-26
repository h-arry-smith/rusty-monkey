#[derive(Debug)]
pub struct Token {
    pub ttype: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(ttype: TokenType, literal: String) -> Self {
        Self { ttype, literal }
    }
}

#[derive(Debug, PartialEq)]
pub enum TokenType {
    Illegal,
    EOF,

    // Identifiers + Literals
    Ident,
    Int,

    // Operators
    Assign,
    Plus,

    // Delimiters
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,

    // Keywods
    Function,
    Let,
}
