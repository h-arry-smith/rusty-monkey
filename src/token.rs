pub struct Token {
    ttype: TokenType,
    literal: String,
}

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
