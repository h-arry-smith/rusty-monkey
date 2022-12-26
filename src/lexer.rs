use crate::token::{Token, TokenType};

#[derive(Debug)]
struct Lexer<'src> {
    input: &'src [u8],
    position: usize,
    read_position: usize,
    ch: u8,
}

impl<'src> Lexer<'src> {
    pub fn new(input: &'src str) -> Self {
        let mut lexer = Self {
            input: input.as_bytes(),
            position: 0,
            read_position: 0,
            ch: 0,
        };
        lexer.read_char();

        lexer
    }

    // TODO: Implement unicode support
    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position]
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        let token = match self.ch {
            b'=' => Lexer::new_token(TokenType::Assign, "="),
            b';' => Lexer::new_token(TokenType::Semicolon, ";"),
            b'(' => Lexer::new_token(TokenType::LParen, "("),
            b')' => Lexer::new_token(TokenType::RParen, ")"),
            b',' => Lexer::new_token(TokenType::Comma, ","),
            b'+' => Lexer::new_token(TokenType::Plus, "+"),
            b'{' => Lexer::new_token(TokenType::LBrace, "{"),
            b'}' => Lexer::new_token(TokenType::RBrace, "}"),
            0 => Lexer::new_token(TokenType::EOF, ""),
            _ => Lexer::new_token(TokenType::Illegal, ""),
        };

        self.read_char();
        token
    }

    fn new_token(ttype: TokenType, literal: &str) -> Token {
        Token::new(ttype, literal.to_owned())
    }
}

#[cfg(test)]
mod tests {
    use crate::token::TokenType;

    use super::Lexer;

    #[test]
    fn test_next_token() {
        let input = "=+(){},;";
        let expected = [
            (TokenType::Assign, "="),
            (TokenType::Plus, "+"),
            (TokenType::LParen, "("),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::RBrace, "}"),
            (TokenType::Comma, ","),
            (TokenType::Semicolon, ";"),
        ];

        let mut lexer = Lexer::new(input);

        for (ttype, literal) in expected {
            let token = lexer.next_token();

            assert_eq!(token.ttype, ttype);
            assert_eq!(token.literal, literal);
        }
    }
}
