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
        self.skip_whitespace();

        let token = match self.ch {
            b'=' => new_token(TokenType::Assign, "="),
            b';' => new_token(TokenType::Semicolon, ";"),
            b'(' => new_token(TokenType::LParen, "("),
            b')' => new_token(TokenType::RParen, ")"),
            b',' => new_token(TokenType::Comma, ","),
            b'+' => new_token(TokenType::Plus, "+"),
            b'{' => new_token(TokenType::LBrace, "{"),
            b'}' => new_token(TokenType::RBrace, "}"),
            0 => new_token(TokenType::EOF, ""),
            _ => {
                if is_letter(&self.ch) {
                    let identifier = self.read_identifier();
                    let ttype = TokenType::from_identifier(&identifier);

                    return Token::new(ttype, identifier);
                } else if self.ch.is_ascii_digit() {
                    let number = self.read_number();

                    return Token::new(TokenType::Int, number);
                } else {
                    new_token(TokenType::Illegal, "")
                }
            }
        };

        self.read_char();
        token
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> String {
        let start = self.position;
        while is_letter(&self.ch) {
            self.read_char();
        }

        self.string_from_input(start)
    }

    fn read_number(&mut self) -> String {
        let start = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }

        self.string_from_input(start)
    }

    fn string_from_input(&self, start: usize) -> String {
        String::from_utf8(self.input[start..self.position].to_owned())
            .expect("could not convert to string")
    }
}

fn new_token(ttype: TokenType, literal: &str) -> Token {
    Token::new(ttype, literal.to_owned())
}

fn is_letter(ch: &u8) -> bool {
    ch.is_ascii_alphabetic() || *ch == b'_'
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

    #[test]
    fn multiline_code() {
        let input = r#"let five = 5;
        let ten = 10;
        
        let add = fn(x, y) {
            x + y;
        };
        
        let result = add(five, ten);"#;

        let expected = [
            (TokenType::Let, "let"),
            (TokenType::Ident, "five"),
            (TokenType::Assign, "="),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "ten"),
            (TokenType::Assign, "="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "add"),
            (TokenType::Assign, "="),
            (TokenType::Function, "fn"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "x"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "y"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Ident, "x"),
            (TokenType::Plus, "+"),
            (TokenType::Ident, "y"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "result"),
            (TokenType::Assign, "="),
            (TokenType::Ident, "add"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "five"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "ten"),
            (TokenType::RParen, ")"),
            (TokenType::Semicolon, ";"),
            (TokenType::EOF, ""),
        ];

        let mut lexer = Lexer::new(input);

        for (ttype, literal) in expected {
            let token = lexer.next_token();

            assert_eq!(token.ttype, ttype);
            assert_eq!(token.literal, literal);
        }
    }
}
