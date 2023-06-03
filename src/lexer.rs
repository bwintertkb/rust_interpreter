#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Token {
    Illegal,
    EOF,
    Ident(String),
    Int(i64),
    Assign,
    Plus,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Function,
    Let,
}

#[derive(Debug, Default, Clone)]
pub struct Lexer {
    input: String,
    position: usize,      // current position in input (index of current char)
    read_position: usize, // current reading position in input (after current char, position + 1)
    ch: u8,               // current char under examination
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Self {
            input,
            position: 0,
            read_position: 0,
            ch: 0,
        };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0 // value of 0 means end of file
        } else {
            for (idx, c) in self.input.char_indices() {
                if idx != self.read_position {
                    continue;
                }
                self.ch = c as u8;
                break;
            }
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn tokens(&mut self) -> Vec<Token> {
        let mut token = self.next_token();
        let mut tokens = vec![token];

        while tokens.last().is_some() && *tokens.last().unwrap() != Token::EOF {
            token = self.next_token();
            tokens.push(token);
        }
        tokens
    }

    fn next_token(&mut self) -> Token {
        Token::EOF
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = "=*(){},;";

        let expected = [
            Token::Assign,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::Semicolon,
            Token::EOF,
        ];

        let mut lexer = Lexer::new(input.to_owned());
        println!("{:?}", lexer);

        let mut tokens: Vec<Token> = Vec::new();

        assert_eq!(lexer.tokens(), expected);
    }
}
