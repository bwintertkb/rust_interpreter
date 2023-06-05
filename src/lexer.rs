#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Illegal,
    EOF,
    Ident(String),
    Int(i64),
    Float(f64),
    Assign,
    Plus,
    Minus,
    Multiply,
    Divide,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Function,
    Let,
    LessThan,
    GreaterThan,
    Return,
    True,
    False,
    If,
    Else,
    Equal,
    NEqual,
}

impl Token {
    fn is_special_char(c: char) -> bool {
        c == '='
            || c == '+'
            || c == ','
            || c == ';'
            || c == '('
            || c == ')'
            || c == '{'
            || c == '}'
            || c == '*'
            || c == '/'
            || c == '!'
    }

    fn is_numeric(c: char) -> bool {
        c.is_numeric()
    }

    fn is_char(c: char) -> bool {
        !Token::is_dot(c)
            && !Token::is_special_char(c)
            && !Token::is_numeric(c)
            && !Token::is_end(c)
            && !Token::is_space(c)
    }

    fn is_dot(c: char) -> bool {
        c == '.'
    }

    fn is_end(c: char) -> bool {
        c == '\0'
    }

    fn from_special_char(c: char) -> Token {
        match c {
            '=' => Token::Assign,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Multiply,
            '/' => Token::Divide,
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '<' => Token::LessThan,
            '>' => Token::GreaterThan,
            _ => Token::Illegal,
        }
    }

    fn is_special_double_char(c_curr: char, c_next: char) -> bool {
        (c_curr == '!' || c_curr == '=') && c_next == '='
    }

    fn from_number(str_: &str) -> Token {
        if str_.contains('.') {
            let float = str_.parse::<f64>().unwrap();
            return Token::Float(float);
        }

        let int = str_.parse::<i64>().unwrap();
        Token::Int(int)
    }

    fn from_chars(str_: &str) -> Token {
        match str_ {
            "fn" => Token::Function,
            "let" => Token::Let,
            "return" => Token::Return,
            "true" => Token::True,
            "false" => Token::False,
            "if" => Token::If,
            "else" => Token::Else,
            _ => Token::Ident(str_.to_owned()),
        }
    }

    fn is_space(c: char) -> bool {
        c == ' ' || c == '\t' || c == '\n' || c == '\r'
    }
}

impl From<&str> for Token {
    fn from(s: &str) -> Self {
        match s {
            "=" => Token::Assign,
            "+" => Token::Plus,
            "-" => Token::Minus,
            "*" => Token::Multiply,
            "/" => Token::Divide,
            "," => Token::Comma,
            ";" => Token::Semicolon,
            "(" => Token::LParen,
            ")" => Token::RParen,
            "{" => Token::LBrace,
            "}" => Token::RBrace,
            "==" => Token::Equal,
            "!=" => Token::NEqual,
            "fn" => Token::Function,
            "let" => Token::Let,
            r"\O" => Token::EOF,
            _ => Token::Illegal,
        }
    }
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
        self.ch = self.next_char();
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn next_char(&mut self) -> u8 {
        let char = if self.read_position >= self.input.len() {
            0 // value of 0 means end of file
        } else {
            let mut char = 0;
            for (idx, c) in self.input.char_indices() {
                if idx != self.read_position {
                    continue;
                }
                char = c as u8;
                break;
            }
            char
        };
        char
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
        let mut char_buffer: Vec<u8> = Vec::with_capacity(self.input.len());
        while Token::is_space(self.ch as char) && !Token::is_end(self.ch as char) {
            self.read_char();
        }
        if Token::is_special_char(self.ch as char) {
            char_buffer.push(self.ch);
            if Token::is_special_double_char(self.ch as char, self.next_char() as char) {
                self.read_char();
                char_buffer.push(self.ch);
            }
            let str_ = String::from_utf8(char_buffer).unwrap();
            let token = Token::from(&str_[..]);
            self.read_char();
            return token;
        }

        // Check if it's a number, including floating point
        if Token::is_numeric(self.ch as char)
            || (Token::is_dot(self.ch as char) && Token::is_numeric(self.next_char() as char))
        {
            char_buffer.push(self.ch);
            self.read_char();
            while Token::is_numeric(self.ch as char) || Token::is_dot(self.ch as char) {
                char_buffer.push(self.ch);
                self.read_char();
            }
            let str_ = String::from_utf8(char_buffer).unwrap();
            let token = Token::from_number(&str_[..]);
            return token;
        }

        // Check non-numeric or special characters
        if Token::is_char(self.ch as char) {
            char_buffer.push(self.ch);
            self.read_char();
            while !Token::is_space(self.ch as char)
                && !Token::is_special_char(self.ch as char)
                && !Token::is_end(self.ch as char)
            {
                char_buffer.push(self.ch);
                self.read_char();
            }
            let str_ = String::from_utf8(char_buffer).unwrap();
            let token = Token::from_chars(&str_[..]);
            return token;
        }

        if Token::is_end(self.ch as char) {
            return Token::EOF;
        }
        Token::Illegal
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    //     #[test]
    //     fn test_next_token_special_chars() {
    //         let input = "=+(){},;";
    //
    //         let expected = [
    //             Token::Assign,
    //             Token::Plus,
    //             Token::LParen,
    //             Token::RParen,
    //             Token::LBrace,
    //             Token::RBrace,
    //             Token::Comma,
    //             Token::Semicolon,
    //             Token::EOF,
    //         ];
    //
    //         let mut lexer = Lexer::new(input.to_owned());
    //         assert_eq!(lexer.tokens(), expected);
    //     }
    //
    //     #[test]
    //     fn test_next_token_integer() {
    //         let input = "89182";
    //
    //         let expected = [Token::Int(89182), Token::EOF];
    //
    //         let mut lexer = Lexer::new(input.to_owned());
    //         assert_eq!(lexer.tokens(), expected);
    //     }
    //
    //     #[test]
    //     fn test_next_token_float() {
    //         let input = "8918.2";
    //
    //         let expected = [Token::Float(8918.2), Token::EOF];
    //
    //         let mut lexer = Lexer::new(input.to_owned());
    //         assert_eq!(lexer.tokens(), expected);
    //     }
    //
    //     #[test]
    //     fn test_next_token_fn_keyword() {
    //         let input = "fn";
    //
    //         let expected = [Token::Function, Token::EOF];
    //
    //         let mut lexer = Lexer::new(input.to_owned());
    //         assert_eq!(lexer.tokens(), expected);
    //     }
    //
    //     #[test]
    //     fn test_next_token_let_keyword() {
    //         let input = "let";
    //
    //         let expected = [Token::Let, Token::EOF];
    //
    //         let mut lexer = Lexer::new(input.to_owned());
    //         assert_eq!(lexer.tokens(), expected);
    //     }
    //
    //     #[test]
    //     fn test_next_token_variable_name_keyword() {
    //         let input = "my_var87a";
    //
    //         let expected = [Token::Ident("my_var87a".to_owned()), Token::EOF];
    //
    //         let mut lexer = Lexer::new(input.to_owned());
    //         assert_eq!(lexer.tokens(), expected);
    //     }
    //
    //     #[test]
    //     fn test_next_token_variable_assignment() {
    //         let input = "let my_var = 8.2;";
    //         let expected = [
    //             Token::Let,
    //             Token::Ident("my_var".to_owned()),
    //             Token::Assign,
    //             Token::Float(8.2),
    //             Token::Semicolon,
    //             Token::EOF,
    //         ];
    //         let mut lexer = Lexer::new(input.to_owned());
    //         assert_eq!(lexer.tokens(), expected);
    //     }
    //
    //     #[test]
    //     fn test_function_declaration() {
    //         let input = "let add = fn(x, y) {
    //     x + y;
    //
    // };";
    //         let expected = [
    //             Token::Let,
    //             Token::Ident("add".to_owned()),
    //             Token::Assign,
    //             Token::Function,
    //             Token::LParen,
    //             Token::Ident("x".to_owned()),
    //             Token::Comma,
    //             Token::Ident("y".to_owned()),
    //             Token::RParen,
    //             Token::LBrace,
    //             Token::Ident("x".to_owned()),
    //             Token::Plus,
    //             Token::Ident("y".to_owned()),
    //             Token::Semicolon,
    //             Token::RBrace,
    //             Token::Semicolon,
    //             Token::EOF,
    //         ];
    //         let mut lexer = Lexer::new(input.to_owned());
    //         assert_eq!(lexer.tokens(), expected);
    //     }
    //
    //     #[test]
    //     fn test_function_call() {
    //         let input = "let result =add(five, ten);";
    //         let expected = [
    //             Token::Let,
    //             Token::Ident("result".to_owned()),
    //             Token::Assign,
    //             Token::Ident("add".to_owned()),
    //             Token::LParen,
    //             Token::Ident("five".to_owned()),
    //             Token::Comma,
    //             Token::Ident("ten".to_owned()),
    //             Token::RParen,
    //             Token::Semicolon,
    //             Token::EOF,
    //         ];
    //
    //         let mut lexer = Lexer::new(input.to_owned());
    //         assert_eq!(lexer.tokens(), expected);
    //     }
    //
    //     #[test]
    //     fn test_mini_sequence() {
    //         let input = "
    // let five = 5;
    // let ten = 10;
    // let add = fn(x, y) { x + y; };
    // let result = add(five, ten);
    // ";
    //         let expected = [
    //             Token::Let,
    //             Token::Ident("five".to_owned()),
    //             Token::Assign,
    //             Token::Int(5),
    //             Token::Semicolon,
    //             Token::Let,
    //             Token::Ident("ten".to_owned()),
    //             Token::Assign,
    //             Token::Int(10),
    //             Token::Semicolon,
    //             Token::Let,
    //             Token::Ident("add".to_owned()),
    //             Token::Assign,
    //             Token::Function,
    //             Token::LParen,
    //             Token::Ident("x".to_owned()),
    //             Token::Comma,
    //             Token::Ident("y".to_owned()),
    //             Token::RParen,
    //             Token::LBrace,
    //             Token::Ident("x".to_owned()),
    //             Token::Plus,
    //             Token::Ident("y".to_owned()),
    //             Token::Semicolon,
    //             Token::RBrace,
    //             Token::Semicolon,
    //             Token::Let,
    //             Token::Ident("result".to_owned()),
    //             Token::Assign,
    //             Token::Ident("add".to_owned()),
    //             Token::LParen,
    //             Token::Ident("five".to_owned()),
    //             Token::Comma,
    //             Token::Ident("ten".to_owned()),
    //             Token::RParen,
    //             Token::Semicolon,
    //             Token::EOF,
    //         ];
    //         let mut lexer = Lexer::new(input.to_owned());
    //         assert_eq!(lexer.tokens(), expected);
    //     }

    #[test]
    fn test_not_equal() {
        let input = "!=";

        let expected = [Token::NEqual, Token::EOF];

        let mut lexer = Lexer::new(input.to_owned());
        assert_eq!(lexer.tokens(), expected);
    }
}
