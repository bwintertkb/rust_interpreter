use serde::{de::Visitor, Deserialize, Serialize};

const TOKEN_NAME: &str = "Token";

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
    Not,
}

impl Token {
    pub fn as_bytes(&self) -> &[u8] {
        let bytes = unsafe {
            ::core::slice::from_raw_parts(
                (self as *const Token) as *const u8,
                ::core::mem::size_of::<Token>(),
            )
        };
        bytes
    }

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
            || c == '-'
            || c == '<'
            || c == '>'
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
            '!' => Token::Not,
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
            "!" => Token::Not,
            "<" => Token::LessThan,
            ">" => Token::GreaterThan,
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

    pub fn next_token(&mut self) -> Token {
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

impl Serialize for Token {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // https://serde.rs/impl-serialize.html
        match *self {
            Token::Illegal => serializer.serialize_unit_variant(TOKEN_NAME, 0, "Illegal"),
            Token::EOF => serializer.serialize_unit_variant(TOKEN_NAME, 1, "EOF"),
            Token::Ident(ref s) => serializer.serialize_newtype_variant(TOKEN_NAME, 2, "Ident", s),
            Token::Int(ref v) => serializer.serialize_newtype_variant(TOKEN_NAME, 3, "Int", v),
            Token::Float(ref v) => serializer.serialize_newtype_variant(TOKEN_NAME, 4, "Float", v),
            Token::Assign => serializer.serialize_unit_variant(TOKEN_NAME, 5, "Assign"),
            Token::Plus => serializer.serialize_unit_variant(TOKEN_NAME, 6, "Plus"),
            Token::Minus => serializer.serialize_unit_variant(TOKEN_NAME, 7, "Minus"),
            Token::Multiply => serializer.serialize_unit_variant(TOKEN_NAME, 8, "Multiply"),
            Token::Divide => serializer.serialize_unit_variant(TOKEN_NAME, 9, "Divide"),
            Token::Comma => serializer.serialize_unit_variant(TOKEN_NAME, 10, "Comma"),
            Token::Semicolon => serializer.serialize_unit_variant(TOKEN_NAME, 11, "Semicolon"),
            Token::LParen => serializer.serialize_unit_variant(TOKEN_NAME, 12, "LParen"),
            Token::RParen => serializer.serialize_unit_variant(TOKEN_NAME, 13, "RParen"),
            Token::LBrace => serializer.serialize_unit_variant(TOKEN_NAME, 14, "LBrace"),
            Token::RBrace => serializer.serialize_unit_variant(TOKEN_NAME, 15, "RBrace"),
            Token::Function => serializer.serialize_unit_variant(TOKEN_NAME, 16, "Function"),
            Token::Let => serializer.serialize_unit_variant(TOKEN_NAME, 17, "Let"),
            Token::LessThan => serializer.serialize_unit_variant(TOKEN_NAME, 18, "LessThan"),
            Token::GreaterThan => serializer.serialize_unit_variant(TOKEN_NAME, 19, "GreaterThan"),
            Token::Return => serializer.serialize_unit_variant(TOKEN_NAME, 20, "Return"),
            Token::True => serializer.serialize_unit_variant(TOKEN_NAME, 21, "True"),
            Token::False => serializer.serialize_unit_variant(TOKEN_NAME, 22, "False"),
            Token::If => serializer.serialize_unit_variant(TOKEN_NAME, 23, "If"),
            Token::Else => serializer.serialize_unit_variant(TOKEN_NAME, 24, "Else"),
            Token::Equal => serializer.serialize_unit_variant(TOKEN_NAME, 25, "Equal"),
            Token::NEqual => serializer.serialize_unit_variant(TOKEN_NAME, 26, "NEqual"),
            Token::Not => serializer.serialize_unit_variant(TOKEN_NAME, 27, "Not"),
        }
    }
}

struct TokenVisitor;

impl<'de> Visitor<'de> for TokenVisitor {
    type Value = Token;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("Unexpected token")
    }
}

// impl Deserialize for Token {
//     fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
//     where
//         D: serde::Deserializer<'de>,
//     {
//         deserializer.deserialize_enum
//     }
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token_special_chars() {
        let input = "=+(){},;";

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
        assert_eq!(lexer.tokens(), expected);
    }

    #[test]
    fn test_next_token_integer() {
        let input = "89182";

        let expected = [Token::Int(89182), Token::EOF];

        let mut lexer = Lexer::new(input.to_owned());
        assert_eq!(lexer.tokens(), expected);
    }

    #[test]
    fn test_next_token_float() {
        let input = "8918.2";

        let expected = [Token::Float(8918.2), Token::EOF];

        let mut lexer = Lexer::new(input.to_owned());
        assert_eq!(lexer.tokens(), expected);
    }

    #[test]
    fn test_next_token_fn_keyword() {
        let input = "fn";

        let expected = [Token::Function, Token::EOF];

        let mut lexer = Lexer::new(input.to_owned());
        assert_eq!(lexer.tokens(), expected);
    }

    #[test]
    fn test_next_token_let_keyword() {
        let input = "let";

        let expected = [Token::Let, Token::EOF];

        let mut lexer = Lexer::new(input.to_owned());
        assert_eq!(lexer.tokens(), expected);
    }

    #[test]
    fn test_next_token_variable_name_keyword() {
        let input = "my_var87a";

        let expected = [Token::Ident("my_var87a".to_owned()), Token::EOF];

        let mut lexer = Lexer::new(input.to_owned());
        assert_eq!(lexer.tokens(), expected);
    }

    #[test]
    fn test_next_token_variable_assignment() {
        let input = "let my_var = 8.2;";
        let expected = [
            Token::Let,
            Token::Ident("my_var".to_owned()),
            Token::Assign,
            Token::Float(8.2),
            Token::Semicolon,
            Token::EOF,
        ];
        let mut lexer = Lexer::new(input.to_owned());
        assert_eq!(lexer.tokens(), expected);
    }

    #[test]
    fn test_function_declaration() {
        let input = "let add = fn(x, y) {
        x + y;
    
    };";
        let expected = [
            Token::Let,
            Token::Ident("add".to_owned()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".to_owned()),
            Token::Comma,
            Token::Ident("y".to_owned()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".to_owned()),
            Token::Plus,
            Token::Ident("y".to_owned()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::EOF,
        ];
        let mut lexer = Lexer::new(input.to_owned());
        assert_eq!(lexer.tokens(), expected);
    }

    #[test]
    fn test_function_call() {
        let input = "let result =add(five, ten);";
        let expected = [
            Token::Let,
            Token::Ident("result".to_owned()),
            Token::Assign,
            Token::Ident("add".to_owned()),
            Token::LParen,
            Token::Ident("five".to_owned()),
            Token::Comma,
            Token::Ident("ten".to_owned()),
            Token::RParen,
            Token::Semicolon,
            Token::EOF,
        ];

        let mut lexer = Lexer::new(input.to_owned());
        assert_eq!(lexer.tokens(), expected);
    }

    #[test]
    fn test_mini_sequence() {
        let input = "
    let five = 5;
    let ten = 10;
    let add = fn(x, y) { x + y; };
    let result = add(five, ten);
    ";
        let expected = [
            Token::Let,
            Token::Ident("five".to_owned()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_owned()),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_owned()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".to_owned()),
            Token::Comma,
            Token::Ident("y".to_owned()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".to_owned()),
            Token::Plus,
            Token::Ident("y".to_owned()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_owned()),
            Token::Assign,
            Token::Ident("add".to_owned()),
            Token::LParen,
            Token::Ident("five".to_owned()),
            Token::Comma,
            Token::Ident("ten".to_owned()),
            Token::RParen,
            Token::Semicolon,
            Token::EOF,
        ];
        let mut lexer = Lexer::new(input.to_owned());
        assert_eq!(lexer.tokens(), expected);
    }

    #[test]
    fn test_not_equal() {
        let input = "!=";

        let expected = [Token::NEqual, Token::EOF];

        let mut lexer = Lexer::new(input.to_owned());
        assert_eq!(lexer.tokens(), expected);
    }

    #[test]
    fn test_full_special_characters() {
        let input = "
let five = 5; let ten = 10; let add = fn(x, y) { x + y; }; let result = add(five, ten); !-/*5; 5 < 10 > 5;
";
        let expected = [
            Token::Let,
            Token::Ident("five".to_owned()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_owned()),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_owned()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".to_owned()),
            Token::Comma,
            Token::Ident("y".to_owned()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".to_owned()),
            Token::Plus,
            Token::Ident("y".to_owned()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_owned()),
            Token::Assign,
            Token::Ident("add".to_owned()),
            Token::LParen,
            Token::Ident("five".to_owned()),
            Token::Comma,
            Token::Ident("ten".to_owned()),
            Token::RParen,
            Token::Semicolon,
            Token::Not,
            Token::Minus,
            Token::Divide,
            Token::Multiply,
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::LessThan,
            Token::Int(10),
            Token::GreaterThan,
            Token::Int(5),
            Token::Semicolon,
            Token::EOF,
        ];
        let mut lexer = Lexer::new(input.to_owned());
        assert_eq!(lexer.tokens(), expected);
    }

    #[test]
    fn test_extra_keywords() {
        let input = "
if (5 < 10) { return true; } else { return false; } 10 == 10; 10 != 9;

";
        let expected = [
            Token::If,
            Token::LParen,
            Token::Int(5),
            Token::LessThan,
            Token::Int(10),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            Token::Int(10),
            Token::Equal,
            Token::Int(10),
            Token::Semicolon,
            Token::Int(10),
            Token::NEqual,
            Token::Int(9),
            Token::Semicolon,
            Token::EOF,
        ];
        let mut lexer = Lexer::new(input.to_owned());
        assert_eq!(lexer.tokens(), expected);
    }
}
