use serde::{
    de::{VariantAccess, Visitor},
    Deserialize, Serialize,
};

const TOKEN_NAME: &str = "Token";

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Token {
    Illegal,
    EOF,
    Ident(String),
    Int(i64),
    Assign,
    Plus,
    Minus,
    Asterisk,
    Slash,
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
    Bang,
    String(String),
    LBracket,
    RBracket,
    Colon,
}

impl Token {
    pub fn literal(&self) -> String {
        // Token/value literal
        match self {
            Token::Illegal => "illegal".to_owned(),
            Token::EOF => "eof".to_owned(),
            Token::Ident(v) => v.to_owned(),
            Token::Int(v) => v.to_string(),
            Token::Assign => "=".to_owned(),
            Token::Plus => "+".to_owned(),
            Token::Minus => "-".to_owned(),
            Token::Asterisk => "*".to_owned(),
            Token::Slash => "/".to_owned(),
            Token::Comma => ",".to_owned(),
            Token::Semicolon => ";".to_owned(),
            Token::LParen => "(".to_owned(),
            Token::RParen => ")".to_owned(),
            Token::LBrace => "{".to_owned(),
            Token::RBrace => "}".to_owned(),
            Token::Function => "fn".to_owned(),
            Token::Let => "let".to_owned(),
            Token::LessThan => "<".to_owned(),
            Token::GreaterThan => ">".to_owned(),
            Token::Return => "return".to_owned(),
            Token::True => "true".to_owned(),
            Token::False => "false".to_owned(),
            Token::If => "if".to_owned(),
            Token::Else => "else".to_owned(),
            Token::Equal => "==".to_owned(),
            Token::NEqual => "!=".to_owned(),
            Token::Bang => "!".to_owned(),
            Token::String(string) => string.clone(),
            Token::LBracket => "[".to_owned(),
            Token::RBracket => "]".to_owned(),
            Token::Colon => ":".to_owned(),
        }
    }

    pub fn token_literal(&self) -> String {
        // Only token literal
        match self {
            Token::Ident(_) => "ident".to_owned(),
            Token::Int(_) => "int".to_owned(),
            Token::String(_) => "string".to_owned(),
            _ => self.literal(),
        }
    }

    fn from_str(str_: &str) -> Token {
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
        if self.read_position >= self.input.len() {
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
        }
    }

    pub fn tokens(&mut self) -> Vec<Token> {
        let mut token = self.next_token();
        let mut tokens = vec![token];

        while tokens.last().is_some() && *tokens.last().unwrap() != Token::EOF {
            token = self.next_token();
            tokens.push(token);
        }
        tokens
    }

    pub fn next_token(&mut self) -> Token {
        while let ' ' | '\t' | '\n' | '\r' = self.ch as char {
            self.read_char();
        }
        let char = self.ch as char;
        match char {
            '=' => {
                self.read_char();
                if self.ch == b'=' {
                    self.read_char();
                    return Token::Equal;
                }
                Token::Assign
            }
            '+' => {
                self.read_char();
                Token::Plus
            }
            '-' => {
                self.read_char();
                Token::Minus
            }
            '*' => {
                self.read_char();
                Token::Asterisk
            }
            '/' => {
                self.read_char();
                Token::Slash
            }
            ',' => {
                self.read_char();
                Token::Comma
            }
            ';' => {
                self.read_char();
                Token::Semicolon
            }
            ':' => {
                self.read_char();
                Token::Colon
            }
            '(' => {
                self.read_char();
                Token::LParen
            }
            ')' => {
                self.read_char();
                Token::RParen
            }
            '{' => {
                self.read_char();
                Token::LBrace
            }
            '}' => {
                self.read_char();
                Token::RBrace
            }
            '[' => {
                self.read_char();
                Token::LBracket
            }
            ']' => {
                self.read_char();
                Token::RBracket
            }
            '!' => {
                self.read_char();
                if self.ch == b'=' {
                    self.read_char();
                    return Token::NEqual;
                }
                Token::Bang
            }
            '<' => {
                self.read_char();
                Token::LessThan
            }
            '>' => {
                self.read_char();
                Token::GreaterThan
            }
            '"' | '\'' => {
                self.read_char();
                let mut buffer = String::default();
                while self.ch != b'"' && self.ch != b'\'' {
                    buffer.push(self.ch as char);
                    self.read_char();
                }

                self.read_char();
                Token::String(buffer)
            }
            '\0' => Token::EOF,
            '0'..='9' => {
                let mut buffer = String::default();
                buffer.push(char);
                self.read_char();
                while (self.ch as char).is_numeric() {
                    buffer.push(self.ch as char);
                    self.read_char();
                }
                Token::Int(buffer.parse::<i64>().unwrap())
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut buffer = String::default();
                buffer.push(char);
                self.read_char();
                while (self.ch as char).is_alphanumeric() || self.ch == b'_' {
                    buffer.push(self.ch as char);
                    self.read_char();
                }
                Token::from_str(&buffer)
            }
            _ => Token::Illegal,
        }
    }
}

impl Serialize for Token {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        println!("serializing token");
        // https://serde.rs/impl-serialize.html
        match *self {
            Token::Illegal => serializer.serialize_unit_variant(TOKEN_NAME, 0, "Illegal"),
            Token::EOF => serializer.serialize_unit_variant(TOKEN_NAME, 1, "EOF"),
            Token::Ident(ref s) => serializer.serialize_newtype_variant(TOKEN_NAME, 2, "Ident", s),
            Token::Int(ref v) => serializer.serialize_newtype_variant(TOKEN_NAME, 3, "Int", v),
            Token::Assign => serializer.serialize_unit_variant(TOKEN_NAME, 4, "Assign"),
            Token::Plus => serializer.serialize_unit_variant(TOKEN_NAME, 5, "Plus"),
            Token::Minus => serializer.serialize_unit_variant(TOKEN_NAME, 6, "Minus"),
            Token::Asterisk => serializer.serialize_unit_variant(TOKEN_NAME, 7, "Multiply"),
            Token::Slash => serializer.serialize_unit_variant(TOKEN_NAME, 8, "Divide"),
            Token::Comma => serializer.serialize_unit_variant(TOKEN_NAME, 9, "Comma"),
            Token::Semicolon => serializer.serialize_unit_variant(TOKEN_NAME, 10, "Semicolon"),
            Token::LParen => serializer.serialize_unit_variant(TOKEN_NAME, 11, "LParen"),
            Token::RParen => serializer.serialize_unit_variant(TOKEN_NAME, 12, "RParen"),
            Token::LBrace => serializer.serialize_unit_variant(TOKEN_NAME, 13, "LBrace"),
            Token::RBrace => serializer.serialize_unit_variant(TOKEN_NAME, 14, "RBrace"),
            Token::Function => serializer.serialize_unit_variant(TOKEN_NAME, 15, "Function"),
            Token::Let => serializer.serialize_unit_variant(TOKEN_NAME, 16, "Let"),
            Token::LessThan => serializer.serialize_unit_variant(TOKEN_NAME, 17, "LessThan"),
            Token::GreaterThan => serializer.serialize_unit_variant(TOKEN_NAME, 18, "GreaterThan"),
            Token::Return => serializer.serialize_unit_variant(TOKEN_NAME, 19, "Return"),
            Token::True => serializer.serialize_unit_variant(TOKEN_NAME, 20, "True"),
            Token::False => serializer.serialize_unit_variant(TOKEN_NAME, 21, "False"),
            Token::If => serializer.serialize_unit_variant(TOKEN_NAME, 22, "If"),
            Token::Else => serializer.serialize_unit_variant(TOKEN_NAME, 23, "Else"),
            Token::Equal => serializer.serialize_unit_variant(TOKEN_NAME, 24, "Equal"),
            Token::NEqual => serializer.serialize_unit_variant(TOKEN_NAME, 25, "NEqual"),
            Token::Bang => serializer.serialize_unit_variant(TOKEN_NAME, 26, "Not"),
            Token::String(ref s) => {
                serializer.serialize_newtype_variant(TOKEN_NAME, 27, "String", s)
            }
            Token::LBracket => serializer.serialize_unit_variant(TOKEN_NAME, 28, "LBracket"),
            Token::RBracket => serializer.serialize_unit_variant(TOKEN_NAME, 29, "RBracket"),
            Token::Colon => serializer.serialize_unit_variant(TOKEN_NAME, 30, "Colon"),
        }
    }
}

struct TokenVisitor;

impl<'de> Visitor<'de> for TokenVisitor {
    type Value = Token;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("Unexpected token")
    }

    fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::EnumAccess<'de>,
    {
        match data.variant()? {
            ("Illegal", _) => Ok(Token::Illegal),
            ("EOF", _) => Ok(Token::EOF),
            ("Ident", val) => Ok(Token::Ident(val.newtype_variant()?)),
            ("Int", val) => Ok(Token::Int(val.newtype_variant()?)),
            ("Assign", _) => Ok(Token::Assign),
            ("Plus", _) => Ok(Token::Plus),
            ("Minus", _) => Ok(Token::Minus),
            ("Multiply", _) => Ok(Token::Asterisk),
            ("Divide", _) => Ok(Token::Slash),
            ("Comma", _) => Ok(Token::Comma),
            ("Semicolon", _) => Ok(Token::Semicolon),
            ("LParen", _) => Ok(Token::LParen),
            ("RParen", _) => Ok(Token::RParen),
            ("LBrace", _) => Ok(Token::LBrace),
            ("RBrace", _) => Ok(Token::RBrace),
            ("Function", _) => Ok(Token::Function),
            ("Let", _) => Ok(Token::Let),
            ("LessThan", _) => Ok(Token::LessThan),
            ("GreaterThan", _) => Ok(Token::GreaterThan),
            ("Return", _) => Ok(Token::Return),
            ("True", _) => Ok(Token::True),
            ("False", _) => Ok(Token::False),
            ("If", _) => Ok(Token::If),
            ("Else", _) => Ok(Token::Else),
            ("Equal", _) => Ok(Token::Equal),
            ("NEqual", _) => Ok(Token::NEqual),
            ("Not", _) => Ok(Token::Bang),
            ("String", val) => Ok(Token::String(val.newtype_variant()?)),
            ("LBracket", _) => Ok(Token::LBracket),
            ("RBracket", _) => Ok(Token::RBracket),
            ("Colon", _) => Ok(Token::Colon),
            _ => Err(serde::de::Error::invalid_value(
                serde::de::Unexpected::UnitVariant,
                &"expected Token variant",
            )),
        }
    }
}

const TOKEN_VARIANTS: [&str; 32] = [
    "Illegal",
    "EOF",
    "Ident",
    "Int",
    "Float",
    "Assign",
    "Plus",
    "Minus",
    "Multiply",
    "Divide",
    "Comma",
    "Semicolon",
    "LParen",
    "RParen",
    "LBrace",
    "RBrace",
    "Function",
    "Let",
    "LessThan",
    "GreaterThan",
    "Return",
    "True",
    "False",
    "If",
    "Else",
    "Equal",
    "NEqual",
    "Not",
    "String",
    "LBracket",
    "RBracket",
    "Colon",
];

impl<'de> Deserialize<'de> for Token {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_enum(TOKEN_NAME, &TOKEN_VARIANTS, TokenVisitor)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_cbor;

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
        let input = "let my_var = 8;";
        let expected = [
            Token::Let,
            Token::Ident("my_var".to_owned()),
            Token::Assign,
            Token::Int(8),
            Token::Semicolon,
            Token::EOF,
        ];
        let mut lexer = Lexer::new(input.to_owned());
        assert_eq!(lexer.tokens(), expected);
    }

    #[test]
    fn test_next_token_string() {
        let input = " \"\" \"foobar\" \"foo bar\"";

        let expected = [
            Token::String("".to_owned()),
            Token::String("foobar".to_owned()),
            Token::String("foo bar".to_owned()),
            Token::EOF,
        ];

        let mut lexer = Lexer::new(input.to_owned());
        assert_eq!(lexer.tokens(), expected);
    }

    #[test]
    fn test_next_token_arrays() {
        let input = "[1, 2];";

        let expected = [
            Token::LBracket,
            Token::Int(1),
            Token::Comma,
            Token::Int(2),
            Token::RBracket,
            Token::Semicolon,
            Token::EOF,
        ];

        let mut lexer = Lexer::new(input.to_owned());
        assert_eq!(lexer.tokens(), expected);
    }

    #[test]
    fn test_next_token_arrays_joint() {
        let input = "a * b[2], b[1], 2 * [1, 2][1];";

        let expected = [
            Token::Ident("a".to_owned()),
            Token::Asterisk,
            Token::Ident("b".to_owned()),
            Token::LBracket,
            Token::Int(2),
            Token::RBracket,
            Token::Comma,
            Token::Ident("b".to_owned()),
            Token::LBracket,
            Token::Int(1),
            Token::RBracket,
            Token::Comma,
            Token::Int(2),
            Token::Asterisk,
            Token::LBracket,
            Token::Int(1),
            Token::Comma,
            Token::Int(2),
            Token::RBracket,
            Token::LBracket,
            Token::Int(1),
            Token::RBracket,
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
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
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

    #[test]
    fn test_hash_maps() {
        let input = r#"{"food": "bar"}"#;
        let expected = [
            Token::LBrace,
            Token::String("food".to_owned()),
            Token::Colon,
            Token::String("bar".to_owned()),
            Token::RBrace,
            Token::EOF,
        ];

        let mut lexer = Lexer::new(input.to_owned());
        assert_eq!(lexer.tokens(), expected);
    }

    #[test]
    fn test_hash_maps2() {
        let input = r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#;
        let expected = [
            Token::LBrace,
            Token::String("one".to_owned()),
            Token::Colon,
            Token::Int(0),
            Token::Plus,
            Token::Int(1),
            Token::Comma,
            Token::String("two".to_owned()),
            Token::Colon,
            Token::Int(10),
            Token::Minus,
            Token::Int(8),
            Token::Comma,
            Token::String("three".to_owned()),
            Token::Colon,
            Token::Int(15),
            Token::Slash,
            Token::Int(5),
            Token::RBrace,
            Token::EOF,
        ];
        let mut lexer = Lexer::new(input.to_owned());
        assert_eq!(lexer.tokens(), expected);
    }

    #[test]
    fn test_serialization() {
        let token = Token::Ident("myvar".to_string());
        let encoded: Vec<u8> = serde_cbor::to_vec(&token).unwrap();
        // Here we are not testing for equality with a specific byte sequence,
        // as the specific output of bincode isn't as human-readable or predictable as JSON.
        // Just check that serialization was successful (did not return an error).
        assert!(!encoded.is_empty());
    }

    #[test]
    fn test_deserialization() {
        let token = Token::Ident("myvar".to_string());
        let encoded: Vec<u8> = serde_cbor::to_vec(&token).unwrap();
        let decoded: Token = serde_cbor::from_slice(&encoded).unwrap();
        match decoded {
            Token::Ident(s) => assert_eq!(s, "myvar"),
            _ => panic!("Deserialized to incorrect token variant"),
        }
    }

    #[test]
    fn test_round_trip() {
        let original_token = Token::Ident("myvar".to_string());
        let encoded: Vec<u8> = serde_cbor::to_vec(&original_token).unwrap();
        let decoded_token: Token = serde_cbor::from_slice(&encoded).unwrap();
        match decoded_token {
            Token::Ident(s) => assert_eq!(s, "myvar"),
            _ => panic!("Deserialized to incorrect token variant"),
        }
    }
}
