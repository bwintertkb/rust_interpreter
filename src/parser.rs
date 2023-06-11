use std::{
    collections::HashMap,
    error::Error,
    fmt::{self, Display},
};

use crate::{
    ast::{
        ExpressionStatement, Identifier, LetStatement, Program, ReturnStatement, StatementStruct,
        Statements,
    },
    lexer::{Lexer, Token},
};

type prefix_parse_fn = Box<dyn Fn() -> String>;
type infix_parse_fn = Box<dyn Fn(String) -> String>;

#[derive(Debug, Copy, Clone)]
pub enum Iota {
    // Assign numbers to the order of operations
    LOWEST = 1,
    EQUALS = 2,
    LESSGREATER = 3, // > or <
    SUM = 4,
    PRODUCT = 5,
    PREFIX = 6, // -X or !X
    CALL = 7,   // my_function(x)
}

#[derive(Default)]
struct ParserFunction {
    prefix_parse_fns: HashMap<String, prefix_parse_fn>,
    infix_parse_fns: HashMap<String, infix_parse_fn>,
}

impl fmt::Debug for ParserFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Parser")
            .field(
                "prefix_parse_fns",
                &self.prefix_parse_fns.keys().collect::<Vec<_>>(),
            )
            .field(
                "infix_parse_fns",
                &self.infix_parse_fns.keys().collect::<Vec<_>>(),
            )
            .finish()
    }
}

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    curr_token: Option<Token>,
    peek_token: Option<Token>,
    errors: Vec<ParseError>,
    parser_functions: ParserFunction,
}

impl Parser {
    pub fn new(input: &str) -> Self {
        let lexer = Lexer::new(input.to_owned());
        let mut parser = Self {
            lexer,
            curr_token: None,
            peek_token: None,
            errors: Vec::default(),
            parser_functions: ParserFunction::default(),
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn next_token(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = Some(self.lexer.next_token());
    }

    pub fn statements(&mut self) -> Vec<StatementStruct> {
        let mut statement_tokens = Vec::new();
        let mut statements = Vec::new();
        match self.curr_token {
            Some(ref token) => statement_tokens.push(token.clone()),
            None => return statements,
        }
        while self.curr_token != Some(Token::EOF) {
            self.next_token();
            let token = self.curr_token.as_ref().unwrap().clone();
            if token == Token::Semicolon {
                let temp_statements = std::mem::take(&mut statement_tokens);
                let ss = StatementStruct::new(temp_statements);
                statements.push(ss);
            } else {
                statement_tokens.push(token);
            }
        }

        statements
    }

    pub fn curr_token_is(&self, token: Token) -> bool {
        self.curr_token == Some(token)
    }

    pub fn peek_token_is(&self, token: &Token) -> bool {
        matches!((&self.peek_token, token),(Some(a), b) if std::mem::discriminant(a) == std::mem::discriminant(b))
    }

    pub fn peek_err(&mut self, token: &Token) {
        let err = ParseError::NextExpectedTokenError(
            token.literal(),
            self.peek_token.as_ref().unwrap().literal(),
        );
        self.errors.push(err);
    }

    pub fn expect_peek(&mut self, token: Token) -> bool {
        if self.peek_token_is(&token) {
            self.next_token();
            return true;
        }

        self.peek_err(&token);
        false
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::default();

        if self.curr_token.is_none() || self.curr_token_is(Token::EOF) {
            self.errors.push(ParseError::NoToken);
            return program;
        }

        while !self.curr_token_is(Token::EOF) {
            match self.parse_statement() {
                Ok(stmt) => program.statements.push(stmt),
                // Loop here until we reach semicolon or EOF??
                Err(e) => self.errors.push(e),
            }
            self.next_token();
        }

        program
    }

    pub fn parse_statement(&mut self) -> Result<Statements, ParseError> {
        match self.curr_token {
            Some(Token::Let) => self.parse_let_statements(),
            Some(Token::Return) => self.parse_return_statements(),
            _ => self.parse_expression_statement(),
        }
    }

    pub fn parse_let_statements(&mut self) -> Result<Statements, ParseError> {
        if !self.expect_peek(Token::Ident(String::default())) {
            let err = ParseError::NextExpectedTokenError(
                self.curr_token.as_ref().unwrap().literal(),
                "ident".to_owned(),
            );
            return Err(err);
        }
        let curr_token = self.curr_token.clone().unwrap();
        let curr_token_literal = curr_token.literal();
        let identifier = Identifier::new(curr_token, curr_token_literal);
        // TODO expressions
        self.next_token();
        while !self.curr_token_is(Token::Assign) {
            self.next_token();
        }
        let statement = LetStatement::new(identifier, self.curr_token.clone().unwrap());

        while !self.curr_token_is(Token::Semicolon) {
            self.next_token();
        }

        Ok(Statements::Let(statement))
    }

    pub fn parse_return_statements(&mut self) -> Result<Statements, ParseError> {
        // if !self.expect_peek(Token::Ident(String::default())) {
        //     let err = ParseError::NextExpectedTokenError(
        //         self.curr_token.as_ref().unwrap().literal(),
        //         "ident".to_owned(),
        //     );
        //     return Err(err);
        // }
        //
        let curr_token = self.curr_token.clone().unwrap();
        let curr_token_literal = curr_token.literal();

        let stmt = ReturnStatement::new("".to_owned());
        self.next_token();

        while !self.curr_token_is(Token::Semicolon) {
            self.next_token();
        }

        Ok(Statements::Return(stmt))
    }

    pub fn parse_expression_statement(&mut self) -> Result<Statements, ParseError> {
        let curr_token = self.curr_token.clone().unwrap();

        let stmt = ExpressionStatement::new(curr_token, Iota::LOWEST);

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Ok(Statements::Expression(stmt))
    }

    pub fn parse_expression(&mut self, precidence: usize) -> Option<String> {
        let prefix = self
            .parser_functions
            .prefix_parse_fns
            .get(&self.curr_token.clone().unwrap().literal())?;

        let left_exp = prefix();
        Some(left_exp)
    }

    pub fn register_prefix(&mut self, token: &Token, fn_: prefix_parse_fn) {
        self.parser_functions
            .prefix_parse_fns
            .insert(token.literal(), fn_);
    }

    pub fn register_infix(&mut self, token: &Token, fn_: infix_parse_fn) {
        self.parser_functions
            .infix_parse_fns
            .insert(token.literal(), fn_);
    }
}

#[derive(Debug)]
pub enum ParseError {
    NextExpectedTokenError(String, String),
    UnexpectedToken,
    NoToken,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NextExpectedTokenError(actual, expected) => write!(
                f,
                "Expected token '{}', but received token type '{}'",
                expected, actual
            ),
            Self::UnexpectedToken => write!(f, "Unexpected token"),
            Self::NoToken => write!(f, "No token"),
        }
    }
}

impl Error for ParseError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug)]
    struct ExpectedIdentifier {
        identifier: String,
    }

    impl ExpectedIdentifier {
        fn new(identifier: String) -> Self {
            ExpectedIdentifier { identifier }
        }
    }

    #[test]
    fn test_let_statements() {
        let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
    let 7718;
        ";

        let mut parser = Parser::new(input);
        let program = parser.parse_program();

        assert_eq!(program.statements.len(), 3);

        let tests: Vec<ExpectedIdentifier> = vec![
            ExpectedIdentifier::new("x".to_owned()),
            ExpectedIdentifier::new("y".to_owned()),
            ExpectedIdentifier::new("foobar".to_owned()),
        ];

        let zipped: Vec<_> = program.statements.iter().zip(tests.iter()).collect();

        for (stmt, test) in zipped {
            if let Statements::Let(let_) = stmt {
                assert_eq!(let_.name.value, test.identifier);
            }
        }
    }

    #[test]
    fn test_no_token_err() {
        let input = "";
        let mut parser = Parser::new(input);
        parser.parse_program();

        assert_eq!(parser.errors.len(), 1);
    }

    #[test]
    fn test_return_statements() {
        let input = "
return 5;
return 10;
return 993322;
";
        let mut parser = Parser::new(input);
        let program = parser.parse_program();

        assert!(parser.errors.is_empty());

        let tests: Vec<ExpectedIdentifier> = vec![
            ExpectedIdentifier::new("".to_owned()),
            ExpectedIdentifier::new("".to_owned()),
            ExpectedIdentifier::new("".to_owned()),
        ];

        let zipped: Vec<_> = program.statements.iter().zip(tests.iter()).collect();

        for (stmt, test) in zipped {
            if let Statements::Let(let_) = stmt {
                assert_eq!(let_.name.value, test.identifier);
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let mut parser = Parser::new(input);
        let program = parser.parse_program();
        println!("{:?}", program);
    }
}
