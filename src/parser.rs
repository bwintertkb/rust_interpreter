use crate::{
    ast::{Identifier, LetStatement, Program, Statement, StatementStruct, Statements},
    lexer::{Lexer, Token},
};

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    curr_token: Option<Token>,
    peek_token: Option<Token>,
}

impl Parser {
    pub fn new(input: &str) -> Self {
        let lexer = Lexer::new(input.to_owned());
        let mut parser = Self {
            lexer,
            curr_token: None,
            peek_token: None,
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

    pub fn parse_statement(&mut self) -> Option<Statements> {
        println!("curr_token: {:?}", self.curr_token);
        match self.curr_token {
            Some(Token::Let) => self.parse_let_statements(),
            _ => None,
        }
    }

    pub fn curr_token_is(&self, token: Token) -> bool {
        self.curr_token == Some(token)
    }

    pub fn peek_token_is(&self, token: Token) -> bool {
        matches!((&self.peek_token, &token),(Some(a), b) if std::mem::discriminant(a) == std::mem::discriminant(b))
    }

    pub fn expect_peek(&mut self, token: Token) -> bool {
        if self.peek_token_is(token) {
            self.next_token();
            return true;
        }

        false
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::default();

        if self.curr_token.is_none() {
            return program;
        }

        while !self.curr_token_is(Token::EOF) {
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            }
            self.next_token();
        }

        program
    }

    pub fn parse_let_statements(&mut self) -> Option<Statements> {
        if !self.expect_peek(Token::Ident(String::default())) {
            return None;
        }
        let curr_token = self.curr_token.clone().unwrap();
        let curr_token_literal = curr_token.literal();
        let identifier = Identifier::new(curr_token, curr_token_literal);
        // TODO expressions
        let statement = LetStatement::new(identifier, "".to_owned());

        while !self.curr_token_is(Token::Semicolon) {
            self.next_token();
        }

        Some(Statements::Let(statement))
    }
}

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
            let Statements::Let(let_) = stmt;
            assert_eq!(let_.name.value, test.identifier);
        }
    }
}
