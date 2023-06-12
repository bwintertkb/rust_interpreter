use std::{collections::HashMap, error::Error, fmt::Display};

use crate::{
    ast::{
        Identifier, LetStatement, Program, ReturnStatement, Statement, StatementStruct, Statements,
    },
    lexer::{Lexer, Token},
};

#[derive(Debug)]
pub enum ParserFn {
    Prefix,
    Infix(String),
}

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    curr_token: Option<Token>,
    peek_token: Option<Token>,
    errors: Vec<ParseError>,
    parser_fn: HashMap<String, fn(&mut Parser) -> String>,
}

impl Parser {
    pub fn new(input: &str) -> Self {
        let lexer = Lexer::new(input.to_owned());
        let mut parser = Self {
            lexer,
            curr_token: None,
            peek_token: None,
            errors: Vec::default(),
            parser_fn: HashMap::default(),
        };

        parser
            .parser_fn
            .insert("test".to_owned(), parse_prefix_expression);

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

    pub fn parse_statement(&mut self) -> Result<Statements, ParseError> {
        match self.curr_token {
            Some(Token::Let) => self.parse_let_statements(),
            Some(Token::Return) => self.parse_return_statements(),
            _ => Err(ParseError::UnexpectedToken),
        }
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
        let statement = LetStatement::new(identifier, "".to_owned());

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

    //     func (p *Parser) parsePrefixExpression() ast.Expression {
    // 	expression := &ast.PrefixExpression{
    // 		Token:    p.curToken,
    // 		Operator: p.curToken.Literal,
    // 	}
    //
    // 	p.nextToken()
    //
    // 	expression.Right = p.parseExpression(PREFIX)
    //
    // 	return expression
    // }
    //
    // func (p *Parser) parseInfixExpression(left ast.Expression) ast.Expression {
    // 	expression := &ast.InfixExpression{
    // 		Token:    p.curToken,
    // 		Operator: p.curToken.Literal,
    // 		Left:     left,
    // 	}
    //
    // 	precedence := p.curPrecedence()
    // 	p.nextToken()
    // 	expression.Right = p.parseExpression(precedence)
    //
    // 	return expression
    // }
}

fn parse_prefix_expression(p: &mut Parser) -> String {
    "".to_owned()
}

fn parse_infix_expression(p: &mut Parser, expression: String) -> String {
    "".to_owned()
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
}
