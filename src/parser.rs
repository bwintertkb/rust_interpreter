use std::{
    collections::HashMap,
    error::Error,
    fmt::{Debug, Display},
};

use crate::{
    ast::{
        ExpressionStatement, Expressions, Identifier, LetStatement, Program, ReturnStatement,
        StatementStruct, Statements,
    },
    lexer::{Lexer, Token},
};

#[derive(Debug)]
pub enum Iota {
    Lowest = 0,
    Equals = 1,
    LessGreater = 2,
    Sum = 3,
    Product = 4,
    Prefix = 5,
    Call = 6,
}

#[derive(Debug)]
pub enum ParserFn {
    Prefix,
    Infix(String),
}

#[derive(Default)]
pub struct PrefixFns {
    fns: HashMap<String, fn(&mut Parser) -> Expressions>,
}

impl Debug for PrefixFns {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PrefixFns")
            .field("fns", &self.fns.len())
            .finish()
    }
}

#[derive(Default)]
pub struct InfixFns {
    fns: HashMap<String, fn(&mut Parser, Expressions) -> Expressions>,
}

impl Debug for InfixFns {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("InfixFns")
            .field("fns", &self.fns.len())
            .finish()
    }
}

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    curr_token: Option<Token>,
    peek_token: Option<Token>,
    errors: Vec<ParseError>,
    parser_prefix_fn: PrefixFns,
    parser_infix_fn: InfixFns,
}

impl Parser {
    pub fn new(input: &str) -> Self {
        let lexer = Lexer::new(input.to_owned());
        let mut parser = Self {
            lexer,
            curr_token: None,
            peek_token: None,
            errors: Vec::default(),
            parser_prefix_fn: PrefixFns::default(),
            parser_infix_fn: InfixFns::default(),
        };

        parser.register_prefix(Token::Ident("foobar".to_owned()).literal());

        parser.next_token();
        parser.next_token();
        parser
    }

    pub fn next_token(&mut self) {
        // Some comment
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
            _ => self.parse_expression_statement(),
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

    pub fn parse_expression_statement(&mut self) -> Result<Statements, ParseError> {
        let mut stmt = ExpressionStatement::new(self.curr_token.clone().unwrap());
        stmt.expression = self.parse_expression(Iota::Lowest as usize);

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }
        Ok(Statements::Expression(stmt))
    }

    pub fn parse_identifier(&self) -> Identifier {
        let curr_token = self.curr_token.as_ref().unwrap();
        Identifier::new(curr_token.clone(), curr_token.literal())
    }

    pub fn parse_expression(&mut self, precedence: usize) -> Option<Expressions> {
        println!("parse_expression: {:?}", self.curr_token);
        let prefix_fn = self
            .parser_prefix_fn
            .fns
            .get(&self.curr_token.as_ref().unwrap().literal())?;

        let left_expr = prefix_fn(self);
        println!("left_expr: {:?}", left_expr);
        Some(left_expr)
    }

    pub fn register_prefix(&mut self, token_literal: String) {
        self.parser_prefix_fn
            .fns
            .insert(token_literal, parse_prefix_expression);
    }

    pub fn register_infix(&mut self, token_literal: String) {
        self.parser_infix_fn
            .fns
            .insert(token_literal, parse_infix_expression);
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

fn parse_prefix_expression(p: &mut Parser) -> Expressions {
    let token = p.curr_token.clone().unwrap();
    match token {
        Token::Ident(_) => Expressions::Identifier(p.parse_identifier()),
        _ => panic!("Not implemented"),
    }
}

fn parse_infix_expression(p: &mut Parser, expression: Expressions) -> Expressions {
    Expressions::TODO
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
    use crate::parser;

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

        assert_eq!(program.statements.len(), 4);

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

        assert!(parser.errors.is_empty());
        let stmt = &program.statements[0];
        if let Statements::Expression(expr) = stmt {
            let ident = expr.expression.clone().unwrap();
            if let Expressions::Identifier(ident) = ident {
                assert_eq!(ident.value, "foobar");
                assert_eq!(ident.token.literal(), "foobar");
            } else {
                panic!("Not expected expression");
            }
            // assert_eq!(ident.value, "foobar");
        } else {
            panic!("Not expected statement");
        }
    }
}
