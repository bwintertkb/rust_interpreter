use std::{
    collections::HashMap,
    error::Error,
    fmt::{Debug, Display},
};

use once_cell::sync::Lazy;

use crate::{
    ast::{
        ArrayLiteral, BlockStatement, Boolean, CallExpression, ExpressionStatement, Expressions,
        FunctionLiteral, HashMapLiteral, Identifier, IfExpression, IndexExpression,
        InfixExpression, IntegerLiteral, LetStatement, PrefixExpression, Program, ReturnStatement,
        StatementStruct, Statements, StringLiteral,
    },
    lexer::{Lexer, Token},
};

static PRECEDENCE_TABLE: Lazy<HashMap<String, usize>> = Lazy::new(|| {
    let mut precedence_table = HashMap::new();
    precedence_table.insert(Token::Equal.token_literal(), Iota::Equals as usize);
    precedence_table.insert(Token::NEqual.token_literal(), Iota::Equals as usize);
    precedence_table.insert(Token::LessThan.token_literal(), Iota::LessGreater as usize);
    precedence_table.insert(
        Token::GreaterThan.token_literal(),
        Iota::LessGreater as usize,
    );
    precedence_table.insert(Token::Plus.token_literal(), Iota::Sum as usize);
    precedence_table.insert(Token::Minus.token_literal(), Iota::Sum as usize);
    precedence_table.insert(Token::Slash.token_literal(), Iota::Product as usize);
    precedence_table.insert(Token::Asterisk.token_literal(), Iota::Product as usize);
    precedence_table.insert(Token::LParen.token_literal(), Iota::Call as usize);
    precedence_table.insert(Token::LBracket.token_literal(), Iota::Index as usize);

    precedence_table
});

#[derive(Debug)]
pub enum Iota {
    Lowest = 0,
    Equals = 1,
    LessGreater = 2,
    Sum = 3,
    Product = 4,
    Prefix = 5,
    Call = 6,
    Index = 7,
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

        parser.register_prefix(Token::Ident(String::default()).token_literal());
        parser.register_prefix(Token::Int(1).token_literal());
        parser.register_prefix(Token::Bang.token_literal());
        parser.register_prefix(Token::Minus.token_literal());
        parser.register_prefix(Token::True.token_literal());
        parser.register_prefix(Token::False.token_literal());
        parser.register_prefix(Token::LParen.token_literal());
        parser.register_prefix(Token::If.token_literal());
        parser.register_prefix(Token::Function.token_literal());
        parser.register_prefix(Token::String(String::default()).token_literal());
        parser.register_prefix(Token::LBracket.token_literal());
        parser.register_prefix(Token::LBrace.token_literal());

        parser.register_infix(Token::Plus.token_literal());
        parser.register_infix(Token::Minus.token_literal());
        parser.register_infix(Token::Slash.token_literal());
        parser.register_infix(Token::Asterisk.token_literal());
        parser.register_infix(Token::Equal.token_literal());
        parser.register_infix(Token::NEqual.token_literal());
        parser.register_infix(Token::LessThan.token_literal());
        parser.register_infix(Token::GreaterThan.token_literal());
        parser.register_infix(Token::LParen.token_literal());
        parser.register_infix(Token::LBracket.token_literal());

        parser.next_token();
        parser.next_token();
        parser
    }

    pub fn errors(&self) -> &[ParseError] {
        &self.errors
    }

    pub fn print_errors(&self) {
        for error in &self.errors {
            println!("{}", error);
        }
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

    pub fn peek_precedence(&self) -> usize {
        let peek_token = self.peek_token.as_ref().unwrap().token_literal();
        if let Some(precedence) = PRECEDENCE_TABLE.get(&peek_token) {
            return *precedence;
        }
        Iota::Lowest as usize
    }

    pub fn curr_precedence(&self) -> usize {
        let curr_token = self.curr_token.as_ref().unwrap().token_literal();
        if let Some(precedence) = PRECEDENCE_TABLE.get(&curr_token) {
            return *precedence;
        }
        Iota::Lowest as usize
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
        if !self.expect_peek(Token::Assign) {
            let err = ParseError::NextExpectedTokenError(Token::Assign.literal(), "=".to_owned());
            return Err(err);
        }

        self.next_token();
        let curr_token_literal = curr_token.literal();
        let expr = self.parse_expression(Iota::Lowest as usize);

        let identifier = Identifier::new(curr_token, curr_token_literal);
        // TODO expressions
        let statement = LetStatement::new(identifier, expr.unwrap());

        while !self.curr_token_is(Token::Semicolon) {
            self.next_token();
        }

        Ok(Statements::Let(statement))
    }

    pub fn parse_return_statements(&mut self) -> Result<Statements, ParseError> {
        let curr_token = self.curr_token.clone().unwrap();
        let _curr_token_literal = curr_token.literal();

        self.next_token();
        let return_value = self.parse_expression(Iota::Lowest as usize);
        let stmt = ReturnStatement::new(return_value);

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

    pub fn parse_array_literal(&mut self) -> ArrayLiteral {
        let elements = self.parse_expression_list(Token::RBracket);
        ArrayLiteral::new(elements)
    }

    pub fn parse_expression_list(&mut self, end: Token) -> Vec<Expressions> {
        let mut list: Vec<Expressions> = Vec::new();

        if self.peek_token_is(&end) {
            self.next_token();
            return list;
        }

        self.next_token();
        list.push(self.parse_expression(Iota::Lowest as usize).unwrap());
        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();
            list.push(self.parse_expression(Iota::Lowest as usize).unwrap());
        }

        if !self.expect_peek(end.clone()) {
            panic!("Expected end token: {:?}", end);
        }

        list
    }

    pub fn parse_identifier(&self) -> Identifier {
        let curr_token = self.curr_token.as_ref().unwrap();
        Identifier::new(curr_token.clone(), curr_token.literal())
    }

    pub fn parse_integer_literal(&self) -> IntegerLiteral {
        let curr_token = self.curr_token.as_ref().unwrap();
        let value = curr_token.literal().parse::<i64>().unwrap();
        IntegerLiteral::new(curr_token.clone(), value)
    }

    pub fn parse_string_literal(&self) -> StringLiteral {
        let curr_token = self.curr_token.as_ref().unwrap();
        let value = curr_token.literal();
        StringLiteral::new(value)
    }

    pub fn parse_boolean(&self) -> Boolean {
        let curr_token = self.curr_token.as_ref().unwrap().clone();
        Boolean::new(curr_token, self.curr_token_is(Token::True))
    }

    pub fn parse_function_literal(&mut self) -> FunctionLiteral {
        if !self.expect_peek(Token::LParen) {
            panic!("Expected LParen");
        }

        let parameters = self.parse_function_parameters();

        if !self.expect_peek(Token::LBrace) {
            panic!("Expected LParen");
        }

        let body = self.parse_block_statement();

        FunctionLiteral::new(parameters, body)
    }

    pub fn parse_function_parameters(&mut self) -> Vec<Identifier> {
        let mut identifiers = Vec::new();

        if self.peek_token_is(&Token::RParen) {
            self.next_token();
            return identifiers;
        }

        self.next_token();

        let curr_token = self.curr_token.clone().unwrap();
        let value = curr_token.literal();
        let iden = Identifier::new(curr_token, value);
        identifiers.push(iden);

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();

            let curr_token = self.curr_token.clone().unwrap();
            let value = curr_token.literal();
            let iden = Identifier::new(curr_token, value);
            identifiers.push(iden);
        }

        if !self.expect_peek(Token::RParen) {
            panic!("Expected RParen");
        }

        identifiers
    }

    pub fn parse_index_expression(&mut self, left: Expressions) -> Expressions {
        self.next_token();
        let index = self.parse_expression(Iota::Lowest as usize).unwrap();
        println!("Index: {:?}", index);

        if !self.expect_peek(Token::RBracket) {
            panic!("Expected RBracket, current token: {:?}", self.curr_token);
        }

        Expressions::Index(Box::new(IndexExpression::new(left, index)))
    }

    pub fn parse_call_expression(&mut self, expr: Expressions) -> Expressions {
        let arguments = self.parse_expression_list(Token::RParen);
        Expressions::Call(Box::new(CallExpression::new(expr, arguments)))
    }

    pub fn parse_call_arguments(&mut self) -> Vec<Expressions> {
        let mut args: Vec<Expressions> = Vec::new();

        if self.peek_token_is(&Token::RParen) {
            self.next_token();
            return args;
        }

        self.next_token();
        args.push(self.parse_expression(Iota::Lowest as usize).unwrap());

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(Iota::Lowest as usize).unwrap());
        }

        println!("args: {:?}", args);
        if !self.expect_peek(Token::RParen) {
            panic!("Expected RParen");
        }

        args
    }

    pub fn parse_prefix_expression(&mut self) -> PrefixExpression {
        let curr_token = self.curr_token.clone().unwrap();
        let token_literal = curr_token.token_literal();
        self.next_token();

        PrefixExpression::new(
            curr_token,
            token_literal,
            self.parse_expression(Iota::Prefix as usize).unwrap(),
        )
    }

    pub fn parse_infix_expression(&mut self, expression: Expressions) -> InfixExpression {
        let curr_token = self.curr_token.clone().unwrap();
        let curr_token_literal = curr_token.token_literal();
        let curr_precedence = self.curr_precedence();
        self.next_token();
        let right_expression = self.parse_expression(curr_precedence);

        InfixExpression::new(
            curr_token,
            expression,
            curr_token_literal,
            right_expression.unwrap(),
        )
    }

    pub fn parse_grouped_expression(&mut self) -> Expressions {
        self.next_token();

        let exp = self.parse_expression(Iota::Lowest as usize);
        if !self.expect_peek(Token::RParen) {
            panic!("Expected RParen")
        }
        exp.unwrap()
    }

    pub fn parse_if_expression(&mut self) -> IfExpression {
        if !self.expect_peek(Token::LParen) {
            panic!("Expected LParen");
        }

        self.next_token();
        let parsed_expr = self.parse_expression(Iota::Lowest as usize).unwrap();

        if !self.expect_peek(Token::RParen) {
            panic!("Expected RParen");
        }

        if !self.expect_peek(Token::LBrace) {
            panic!("Expected LBrace");
        }

        let consequence = self.parse_block_statement();

        let alternative = if self.peek_token_is(&Token::Else) {
            self.next_token();
            if !self.expect_peek(Token::LBrace) {
                panic!("Expected LBrace");
            }
            Some(self.parse_block_statement())
        } else {
            None
        };

        IfExpression::new(parsed_expr, consequence, alternative)
    }

    pub fn parse_block_statement(&mut self) -> BlockStatement {
        let curr_token = self.curr_token.clone().unwrap();

        let mut block = BlockStatement::new(curr_token, Vec::new());

        self.next_token();

        while !self.curr_token_is(Token::RBrace) && !self.curr_token_is(Token::EOF) {
            let stmt = self.parse_statement();
            if let Ok(stmt) = stmt {
                block.statements.push(stmt);
            }
            self.next_token();
        }
        block
    }

    pub fn parse_hashmap_literal(&mut self) -> HashMapLiteral {
        let mut hash = HashMapLiteral::default();
        let mut pairs = HashMap::new();

        while !self.peek_token_is(&Token::RBrace) {
            self.next_token();
            let key = self.parse_expression(Iota::Lowest as usize).unwrap();
            if !self.expect_peek(Token::Colon) {
                panic!("Parse HashMap Expected Colon");
            }
            self.next_token();
            let value = self.parse_expression(Iota::Lowest as usize).unwrap();
            pairs.insert(key, value);

            if !self.peek_token_is(&Token::RBrace) && !self.expect_peek(Token::Comma) {
                panic!("Parse HashMap Expected RBrace or Comma");
            }
        }

        if !self.expect_peek(Token::RBrace) {
            panic!("Parse HashMap Expected RBrace");
        }

        hash.pairs.0 = pairs;
        hash
    }

    pub fn parse_expression(&mut self, precedence: usize) -> Option<Expressions> {
        let token = self.curr_token.as_ref().unwrap().clone();
        let prefix_fn = match self.parser_prefix_fn.fns.get(&token.token_literal()) {
            Some(fn_) => fn_,
            None => {
                println!("No prefix parse function for: {:?}", token);
                self.no_prefix_parse_fn_error(&token);
                return None;
            }
        };

        let mut left_expr = prefix_fn(self);

        while !self.peek_token_is(&Token::Semicolon) && precedence < self.peek_precedence() {
            let infix_fn = match self
                .parser_infix_fn
                .fns
                .get(&self.peek_token.as_ref().unwrap().token_literal())
            {
                Some(fn_) => *fn_,
                None => return Some(left_expr),
            };

            self.next_token();

            left_expr = infix_fn(self, left_expr);
        }
        Some(left_expr)
    }

    pub fn no_prefix_parse_fn_error(&mut self, token: &Token) {
        self.errors
            .push(ParseError::PrefixParseError(token.token_literal()));
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
}

fn parse_prefix_expression(p: &mut Parser) -> Expressions {
    let token = p.curr_token.clone().unwrap();
    match token {
        Token::Ident(_) => Expressions::Identifier(p.parse_identifier()),
        Token::Int(_) => Expressions::Int(p.parse_integer_literal()),
        Token::True | Token::False => Expressions::Boolean(p.parse_boolean()),
        Token::Minus | Token::Bang => Expressions::PrefixExpr(p.parse_prefix_expression()),
        Token::LParen => p.parse_grouped_expression(),
        Token::If => Expressions::IfExpr(Box::new(p.parse_if_expression())),
        Token::Function => Expressions::Fn(p.parse_function_literal()),
        Token::String(_) => Expressions::String(p.parse_string_literal()),
        Token::LBracket => Expressions::Array(p.parse_array_literal()),
        Token::LBrace => Expressions::HashMap(p.parse_hashmap_literal()),
        _ => panic!("Not implemented"),
    }
}

fn parse_infix_expression(p: &mut Parser, expression: Expressions) -> Expressions {
    if p.curr_token_is(Token::LParen) {
        return p.parse_call_expression(expression);
    }

    if p.curr_token_is(Token::LBracket) {
        return p.parse_index_expression(expression);
    }
    Expressions::InfixExpr(p.parse_infix_expression(expression))
}

#[derive(Debug)]
pub enum ParseError {
    NextExpectedTokenError(String, String),
    PrefixParseError(String),
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
            Self::PrefixParseError(token) => {
                write!(f, "no prefix parse function for {} found", token)
            }
            Self::UnexpectedToken => write!(f, "Unexpected token"),
            Self::NoToken => write!(f, "No token"),
        }
    }
}

impl Error for ParseError {}

#[cfg(test)]
mod tests {
    use crate::{ast::Node};

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
        struct LetStatementTest {
            input: &'static str,
            expected_identifier: &'static str,
            expected_value: Expected,
        }

        impl LetStatementTest {
            fn new(
                input: &'static str,
                expected_identifier: &'static str,
                expected_value: Expected,
            ) -> Self {
                LetStatementTest {
                    input,
                    expected_identifier,
                    expected_value,
                }
            }
        }

        let tests: [LetStatementTest; 3] = [
            LetStatementTest::new("let x = 5;", "x", Expected::Int(5)),
            LetStatementTest::new("let y = true;", "y", Expected::Boolean(true)),
            LetStatementTest::new(
                "let foobar = y;",
                "foobar",
                Expected::String("y".to_owned()),
            ),
        ];

        for test in tests.iter() {
            let mut parser = Parser::new(test.input);
            let program = parser.parse_program();

            assert!(parser.errors.is_empty());

            assert_eq!(program.statements.len(), 1);

            let stmt = &program.statements[0];

            assert!(test_let_statement(stmt, test.expected_identifier));

            println!("STATEMENT: {:?}", stmt);

            let expr = match stmt {
                Statements::Let(expr) => expr,
                _ => panic!("Expected expression"),
            };

            println!("EXPR STATEMENT: {:?}", expr.value);

            test_literal_expression(expr.value.clone(), test.expected_value.clone());

            // match &test.expected_value {
            //     ExpectedValue::Int(value) => {
            //         test_integer_literal(expr, *value);
            //     }
            //     ExpectedValue::Bool(value) => {
            //         test_boolean_literal(expr, *value);
            //     }
            //     ExpectedValue::String(value) => {
            //         test_identifier(expr, (*value).to_owned());
            //     }
            // }
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

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let mut parser = Parser::new(input);
        let program = parser.parse_program();

        assert!(parser.errors.is_empty());

        let stmt = &program.statements[0];
        if let Statements::Expression(expr) = stmt {
            let int_lit = expr.expression.clone().unwrap();
            if let Expressions::Int(int) = int_lit {
                assert_eq!(int.value, 5);
                assert_eq!(int.token_literal(), "5");
            } else {
                panic!("Not expected expression");
            }
        } else {
            panic!("Not expected statement");
        }
    }

    fn test_integer_literal(il: Expressions, value: i64) -> bool {
        let int = if let Expressions::Int(int) = il {
            int
        } else {
            println!("Not expected expression");
            return false;
        };

        if int.value != value {
            println!("Expected value: {}, got: {}", value, int.value);
            return false;
        }

        if int.token.literal() != format!("{}", value) {
            println!(
                "Expected token literal: {}, got: {}",
                value,
                int.token.literal()
            );
            return false;
        }

        true
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        #[derive(Debug, PartialEq, Eq)]
        enum ResType {
            Int(i64),
            Bool(bool),
        }

        impl ResType {
            fn int(&self) -> i64 {
                match self {
                    ResType::Int(v) => *v,
                    _ => panic!("Not an int"),
                }
            }

            fn bool(&self) -> bool {
                match self {
                    ResType::Bool(v) => *v,
                    _ => panic!("Not a bool"),
                }
            }
        }

        struct PrefixTest {
            input: String,
            operator: String,
            value: ResType,
        }

        impl PrefixTest {
            fn new(input: String, operator: String, value: ResType) -> Self {
                PrefixTest {
                    input,
                    operator,
                    value,
                }
            }
        }

        let prefix_tests: [PrefixTest; 2] = [
            PrefixTest::new("!5;".to_owned(), "!".to_owned(), ResType::Int(5)),
            PrefixTest::new("-15".to_owned(), "-".to_owned(), ResType::Int(15)),
        ];

        for pt in prefix_tests.into_iter() {
            let mut parser = Parser::new(&pt.input);
            let program = parser.parse_program();

            assert!(parser.errors.is_empty());
            assert_eq!(program.statements.len(), 1);

            let stmt = &program.statements[0];

            if let Statements::Expression(expr) = stmt {
                let prefix_expr = expr.expression.clone().unwrap();
                if let Expressions::PrefixExpr(prefix_expr) = prefix_expr {
                    assert_eq!(prefix_expr.operator, pt.operator);
                    match pt.value {
                        ResType::Int(v) => assert!(test_integer_literal(*prefix_expr.right, v)),
                        ResType::Bool(v) => assert!(test_boolean_literal(*prefix_expr.right, v)),
                    }
                } else {
                    panic!("Not expected expression");
                }
            } else {
                panic!("Not expected statement");
            }
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        #[derive(Debug, PartialEq, Eq)]
        enum ResType {
            Int(i64),
            Bool(bool),
        }

        impl ResType {
            fn int(&self) -> i64 {
                match self {
                    ResType::Int(v) => *v,
                    _ => panic!("Not an int"),
                }
            }

            fn bool(&self) -> bool {
                match self {
                    ResType::Bool(v) => *v,
                    _ => panic!("Not a bool"),
                }
            }
        }

        // Infix <expression> <infix operator> <expression>
        struct InfixTest {
            input: String,
            left_value: ResType,
            operator: String,
            right_value: ResType,
        }

        impl InfixTest {
            fn new(
                input: String,
                left_value: ResType,
                operator: String,
                right_value: ResType,
            ) -> Self {
                InfixTest {
                    input,
                    left_value,
                    operator,
                    right_value,
                }
            }
        }

        let infix_tests: [InfixTest; 11] = [
            InfixTest::new(
                "5 + 5".to_owned(),
                ResType::Int(5),
                "+".to_owned(),
                ResType::Int(5),
            ),
            InfixTest::new(
                "5 - 5".to_owned(),
                ResType::Int(5),
                "-".to_owned(),
                ResType::Int(5),
            ),
            InfixTest::new(
                "5 * 5".to_owned(),
                ResType::Int(5),
                "*".to_owned(),
                ResType::Int(5),
            ),
            InfixTest::new(
                "5 / 5".to_owned(),
                ResType::Int(5),
                "/".to_owned(),
                ResType::Int(5),
            ),
            InfixTest::new(
                "5 > 5".to_owned(),
                ResType::Int(5),
                ">".to_owned(),
                ResType::Int(5),
            ),
            InfixTest::new(
                "5 < 5".to_owned(),
                ResType::Int(5),
                "<".to_owned(),
                ResType::Int(5),
            ),
            InfixTest::new(
                "5 == 5".to_owned(),
                ResType::Int(5),
                "==".to_owned(),
                ResType::Int(5),
            ),
            InfixTest::new(
                "5 != 5".to_owned(),
                ResType::Int(5),
                "!=".to_owned(),
                ResType::Int(5),
            ),
            InfixTest::new(
                "true == true".to_owned(),
                ResType::Bool(true),
                "==".to_owned(),
                ResType::Bool(true),
            ),
            InfixTest::new(
                "true != false".to_owned(),
                ResType::Bool(true),
                "!=".to_owned(),
                ResType::Bool(false),
            ),
            InfixTest::new(
                "false == false".to_owned(),
                ResType::Bool(false),
                "==".to_owned(),
                ResType::Bool(false),
            ),
        ];

        for infix in infix_tests.into_iter() {
            let mut parser = Parser::new(&infix.input);
            let program = parser.parse_program();

            assert!(parser.errors.is_empty());
            assert_eq!(program.statements.len(), 1);

            let stmt = &program.statements[0];

            if let Statements::Expression(expr) = stmt {
                let infix_expr = expr.expression.clone().unwrap();
                if let Expressions::InfixExpr(expr) = infix_expr {
                    match infix.left_value {
                        ResType::Int(v) => assert!(test_integer_literal(*expr.left, v)),
                        ResType::Bool(v) => assert!(test_boolean_literal(*expr.left, v)),
                    }
                    assert_eq!(expr.operator, infix.operator);
                    match infix.right_value {
                        ResType::Int(v) => assert!(test_integer_literal(*expr.right, v)),
                        ResType::Bool(v) => assert!(test_boolean_literal(*expr.right, v)),
                    }
                } else {
                    panic!("Not expected expression");
                }
            } else {
                panic!("Not expected statement");
            }
        }
    }

    fn test_identifier(expr: Expressions, value: String) -> bool {
        println!("expr is '{:?}'", expr);
        let ident = if let Expressions::Identifier(ident) = expr {
            ident
        } else {
            return false;
        };

        if ident.value != value {
            println!("ident.value not {}. got={}", value, ident.value);
            return false;
        }

        if ident.token_literal() != value {
            println!(
                "ident.token_literal not {}. got={}",
                value,
                ident.token_literal()
            );

            return false;
        }

        true
    }

    #[derive(Debug, Clone)]
    enum Expected {
        Int(i64),
        String(String),
        Boolean(bool),
        Array(Vec<Expected>),
        Infix(Box<Expected>, String, Box<Expected>),
    }

    fn test_let_statement(stmt: &Statements, name: &str) -> bool {
        println!("let_stmt.name.value is '{:?}'", stmt);
        if stmt.token_literal() != "let" {
            println!("s.token_literal not 'let'. got={}", stmt.token_literal());
            return false;
        }

        let let_stmt = if let Statements::Let(stmt) = stmt {
            stmt
        } else {
            return false;
        };

        if let_stmt.name.value != name {
            println!(
                "let_stmt.name.value not '{}'. got={}",
                name, let_stmt.name.value
            );
            return false;
        }

        if let_stmt.name.token_literal() != name {
            println!(
                "let_stmt.name.token_literal not '{}'. got={}",
                name,
                let_stmt.name.token_literal()
            );
            return false;
        }

        true
    }

    fn test_literal_expression(expr: Expressions, expected: Expected) -> bool {
        match expected {
            Expected::Int(int) => test_integer_literal(expr, int),
            Expected::String(str) => test_identifier(expr, str),
            Expected::Boolean(bool_) => test_boolean_literal(expr, bool_),
            Expected::Array(arr) => test_array_literal(expr, arr),
            _ => panic!("Not implemented litral expression"),
        }
    }

    fn test_array_literal(expr: Expressions, expected: Vec<Expected>) -> bool {
        let arr = if let Expressions::Array(arr) = expr {
            arr
        } else {
            return false;
        };

        if arr.elements.len() != expected.len() {
            println!(
                "len(arr.elements) not {}. got={}",
                expected.len(),
                arr.elements.len()
            );
            return false;
        }

        for (i, exp) in expected.iter().enumerate() {
            match exp {
                Expected::Int(int) => test_integer_literal(arr.elements[i].clone(), *int),
                Expected::String(str) => test_identifier(arr.elements[i].clone(), str.to_owned()),
                Expected::Boolean(bool_) => test_boolean_literal(arr.elements[i].clone(), *bool_),
                _ => panic!("Not implemented litral expression"),
            };
        }

        true
    }

    fn test_boolean_literal(expr: Expressions, bool_: bool) -> bool {
        let b = if let Expressions::Boolean(b) = expr {
            b
        } else {
            return false;
        };

        if b.value != bool_ {
            println!("b.value not {}. got={}", bool_, b.value);
            return false;
        }

        if b.token_literal() != format!("{}", bool_) {
            println!("b.token_literal not {}. got={}", bool_, b.token_literal());
            return false;
        }

        true
    }

    fn test_infix_expression(
        expr: Expressions,
        left: Expected,
        operator: String,
        right: Expected,
    ) -> bool {
        let infix = if let Expressions::InfixExpr(infix) = expr {
            infix
        } else {
            return false;
        };

        if !test_literal_expression(*infix.left, left) {
            println!("test_literal_expression left failed");
            return false;
        }

        if infix.operator != operator {
            println!("infix.operator is not {}. got={}", operator, infix.operator);
            return false;
        }

        if !test_literal_expression(*infix.right, right) {
            println!("test_literal_expression right failed");
            return false;
        }

        true
    }

    #[test]
    fn test_boolean_expression() {
        struct BooleanExpect {
            input: String,
            boolean: bool,
        }

        impl BooleanExpect {
            fn new(input: String, boolean: bool) -> Self {
                BooleanExpect { input, boolean }
            }
        }

        let bool_tests: [BooleanExpect; 2] = [
            BooleanExpect::new("true;".to_owned(), true),
            BooleanExpect::new("false;".to_owned(), false),
        ];

        for t in bool_tests.into_iter() {
            let mut parse = Parser::new(&t.input);
            let program = parse.parse_program();

            assert!(parse.errors.is_empty());

            let stmt = &program.statements[0];

            if let Statements::Expression(expr) = stmt {
                let expr = expr.expression.as_ref().unwrap();
                if let Expressions::Boolean(boolean) = expr {
                    assert_eq!(boolean.value, t.boolean);
                } else {
                    panic!("Not expected expression");
                }
            } else {
                panic!("Not expected statement");
            }
        }
    }

    #[test]
    fn test_if_expression() {
        let input: &str = "if(x<y){x}";
        let mut parser = Parser::new(input);
        let program = parser.parse_program();
        assert!(parser.errors.is_empty());
        let stmt = &program.statements[0];
        let if_ = if let Statements::Expression(expr) = stmt {
            let expr = expr.expression.as_ref().unwrap();
            if let Expressions::IfExpr(if_) = expr {
                assert_eq!(if_.consequence.statements.len(), 1);
                assert!(if_.alternative.is_none());
                if_
            } else {
                panic!("Not expected expression");
            }
        } else {
            panic!("Not expected statement");
        };

        let stmt = &if_.consequence.statements[0];
        if let Statements::Expression(expr) = stmt {
            let expr = expr.expression.as_ref().unwrap();
            assert!(test_identifier(expr.clone(), "x".to_owned()));
        } else {
            panic!("Not expected statement");
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input: &str = "if (x < y) { x } else { y }";
        let mut parser = Parser::new(input);
        let program = parser.parse_program();
        assert!(parser.errors.is_empty());
        let stmt = &program.statements[0];
        let if_ = if let Statements::Expression(expr) = stmt {
            let expr = expr.expression.as_ref().unwrap();
            if let Expressions::IfExpr(if_) = expr {
                assert!(test_infix_expression(
                    if_.condition.clone(),
                    Expected::String("x".to_owned()),
                    "<".to_owned(),
                    Expected::String("y".to_owned()),
                ));
                assert_eq!(if_.consequence.statements.len(), 1);
                if_
            } else {
                panic!("Not expected expression");
            }
        } else {
            panic!("Not expected statement");
        };

        let stmt = &if_.consequence.statements[0];
        if let Statements::Expression(expr) = stmt {
            let expr = expr.expression.as_ref().unwrap();
            assert!(test_identifier(expr.clone(), "x".to_owned()));
        } else {
            panic!("Not expected statement");
        }

        assert!(if_.alternative.is_some());
        let alternative = if_.alternative.clone().unwrap();
        assert_eq!(alternative.statements.len(), 1);
        if let Statements::Expression(expr) = stmt {
            let expr = expr.expression.as_ref().unwrap();
            assert!(test_identifier(expr.clone(), "x".to_owned()));
        } else {
            panic!("Not expected statement");
        }
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x,y){x+y;}";

        let mut parser = Parser::new(input);
        let program = parser.parse_program();

        assert!(parser.errors.is_empty());
        assert_eq!(program.statements.len(), 1);

        let stmt = &program.statements[0];

        let fn_lit = if let Statements::Expression(fn_) = stmt {
            let fn_ = fn_.expression.as_ref().unwrap().clone();
            if let Expressions::Fn(fn_) = fn_ {
                fn_
            } else {
                panic!("Not expected expression");
            }
        } else {
            panic!("Not expected statement");
        };

        assert_eq!(fn_lit.parameters.len(), 2);

        assert!(test_literal_expression(
            Expressions::Identifier(fn_lit.parameters[0].clone()),
            Expected::String("x".to_owned())
        ));
        assert!(test_literal_expression(
            Expressions::Identifier(fn_lit.parameters[1].clone()),
            Expected::String("y".to_owned())
        ));

        assert_eq!(fn_lit.body.statements.len(), 1);
        let body_stmt = &fn_lit.body.statements[0];

        let body_expr = if let Statements::Expression(body) = body_stmt {
            body.expression.as_ref().unwrap().clone()
        } else {
            panic!("Not expected statement");
        };

        assert!(test_infix_expression(
            body_expr,
            Expected::String("x".to_owned()),
            "+".to_owned(),
            Expected::String("y".to_owned())
        ));
    }

    #[test]
    fn test_function_parameter_parsing() {
        struct ExpectedParams {
            input: &'static str,
            expected_params: Vec<&'static str>,
        }

        impl ExpectedParams {
            fn new(input: &'static str, expected_params: Vec<&'static str>) -> Self {
                ExpectedParams {
                    input,
                    expected_params,
                }
            }
        }

        let tests: [ExpectedParams; 3] = [
            ExpectedParams::new("fn(){}", Vec::new()),
            ExpectedParams::new("fn(x){}", vec!["x"]),
            ExpectedParams::new("fn(x, y, z){}", vec!["x", "y", "z"]),
        ];

        for t in tests.into_iter() {
            let mut parser = Parser::new(t.input);
            let program = parser.parse_program();
            assert!(parser.errors.is_empty());
            let stmt = &program.statements[0];
            let fn_ = if let Statements::Expression(expr) = stmt {
                let expr = expr.expression.as_ref().unwrap();
                if let Expressions::Fn(fn_) = expr {
                    fn_
                } else {
                    panic!("Not expected expression");
                }
            } else {
                panic!("Not expected statement");
            };

            assert_eq!(fn_.parameters.len(), t.expected_params.len());
            for (i, param) in fn_.parameters.iter().enumerate() {
                assert!(test_literal_expression(
                    Expressions::Identifier(param.clone()),
                    Expected::String(t.expected_params[i].to_owned())
                ));
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input: &str = "add(1, 2 * 3, 4 + 5)";

        let mut parser = Parser::new(input);
        let program = parser.parse_program();

        println!("Errors: {:?}", parser.errors);
        assert!(parser.errors.is_empty());
        assert_eq!(program.statements.len(), 1);

        let stmt = &program.statements[0];

        let call_expr = if let Statements::Expression(expr) = stmt {
            let expr = expr.expression.as_ref().unwrap().clone();
            if let Expressions::Call(call_expr) = expr {
                call_expr
            } else {
                panic!("Not expected expression");
            }
        } else {
            panic!("Not expected statement");
        };

        assert!(test_identifier(
            call_expr.function.clone(),
            "add".to_owned()
        ));

        assert_eq!(call_expr.arguments.len(), 3);

        assert!(test_literal_expression(
            call_expr.arguments[0].clone(),
            Expected::Int(1)
        ));

        assert!(test_infix_expression(
            call_expr.arguments[1].clone(),
            Expected::Int(2),
            "*".to_owned(),
            Expected::Int(3)
        ));

        assert!(test_infix_expression(
            call_expr.arguments[2].clone(),
            Expected::Int(4),
            "+".to_owned(),
            Expected::Int(5)
        ));
    }

    #[test]
    fn test_string_literal_expression() {
        let input: &str = "\"hello world\";";
        let mut parser = Parser::new(input);
        let program = parser.parse_program();

        assert!(parser.errors.is_empty());
        assert_eq!(program.statements.len(), 1);

        let stmt = &program.statements[0];

        let literal = if let Statements::Expression(expr) = stmt {
            let expr = expr.expression.as_ref().unwrap().clone();
            if let Expressions::String(literal) = expr {
                literal
            } else {
                panic!("Not expected expression");
            }
        } else {
            panic!("Not expected statement");
        };

        assert_eq!(literal.value, "hello world");
    }

    #[test]
    fn test_parse_array_literals() {
        let input: &str = "[1, 2 * 2, 3 + 3]";

        let mut parser = Parser::new(input);
        let program = parser.parse_program();

        assert!(parser.errors.is_empty());
        assert_eq!(program.statements.len(), 1);

        let stmt = &program.statements[0];

        let array = if let Statements::Expression(expr) = stmt {
            let expr = expr.expression.as_ref().unwrap().clone();
            if let Expressions::Array(array) = expr {
                array
            } else {
                panic!("Not expected expression");
            }
        } else {
            panic!("Not expected statement");
        };

        assert_eq!(array.elements.len(), 3);

        assert!(test_literal_expression(
            array.elements[0].clone(),
            Expected::Int(1)
        ));

        assert!(test_infix_expression(
            array.elements[1].clone(),
            Expected::Int(2),
            "*".to_owned(),
            Expected::Int(2)
        ));

        assert!(test_infix_expression(
            array.elements[2].clone(),
            Expected::Int(3),
            "+".to_owned(),
            Expected::Int(3)
        ));
    }

    #[test]
    fn test_parsing_index_expression() {
        let input: &str = "myArray[1 + 1]";

        let mut parser = Parser::new(input);
        let program = parser.parse_program();

        println!("Errors: {:?}", parser.errors);
        println!("Program: {:?}", program);
        assert!(parser.errors.is_empty());
        assert_eq!(program.statements.len(), 1);

        let stmt = &program.statements[0];

        let index_expr = if let Statements::Expression(expr) = stmt {
            let expr = expr.expression.as_ref().unwrap().clone();
            if let Expressions::Index(index_expr) = expr {
                index_expr
            } else {
                panic!("Not expected expression");
            }
        } else {
            panic!("Not expected statement");
        };

        assert!(test_identifier(
            index_expr.left.clone(),
            "myArray".to_owned()
        ));

        assert!(test_infix_expression(
            index_expr.index.clone(),
            Expected::Int(1),
            "+".to_owned(),
            Expected::Int(1)
        ));
    }

    #[test]
    fn test_parsing_index_expression_2() {
        let input: &str = "[1, 2, 3][1 + 1]";

        let mut parser = Parser::new(input);
        let program = parser.parse_program();

        println!("Errors: {:?}", parser.errors);
        println!("Program: {:?}", program);
        assert!(parser.errors.is_empty());
        assert_eq!(program.statements.len(), 1);

        let stmt = &program.statements[0];

        let index_expr = if let Statements::Expression(expr) = stmt {
            let expr = expr.expression.as_ref().unwrap().clone();
            if let Expressions::Index(index_expr) = expr {
                index_expr
            } else {
                panic!("Not expected expression");
            }
        } else {
            panic!("Not expected statement");
        };

        assert!(test_literal_expression(
            index_expr.left.clone(),
            Expected::Array(vec![Expected::Int(1), Expected::Int(2), Expected::Int(3)])
        ));

        assert!(test_infix_expression(
            index_expr.index.clone(),
            Expected::Int(1),
            "+".to_owned(),
            Expected::Int(1)
        ));
    }

    #[test]
    fn test_parsing_hash_literals_string_key() {
        let input: &str = r#"{"one": 1, "two": 2, "three": 3}"#;

        let mut parser = Parser::new(input);
        let program = parser.parse_program();

        assert!(parser.errors.is_empty());
        assert_eq!(program.statements.len(), 1);

        let stmt = &program.statements[0];

        let hash = if let Statements::Expression(expr) = stmt {
            let expr = expr.expression.as_ref().unwrap().clone();
            if let Expressions::HashMap(hash) = expr {
                hash
            } else {
                panic!("Not expected expression");
            }
        } else {
            panic!("Not expected statement");
        };

        assert_eq!(hash.pairs.0.len(), 3);

        let mut expected = HashMap::new();
        expected.insert("one", 1);
        expected.insert("two", 2);
        expected.insert("three", 3);
        for (_i, (key, value)) in hash.pairs.0.iter().enumerate() {
            let Expressions::String(str) = key else {
                panic!("Not expected expression");
            };

            let expected_value = expected.get(&str.value[..]).unwrap();

            assert!(test_integer_literal(value.clone(), *expected_value))
        }
    }

    #[test]
    fn test_parsing_empty_hash_literal() {
        let input: &str = "{}";

        let mut parser = Parser::new(input);
        let program = parser.parse_program();

        assert!(parser.errors.is_empty());
        assert_eq!(program.statements.len(), 1);

        let stmt = &program.statements[0];

        let hash = if let Statements::Expression(expr) = stmt {
            let expr = expr.expression.as_ref().unwrap().clone();
            if let Expressions::HashMap(hash) = expr {
                hash
            } else {
                panic!("Not expected expression");
            }
        } else {
            panic!("Not expected statement");
        };

        assert_eq!(hash.pairs.0.len(), 0);
    }

    #[test]
    fn test_parse_hash_literal_with_expressions() {
        let input: &str = r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#;

        let mut parser = Parser::new(input);
        let program = parser.parse_program();

        assert!(parser.errors.is_empty());
        assert_eq!(program.statements.len(), 1);

        let stmt = &program.statements[0];

        let hash = if let Statements::Expression(expr) = stmt {
            let expr = expr.expression.as_ref().unwrap().clone();
            if let Expressions::HashMap(hash) = expr {
                hash
            } else {
                panic!("Not expected expression");
            }
        } else {
            panic!("Not expected statement");
        };

        assert_eq!(hash.pairs.0.len(), 3);

        let mut expected_map: HashMap<String, Box<dyn Fn(Expressions)>> = HashMap::new();
        expected_map.insert(
            "one".to_string(),
            Box::new(|expr| {
                assert!(test_infix_expression(
                    expr,
                    Expected::Int(0),
                    "+".to_owned(),
                    Expected::Int(1)
                ))
            }),
        );
        expected_map.insert(
            "two".to_string(),
            Box::new(|expr| {
                assert!(test_infix_expression(
                    expr,
                    Expected::Int(10),
                    "-".to_owned(),
                    Expected::Int(8)
                ))
            }),
        );
        expected_map.insert(
            "three".to_string(),
            Box::new(|expr| {
                assert!(test_infix_expression(
                    expr,
                    Expected::Int(15),
                    "/".to_owned(),
                    Expected::Int(5)
                ))
            }),
        );

        for (_i, (key, value)) in hash.pairs.0.iter().enumerate() {
            let Expressions::String(str) = key else {
            panic!("Not expected expression");
        };

            let test_func = expected_map.get(&str.value[..]).unwrap();

            test_func(value.clone());
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        struct OperatorPrecendence {
            input: &'static str,
            expected: &'static str,
        }

        impl OperatorPrecendence {
            fn new(input: &'static str, expected: &'static str) -> Self {
                OperatorPrecendence { input, expected }
            }
        }

        let tests = [
            OperatorPrecendence::new("-a * b", "((-a) * b)"),
            OperatorPrecendence::new("!-a", "(!(-a))"),
            OperatorPrecendence::new("a + b + c", "((a + b) + c)"),
            OperatorPrecendence::new("a + b - c", "((a + b) - c)"),
            OperatorPrecendence::new("a * b * c", "((a * b) * c)"),
            OperatorPrecendence::new("a * b / c", "((a * b) / c)"),
            OperatorPrecendence::new("a + b / c", "(a + (b / c))"),
            OperatorPrecendence::new("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            OperatorPrecendence::new("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            OperatorPrecendence::new("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            OperatorPrecendence::new("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            OperatorPrecendence::new(
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            OperatorPrecendence::new("true", "true"),
            OperatorPrecendence::new("false", "false"),
            OperatorPrecendence::new("3 > 5 == false", "((3 > 5) == false)"),
            OperatorPrecendence::new("3 < 5 == true", "((3 < 5) == true)"),
            OperatorPrecendence::new("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            OperatorPrecendence::new("(5 + 5) * 2", "((5 + 5) * 2)"),
            OperatorPrecendence::new("2 / (5 + 5)", "(2 / (5 + 5))"),
            OperatorPrecendence::new("-(5 + 5)", "(-(5 + 5))"),
            OperatorPrecendence::new("!(true == true)", "(!(true == true))"),
            OperatorPrecendence::new("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            OperatorPrecendence::new(
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            OperatorPrecendence::new(
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            OperatorPrecendence::new(
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            OperatorPrecendence::new(
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        for t in tests.into_iter() {
            let mut parser = Parser::new(t.input);
            let program = parser.parse_program();
            assert!(parser.errors.is_empty());

            let actual = program.string();
            assert_eq!(actual, t.expected);
        }
    }
}
