use std::{
    collections::HashMap,
    error::Error,
    fmt::{Debug, Display},
};

use once_cell::sync::Lazy;

use crate::{
    ast::{
        Boolean, ExpressionStatement, Expressions, Identifier, InfixExpression, IntegerLiteral,
        LetStatement, PrefixExpression, Program, ReturnStatement, StatementStruct, Statements,
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

        parser.register_infix(Token::Plus.token_literal());
        parser.register_infix(Token::Minus.token_literal());
        parser.register_infix(Token::Slash.token_literal());
        parser.register_infix(Token::Asterisk.token_literal());
        parser.register_infix(Token::Equal.token_literal());
        parser.register_infix(Token::NEqual.token_literal());
        parser.register_infix(Token::LessThan.token_literal());
        parser.register_infix(Token::GreaterThan.token_literal());

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

    pub fn parse_integer_literal(&self) -> IntegerLiteral {
        let curr_token = self.curr_token.as_ref().unwrap();
        let value = curr_token.literal().parse::<i64>().unwrap();
        IntegerLiteral::new(curr_token.clone(), value)
    }

    pub fn parse_boolean(&self) -> Boolean {
        let curr_token = self.curr_token.as_ref().unwrap().clone();
        Boolean::new(curr_token, self.curr_token_is(Token::True))
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

    pub fn parse_expression(&mut self, precedence: usize) -> Option<Expressions> {
        let token = self.curr_token.as_ref().unwrap().clone();
        let prefix_fn = match self.parser_prefix_fn.fns.get(&token.token_literal()) {
            Some(fn_) => fn_,
            None => {
                self.no_prefix_parse_fn_error(&token);
                return None;
            }
        };

        let mut left_expr = prefix_fn(self);
        //

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
        Token::Int(_) => Expressions::Int(p.parse_integer_literal()),
        Token::True | Token::False => Expressions::Boolean(p.parse_boolean()),
        Token::Minus | Token::Bang => Expressions::PrefixExpr(p.parse_prefix_expression()),
        _ => panic!("Not implemented"),
    }
}

fn parse_infix_expression(p: &mut Parser, expression: Expressions) -> Expressions {
    let infix = p.parse_infix_expression(expression);
    Expressions::InfixExpr(infix)
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
    use crate::{ast::Node, parser};

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

    //     #[test]
    //     fn test_let_statements() {
    //         let input = "
    //         let x = 5;
    //         let y = 10;
    //         let foobar = 838383;
    //     let 7718;
    //         ";
    //
    //         let mut parser = Parser::new(input);
    //         let program = parser.parse_program();
    //
    //         assert_eq!(program.statements.len(), 4);
    //
    //         let tests: Vec<ExpectedIdentifier> = vec![
    //             ExpectedIdentifier::new("x".to_owned()),
    //             ExpectedIdentifier::new("y".to_owned()),
    //             ExpectedIdentifier::new("foobar".to_owned()),
    //         ];
    //
    //         let zipped: Vec<_> = program.statements.iter().zip(tests.iter()).collect();
    //
    //         for (stmt, test) in zipped {
    //             if let Statements::Let(let_) = stmt {
    //                 assert_eq!(let_.name.value, test.identifier);
    //             }
    //         }
    //     }
    //
    //     #[test]
    //     fn test_no_token_err() {
    //         let input = "";
    //         let mut parser = Parser::new(input);
    //         parser.parse_program();
    //
    //         assert_eq!(parser.errors.len(), 1);
    //     }
    //
    //     #[test]
    //     fn test_return_statements() {
    //         let input = "
    // return 5;
    // return 10;
    // return 993322;
    // ";
    //         let mut parser = Parser::new(input);
    //         let program = parser.parse_program();
    //
    //         assert!(parser.errors.is_empty());
    //
    //         let tests: Vec<ExpectedIdentifier> = vec![
    //             ExpectedIdentifier::new("".to_owned()),
    //             ExpectedIdentifier::new("".to_owned()),
    //             ExpectedIdentifier::new("".to_owned()),
    //         ];
    //
    //         let zipped: Vec<_> = program.statements.iter().zip(tests.iter()).collect();
    //
    //         for (stmt, test) in zipped {
    //             if let Statements::Let(let_) = stmt {
    //                 assert_eq!(let_.name.value, test.identifier);
    //             }
    //         }
    //     }
    //
    //     #[test]
    //     fn test_identifier_expression() {
    //         let input = "foobar;";
    //
    //         let mut parser = Parser::new(input);
    //         let program = parser.parse_program();
    //
    //         assert!(parser.errors.is_empty());
    //         let stmt = &program.statements[0];
    //         if let Statements::Expression(expr) = stmt {
    //             let ident = expr.expression.clone().unwrap();
    //             if let Expressions::Identifier(ident) = ident {
    //                 assert_eq!(ident.value, "foobar");
    //                 assert_eq!(ident.token.literal(), "foobar");
    //             } else {
    //                 panic!("Not expected expression");
    //             }
    //             // assert_eq!(ident.value, "foobar");
    //         } else {
    //             panic!("Not expected statement");
    //         }
    //     }

    // #[test]
    // fn test_integer_literal_expression() {
    //     let input = "5;";
    //
    //     let mut parser = Parser::new(input);
    //     let program = parser.parse_program();
    //     println!("{:?}", parser);
    //
    //     assert!(parser.errors.is_empty());
    //
    //     let stmt = &program.statements[0];
    //     println!("STATEMENT: {:?}", stmt);
    //     if let Statements::Expression(expr) = stmt {
    //         let int_lit = expr.expression.clone().unwrap();
    //         if let Expressions::Int(int) = int_lit {
    //             assert_eq!(int.value, 5);
    //             assert_eq!(int.token_literal(), "5");
    //         } else {
    //             panic!("Not expected expression");
    //         }
    //     } else {
    //         panic!("Not expected statement");
    //     }
    // }

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

    // #[test]
    // fn test_parsing_prefix_expressions() {
    //     struct PrefixTest {
    //         input: String,
    //         operator: String,
    //         integer_value: i64,
    //     }
    //
    //     impl PrefixTest {
    //         fn new(input: String, operator: String, integer_value: i64) -> Self {
    //             PrefixTest {
    //                 input,
    //                 operator,
    //                 integer_value,
    //             }
    //         }
    //     }
    //
    //     let prefix_tests: [PrefixTest; 2] = [
    //         PrefixTest::new("!5;".to_owned(), "!".to_owned(), 5),
    //         PrefixTest::new("-15".to_owned(), "-".to_owned(), 15),
    //     ];
    //
    //     for pt in prefix_tests.into_iter() {
    //         let mut parser = Parser::new(&pt.input);
    //         let program = parser.parse_program();
    //
    //         println!("ERRORS: {:?}", parser.errors);
    //         assert!(parser.errors.is_empty());
    //         println!("statements: {:?}", program.statements);
    //         assert_eq!(program.statements.len(), 1);
    //
    //         let stmt = &program.statements[0];
    //
    //         if let Statements::Expression(expr) = stmt {
    //             let prefix_expr = expr.expression.clone().unwrap();
    //             if let Expressions::PrefixExpr(prefix_expr) = prefix_expr {
    //                 assert_eq!(prefix_expr.operator, pt.operator);
    //                 assert!(test_integer_literal(*prefix_expr.right, pt.integer_value));
    //             } else {
    //                 panic!("Not expected expression");
    //             }
    //         } else {
    //             panic!("Not expected statement");
    //         }
    //     }
    // }

    // #[test]
    // fn test_parsing_infix_expressions() {
    //     // Infix <expression> <infix operator> <expression>
    //     struct InfixTest {
    //         input: String,
    //         left_value: i64,
    //         operator: String,
    //         right_value: i64,
    //     }
    //
    //     impl InfixTest {
    //         fn new(input: String, left_value: i64, operator: String, right_value: i64) -> Self {
    //             InfixTest {
    //                 input,
    //                 left_value,
    //                 operator,
    //                 right_value,
    //             }
    //         }
    //     }
    //
    //     let infix_tests: [InfixTest; 8] = [
    //         InfixTest::new("5 + 5".to_owned(), 5, "+".to_owned(), 5),
    //         InfixTest::new("5 - 5".to_owned(), 5, "-".to_owned(), 5),
    //         InfixTest::new("5 * 5".to_owned(), 5, "*".to_owned(), 5),
    //         InfixTest::new("5 / 5".to_owned(), 5, "/".to_owned(), 5),
    //         InfixTest::new("5 > 5".to_owned(), 5, ">".to_owned(), 5),
    //         InfixTest::new("5 < 5".to_owned(), 5, "<".to_owned(), 5),
    //         InfixTest::new("5 == 5".to_owned(), 5, "==".to_owned(), 5),
    //         InfixTest::new("5 != 5".to_owned(), 5, "!=".to_owned(), 5),
    //     ];
    //
    //     for infix in infix_tests.into_iter() {
    //         let mut parser = Parser::new(&infix.input);
    //         let program = parser.parse_program();
    //
    //         println!("ERRORS: {:?}", parser.errors);
    //         assert!(parser.errors.is_empty());
    //         println!("statements: {:?}", program.statements);
    //         assert_eq!(program.statements.len(), 1);
    //
    //         let stmt = &program.statements[0];
    //
    //         if let Statements::Expression(expr) = stmt {
    //             let infix_expr = expr.expression.clone().unwrap();
    //             if let Expressions::InfixExpr(expr) = infix_expr {
    //                 assert!(test_integer_literal(*expr.left, infix.left_value));
    //                 assert_eq!(expr.operator, infix.operator);
    //                 assert!(test_integer_literal(*expr.right, infix.right_value));
    //             } else {
    //                 panic!("Not expected expression");
    //             }
    //         } else {
    //             panic!("Not expected statement");
    //         }
    //     }
    // }
    //
    fn test_identifier(expr: Expressions, value: String) -> bool {
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

    enum Expected {
        Int(i64),
        String(String),
    }

    fn test_literal_expression(expr: Expressions, expected: Expected) -> bool {
        match expected {
            Expected::Int(int) => test_integer_literal(expr, int),
            Expected::String(str) => test_identifier(expr, str),
        }
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

        let tests: [OperatorPrecendence; 12] = [
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
