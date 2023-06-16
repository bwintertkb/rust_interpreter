use crate::lexer::Token;

pub trait Node {
    fn token_literal(&self) -> String;
}

#[derive(Debug)]
pub struct StatementStruct {
    pub tokens: Vec<Token>,
}

impl StatementStruct {
    pub fn new(tokens: Vec<Token>) -> Self {
        StatementStruct { tokens }
    }
}

pub trait Statement {
    fn statement_node<T: Node>(&self) -> T;
}

pub trait Expression {
    fn expression_node<T: Node>(&self) -> T;
}

// pub struct Program<T: Statement> {
//     statements: Vec<T>,
// }
//
// impl<T: Statement + Node> Node for Program<T> {
//     fn token_literal(&self) -> String {
//         if !self.statements.is_empty() {
//             return self.statements[0].token_literal();
//         }
//
//         String::new()
//     }
// }

#[derive(Debug, Default)]
pub struct Program {
    pub statements: Vec<Statements>,
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Identifier {
    pub fn new(token: Token, value: String) -> Self {
        Identifier { token, value }
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

impl Expression for Identifier {
    fn expression_node<T: Node>(&self) -> T {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl IntegerLiteral {
    pub fn new(token: Token, value: i64) -> Self {
        IntegerLiteral { token, value }
    }
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: String,
}

impl LetStatement {
    pub fn new(identifier: Identifier, expression: String) -> Self {
        LetStatement {
            token: Token::Let,
            name: identifier,
            value: expression,
        }
    }
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    token: Token, // The return token.
    return_value: String,
}

impl ReturnStatement {
    pub fn new(return_value: String) -> Self {
        ReturnStatement {
            token: Token::Return,
            return_value,
        }
    }
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Expressions>,
}

impl ExpressionStatement {
    pub fn new(token: Token) -> Self {
        ExpressionStatement {
            token,
            expression: None,
        }
    }
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

#[derive(Debug, Clone)]
pub enum Statements {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

pub struct ExpressionValue;

#[derive(Debug, Clone)]
pub enum Expressions {
    Identifier(Identifier),
    Int(IntegerLiteral),
    TODO,
}
