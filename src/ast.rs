use std::fmt::Write;

use crate::{lexer::Token, parser::Iota};

pub trait Node {
    fn token_literal(&self) -> String;
    fn string(&self) -> String;
}

#[derive(Debug)]
pub struct StatementStruct {
    tokens: Vec<Token>,
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

impl Program {
    pub fn string(&self) -> String {
        let mut string: String = String::new();

        self.statements.iter().for_each(|s| {
            let res = match s {
                Statements::Let(n) => n.string(),
                Statements::Return(n) => n.string(),
                Statements::Expression(n) => n.string(),
            };

            string.write_str(&res).unwrap();
        });

        string
    }
}

#[derive(Debug)]
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

    fn string(&self) -> String {
        self.value.clone()
    }
}

impl Expression for Identifier {
    fn expression_node<T: Node>(&self) -> T {
        todo!()
    }
}

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Token,
}

impl LetStatement {
    //page 67
    pub fn new(identifier: Identifier, expression: Token) -> Self {
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

    fn string(&self) -> String {
        let mut string: String = String::new();

        string.write_str(&self.token_literal()).unwrap();
        string.write_char(' ').unwrap();
        string.write_str(&self.name.string()).unwrap();
        string.write_str(" = ").unwrap();
        if self.value != Token::EOF || self.value != Token::Illegal {
            string.write_str(&self.value.literal()).unwrap();
        }
        string.write_char(';').unwrap();
        string
    }
}

#[derive(Debug)]
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

    fn string(&self) -> String {
        let mut string: String = String::new();

        string.write_str(&self.token_literal()).unwrap();
        string.write_char(' ').unwrap();
        if !self.return_value.is_empty() {
            string.write_str(&self.return_value).unwrap();
        }
        string.write_char(';').unwrap();
        string
    }
}

#[derive(Debug)]
pub struct ExpressionStatement {
    token: Token, // the first token of the expression
    expression: Iota,
}

impl ExpressionStatement {
    pub fn new(token: Token, expression: Iota) -> Self {
        ExpressionStatement { token, expression }
    }
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal()
    }

    fn string(&self) -> String {
        (self.expression as usize).to_string()
    }
}

#[derive(Debug)]
pub enum Statements {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

pub struct ExpressionValue;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_program_string() {
        // Represents let myVar = anotherVar;
        let expected = "let myVar = anotherVar;";
        let mut program = Program::default();

        let identifier = Identifier::new(Token::Ident("myVar".to_owned()), "myVar".to_owned());
        let let_statement = LetStatement::new(identifier, Token::Ident("anotherVar".to_owned()));
        program.statements = vec![Statements::Let(let_statement)];
        let actual = program.string();
        assert_eq!(actual, expected);

        // let statements = vec![]
    }
}
