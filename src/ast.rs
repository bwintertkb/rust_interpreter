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

impl Program {
    pub fn string(&self) -> String {
        let mut string_buffer = String::new();

        self.statements.iter().for_each(|s| {
            string_buffer.push_str(&s.string());
        });

        string_buffer
    }
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

    pub fn string(&self) -> String {
        self.value.clone()
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

    pub fn string(&self) -> String {
        self.token.literal()
    }
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

#[derive(Debug, Clone)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expressions>,
}

impl PrefixExpression {
    pub fn new(token: Token, operator: String, right: Expressions) -> Self {
        PrefixExpression {
            token,
            operator,
            right: Box::new(right),
        }
    }

    pub fn string(&self) -> String {
        format!("({}{})", self.operator, self.right.string())
    }
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

#[derive(Debug, Clone)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expressions>,
    pub operator: String,
    pub right: Box<Expressions>,
}

impl InfixExpression {
    pub fn new(token: Token, left: Expressions, operator: String, right: Expressions) -> Self {
        InfixExpression {
            token,
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }

    pub fn string(&self) -> String {
        format!(
            "({} {} {})",
            self.left.string(),
            self.operator,
            self.right.string()
        )
    }
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

#[derive(Debug, Clone)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl Boolean {
    pub fn new(token: Token, value: bool) -> Self {
        Boolean { token, value }
    }

    pub fn string(&self) -> String {
        self.token.literal()
    }
}

impl Node for Boolean {
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

    pub fn string(&self) -> String {
        let mut string_buffer = format!("{} {} = ", self.token_literal(), self.name.string());

        if !self.value.is_empty() {
            string_buffer.push_str(&self.value);
        }

        string_buffer.push(';');
        string_buffer
    }
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.token_literal()
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

    pub fn string(&self) -> String {
        let mut string_buffer = format!("{} ", self.token_literal());

        if !self.return_value.is_empty() {
            string_buffer.push_str(&self.return_value);
        }
        string_buffer
    }
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.token_literal()
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

    pub fn string(&self) -> String {
        if let Some(ref expr) = self.expression {
            return expr.string();
        }
        "".to_owned()
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

impl Statements {
    pub fn string(&self) -> String {
        match self {
            Statements::Let(stmt) => stmt.string(),
            Statements::Return(stmt) => stmt.string(),
            Statements::Expression(stmt) => stmt.string(),
        }
    }
}

pub struct ExpressionValue;

#[derive(Debug, Clone)]
pub enum Expressions {
    Identifier(Identifier),
    Int(IntegerLiteral),
    Boolean(Boolean),
    PrefixExpr(PrefixExpression),
    InfixExpr(InfixExpression),
    TODO,
}

impl Expressions {
    pub fn string(&self) -> String {
        match self {
            Expressions::Identifier(ident) => ident.token_literal(),
            Expressions::Int(int) => int.token_literal(),
            Expressions::PrefixExpr(prefix) => prefix.string(),
            Expressions::InfixExpr(infix) => infix.string(),
            Expressions::Boolean(bool_) => bool_.string(),
            _ => panic!("Not implemented"),
        }
    }
}
