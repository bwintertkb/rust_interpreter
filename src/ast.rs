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

#[derive(Debug, Clone, Default)]
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

    pub fn take_statements(&mut self) -> Vec<Statements> {
        std::mem::take(&mut self.statements)
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
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl FunctionLiteral {
    pub fn new(parameters: Vec<Identifier>, body: BlockStatement) -> Self {
        FunctionLiteral {
            token: Token::Function,
            parameters,
            body,
        }
    }

    pub fn string(&self) -> String {
        let mut string_buffer = String::new();

        let params: Vec<String> = self.parameters.iter().map(|p| p.string()).collect();

        string_buffer.push_str(&self.token_literal());
        string_buffer.push('(');
        string_buffer.push_str(&params.join(", "));
        string_buffer.push_str(") ");
        string_buffer.push_str(&self.body.string());

        string_buffer
    }
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    pub token: Token,          // The '(' token
    pub function: Expressions, //Identifier or Function literal
    pub arguments: Vec<Expressions>,
}

impl CallExpression {
    pub fn new(function: Expressions, arguments: Vec<Expressions>) -> Self {
        CallExpression {
            token: Token::LParen,
            function,
            arguments,
        }
    }

    pub fn string(&self) -> String {
        let mut string_buffer = String::new();

        let args: Vec<String> = self.arguments.iter().map(|a| a.string()).collect();

        string_buffer.push_str(&self.function.string());
        string_buffer.push('(');
        string_buffer.push_str(&args.join(", "));
        string_buffer.push(')');

        string_buffer
    }
}

impl Node for CallExpression {
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
pub struct IfExpression {
    pub token: Token,
    pub condition: Expressions,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl IfExpression {
    pub fn new(
        condition: Expressions,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    ) -> Self {
        IfExpression {
            token: Token::If,
            condition,
            consequence,
            alternative,
        }
    }

    pub fn string(&self) -> String {
        let mut string_buffer = format!(
            "if{} {}",
            self.condition.string(),
            self.consequence.string()
        );

        if let Some(alternative) = &self.alternative {
            string_buffer.push_str(&format!("else {}", alternative.string()));
        }

        string_buffer
    }
}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        self.token.token_literal()
    }
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statements>,
}

impl BlockStatement {
    pub fn new(token: Token, statements: Vec<Statements>) -> Self {
        BlockStatement { token, statements }
    }

    pub fn string(&self) -> String {
        let mut string_buffer = String::new();

        self.statements.iter().for_each(|s| {
            string_buffer.push_str(&s.string());
        });

        string_buffer
    }
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.token_literal()
    }
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Expressions,
}

impl LetStatement {
    pub fn new(identifier: Identifier, value: Expressions) -> Self {
        LetStatement {
            token: Token::Let,
            name: identifier,
            value,
        }
    }

    pub fn string(&self) -> String {
        let mut string_buffer = format!("{} {} = ", self.token_literal(), self.name.string());

        string_buffer.push_str(&self.value.string());

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
    pub token: Token, // The return token.
    pub return_value: Option<Expressions>,
}

impl ReturnStatement {
    pub fn new(return_value: Option<Expressions>) -> Self {
        ReturnStatement {
            token: Token::Return,
            return_value,
        }
    }

    pub fn string(&self) -> String {
        let mut string_buffer = format!("{} ", self.token_literal());

        if self.return_value.is_some() {
            string_buffer.push_str(&self.return_value.as_ref().unwrap().string());
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

impl Node for Statements {
    fn token_literal(&self) -> String {
        match self {
            Statements::Let(stmt) => stmt.token.token_literal(),
            Statements::Return(stmt) => stmt.token.token_literal(),
            Statements::Expression(stmt) => stmt.token.token_literal(),
        }
    }
}

pub struct ExpressionValue;

#[derive(Debug, Clone)]
pub enum Expressions {
    Identifier(Identifier),
    Int(IntegerLiteral),
    Boolean(Boolean),
    IfExpr(Box<IfExpression>),
    PrefixExpr(PrefixExpression),
    InfixExpr(InfixExpression),
    Fn(FunctionLiteral),
    Call(Box<CallExpression>),
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
            Expressions::IfExpr(if_expr) => if_expr.string(),
            Expressions::Fn(fn_expr) => fn_expr.string(),
            Expressions::Call(call_expr) => call_expr.string(),
            _ => panic!("Not implemented"),
        }
    }
}
