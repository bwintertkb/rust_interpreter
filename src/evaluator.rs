use std::{cell::RefCell, collections::HashMap, rc::Rc};

use once_cell::sync::Lazy;

use crate::{
    ast::{
        ArrayLiteral, BlockStatement, Boolean as BooleanLiteral, CallExpression,
        ExpressionStatement, Expressions, FunctionLiteral, HashMapLiteral, Identifier,
        IfExpression, IndexExpression, InfixExpression, IntegerLiteral, LetStatement,
        PrefixExpression, Program, ReturnStatement, Statements, StringLiteral,
    },
    environment::{new_enclosed_environment, Environment},
    lexer::Token,
    object::{
        Array, Boolean, ErrorMonkey, Function, HashKey, HashMapMonkey, HashPair, Integer, Object,
        Objects, ReturnValue, StringObject, ARRAY_OBJ, BUILTINS, HASH_OBJ, INTEGER_OBJ, NULL,
        STRING_OBJ,
    },
};

static TRUE: Lazy<Boolean> = Lazy::new(|| Boolean::new(true));
static FALSE: Lazy<Boolean> = Lazy::new(|| Boolean::new(false));

#[derive(Debug, Clone)]
pub enum Eval {
    Program(Program),
    ExpressionStatement(ExpressionStatement),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    IfExpression(IfExpression),
    BlockStatement(BlockStatement),
    IntLit(IntegerLiteral),
    BoolLit(BooleanLiteral),
    ReturnStatement(ReturnStatement),
    LetStatement(LetStatement),
    Identifier(Identifier),
    Function(FunctionLiteral),
    CallExpression(CallExpression),
    String(StringLiteral),
    Array(ArrayLiteral),
    IndexExpression(IndexExpression),
    HashMap(HashMapLiteral),
}

impl From<ExpressionStatement> for Eval {
    fn from(expr: ExpressionStatement) -> Self {
        expr.expression.unwrap().into()
    }
}

impl From<Expressions> for Eval {
    fn from(value: Expressions) -> Self {
        match value {
            Expressions::Int(int) => Eval::IntLit(int),
            Expressions::Boolean(b) => Eval::BoolLit(b),
            Expressions::PrefixExpr(expr) => Eval::PrefixExpression(expr),
            Expressions::InfixExpr(expr) => Eval::InfixExpression(expr),
            Expressions::IfExpr(expr) => Eval::IfExpression(*expr),
            Expressions::Identifier(expr) => Eval::Identifier(expr),
            Expressions::Fn(expr) => Eval::Function(expr),
            Expressions::Call(expr) => Eval::CallExpression(*expr),
            Expressions::String(expr) => Eval::String(expr),
            Expressions::Array(expr) => Eval::Array(expr),
            Expressions::Index(idx) => Eval::IndexExpression(*idx),
            Expressions::HashMap(map_) => Eval::HashMap(map_),
            _ => {
                panic!("GOT UNSUPPORTED FROM<EXPRESSIONS>, received {:?}", value);
            }
        }
    }
}

impl From<HashKey> for Eval {
    fn from(value: HashKey) -> Self {
        match value {
            HashKey::Integer(int) => {
                Eval::IntLit(IntegerLiteral::new(Token::Int(int.value), int.value))
            }
            HashKey::Boolean(b) => {
                let bool_lit = if b.value {
                    BooleanLiteral::new(Token::True, true)
                } else {
                    BooleanLiteral::new(Token::False, false)
                };
                Eval::BoolLit(bool_lit)
            }
            HashKey::String(s) => Eval::String(StringLiteral::new(s.value)),
            _ => {
                panic!("GOT UNSUPPORTED FROM<HASHKEY>, received {:?}", value);
            }
        }
    }
}

impl From<BlockStatement> for Eval {
    fn from(value: BlockStatement) -> Self {
        Eval::BlockStatement(value)
    }
}

impl From<ReturnStatement> for Eval {
    fn from(value: ReturnStatement) -> Self {
        Eval::ReturnStatement(value)
    }
}

impl From<LetStatement> for Eval {
    fn from(value: LetStatement) -> Self {
        Eval::LetStatement(value)
    }
}

impl From<Identifier> for Eval {
    fn from(value: Identifier) -> Self {
        Eval::Identifier(value)
    }
}

pub fn eval(node: &Eval, env: &Rc<RefCell<Environment>>) -> Objects {
    match node {
        Eval::Identifier(ref ident) => eval_identifier(ident, env),
        Eval::Program(ref program) => eval_program(&program.statements, env),
        Eval::BlockStatement(ref block) => eval_block_statements(&block.statements, env),
        Eval::IfExpression(ref expr) => eval_if_expression(expr, env),
        Eval::ExpressionStatement(expr) => eval(&expr.clone().into(), env),
        Eval::PrefixExpression(expr) => {
            let right_expr = *expr.right.clone();
            let right = eval(&right_expr.into(), env);
            if right.is_error() {
                return right;
            }
            eval_prefix_expression(&expr.operator, right)
        }
        Eval::InfixExpression(expr) => {
            let left_expr = *expr.left.clone();
            let right_expr = *expr.right.clone();

            let left = eval(&left_expr.into(), env);
            if left.is_error() {
                return left;
            }
            let right = eval(&right_expr.into(), env);
            if right.is_error() {
                return right;
            }
            eval_infix_expression(&expr.operator, left, right)
        }
        Eval::IntLit(int) => Objects::Integer(Integer::new(int.value)),
        Eval::BoolLit(b) => Objects::Boolean(native_bool_to_boolean_obj(b.value)),
        Eval::ReturnStatement(stmt) => {
            let val = eval(&stmt.return_value.clone().unwrap().into(), env);
            if val.is_error() {
                return val;
            }
            Objects::ReturnValue(Box::new(ReturnValue::new(val)))
        }
        Eval::LetStatement(let_) => {
            let val = eval(&let_.value.clone().into(), env);
            if val.is_error() {
                return val;
            }

            env.borrow_mut().set(let_.name.string(), val);

            Objects::Null(&NULL)
        }
        Eval::Function(func) => {
            let params = func.parameters.clone();
            let body = func.body.clone();

            Objects::Function(Function::new(params, body, env.clone()))
        }
        Eval::CallExpression(expr) => {
            let func = eval(&expr.function.clone().into(), env);
            if func.is_error() {
                return func;
            }
            let args = eval_expressions(expr.arguments.clone(), env);
            if args.len() == 1 && args[0].is_error() {
                return args[0].clone();
            }

            apply_function(func, args)
        }
        Eval::String(str) => Objects::String(StringObject::new(&str.value)),
        Eval::Array(array) => {
            let elements = eval_expressions(array.elements.clone(), env);
            if elements.len() == 1 && elements[0].is_error() {
                return elements[0].clone();
            }
            Objects::Array(Array::new(elements))
        }
        Eval::IndexExpression(idx) => {
            let left = eval(&idx.left.clone().into(), env);
            if left.is_error() {
                return left;
            }

            let index = eval(&idx.index.clone().into(), env);
            if index.is_error() {
                return index;
            }

            eval_index_expression(left, index)
        }
        Eval::HashMap(map_) => eval_hash_literal(map_, env),
    }
}

fn new_error(message: &str) -> ErrorMonkey {
    ErrorMonkey::new(message)
}

fn eval_expressions(expressions: Vec<Expressions>, env: &Rc<RefCell<Environment>>) -> Vec<Objects> {
    let mut res: Vec<Objects> = Vec::new();

    for expr in expressions.iter() {
        let evaluated = eval(&expr.clone().into(), env);
        res.push(evaluated.clone());

        if evaluated.is_error() {
            return res;
        }
    }
    res
}

fn eval_hash_literal(map_: &HashMapLiteral, env: &Rc<RefCell<Environment>>) -> Objects {
    let mut pairs = HashMap::new();

    for (k, v) in map_.pairs.0.iter() {
        let key = eval(&k.clone().into(), env);
        if key.is_error() {
            return key;
        }

        let hash_key = match key {
            Objects::Integer(int) => HashKey::Integer(int),
            Objects::String(str) => HashKey::String(str),
            Objects::Boolean(bool_) => HashKey::Boolean(*bool_),
            _ => {
                return Objects::Error(new_error(&format!(
                    "unusable as hash key: {:?}",
                    key.obj_type()
                )))
            }
        };
        let value = eval(&v.clone().into(), env);
        if value.is_error() {
            return value;
        }

        pairs.insert(
            hash_key.clone(),
            HashPair::new(Objects::HashKey(hash_key), value),
        );
    }

    Objects::HashMap(HashMapMonkey::new(pairs))
}

fn eval_index_expression(left: Objects, index: Objects) -> Objects {
    if left.obj_type() == ARRAY_OBJ && index.obj_type() == INTEGER_OBJ {
        return eval_array_index_expression(left, index);
    }

    if left.obj_type() == HASH_OBJ {
        return eval_hash_index_expression(left, index);
    }

    Objects::Error(new_error(&format!(
        "index operator not supported: {:?}",
        left.obj_type()
    )))
}

fn eval_hash_index_expression(left: Objects, index: Objects) -> Objects {
    let Objects::HashMap(map_) = left else {
        return Objects::Error(new_error(&format!("HASH index operator not supported: {:?}", left.obj_type())));
    };

    let hash_key = match index {
        Objects::Integer(int) => HashKey::Integer(int),
        Objects::String(str) => HashKey::String(str),
        Objects::Boolean(bool_) => HashKey::Boolean(*bool_),
        _ => {
            return Objects::Error(new_error(&format!(
                "unusable as hash key: {:?}",
                index.obj_type()
            )))
        }
    };

    match map_.pairs.0.get(&hash_key) {
        Some(pair) => pair.value.clone(),
        None => Objects::Null(&NULL),
    }
}

fn eval_array_index_expression(left: Objects, index: Objects) -> Objects {
    let Objects::Array(arr) = left else {
        return Objects::Error(new_error(&format!("ARRAY index operator not supported: {:?}", left.obj_type())));
    };

    let Objects::Integer(int) = index else {
        return Objects::Error(new_error(&format!("INTEGER index operator not supported: {:?}", index.obj_type())));
    };

    let idx = int.value;

    if idx < 0 || idx >= arr.elements.len() as i64 {
        return Objects::Null(&NULL);
    }

    arr.elements[idx as usize].clone()
}

fn apply_function(func: Objects, args: Vec<Objects>) -> Objects {
    match func {
        Objects::Function(func) => {
            let extended_env = extend_function_env(&func, args);
            let evaluated = eval(&func.body.into(), &extended_env);
            unwrap_return_value(evaluated)
        }
        Objects::Builtin(b) => (b.func)(args),
        _ => Objects::Error(new_error(&format!("not a function: {:?}", func.obj_type()))),
    }
}

fn extend_function_env(func: &Function, args: Vec<Objects>) -> Rc<RefCell<Environment>> {
    let env = new_enclosed_environment(func.env.0.clone());
    for (idx, ident) in func.parameters.iter().enumerate() {
        env.borrow_mut().set(ident.value.clone(), args[idx].clone());
    }
    env
}

fn unwrap_return_value(obj: Objects) -> Objects {
    if let Objects::ReturnValue(rtrn) = obj {
        return rtrn.value;
    }
    obj
}

fn eval_identifier(ident: &Identifier, env: &Rc<RefCell<Environment>>) -> Objects {
    if let Some(val) = env.borrow().get(&ident.string()) {
        return val;
    }

    if let Some(f) = BUILTINS.get(&ident.value) {
        return Objects::Builtin(f.clone());
    }

    Objects::Error(new_error(&format!(
        "identifier not found: {}",
        ident.string()
    )))
}

fn eval_program(statements: &[Statements], env: &Rc<RefCell<Environment>>) -> Objects {
    let mut result = Objects::Null(&NULL);
    for s in statements.iter() {
        let expr = match s {
            Statements::Expression(expr) => Eval::from(expr.expression.clone().unwrap()),
            Statements::Return(rtrn) => {
                let eval_return = Eval::from(rtrn.clone());
                let res = eval(&eval_return, env);
                return res;
            }
            Statements::Let(expr) => Eval::from(expr.clone()),
            _ => panic!("Expected ExpressionStatement. Received {:?}", s),
        };
        let res = eval(&expr, env);
        if res.is_error() {
            return res;
        }

        result = res;
    }
    result
}

fn eval_block_statements(block: &[Statements], env: &Rc<RefCell<Environment>>) -> Objects {
    let mut result = Objects::Null(&NULL);

    for s in block.iter() {
        let expr = match s {
            Statements::Expression(expr) => Eval::from(expr.expression.clone().unwrap()),
            Statements::Return(expr) => Eval::from(expr.clone()),
            Statements::Let(expr) => Eval::from(expr.clone()),
            _ => panic!("Expected ExpressionStatement"),
        };

        result = eval(&expr, env);

        if !result.is_null() && (result.is_error() || result.is_return()) {
            if let Objects::ReturnValue(ref r) = result {
                if r.value.is_error() {
                    return r.value.clone();
                }
            }
            return result;
        }
    }
    result
}

pub fn native_bool_to_boolean_obj(input: bool) -> &'static Lazy<Boolean> {
    if input {
        return &TRUE;
    }
    &FALSE
}

fn eval_prefix_expression(operator: &str, right: Objects) -> Objects {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Objects::Error(new_error(&format!(
            "unknown operator: {}{}",
            operator,
            right.obj_type()
        ))),
    }
}

fn eval_infix_expression(operator: &str, left: Objects, right: Objects) -> Objects {
    if left.obj_type() == INTEGER_OBJ && right.obj_type() == INTEGER_OBJ {
        let Objects::Integer(int_left) = left else {
            return Objects::Null(&NULL);
        };
        let Objects::Integer(int_right) = right else {
            return Objects::Null(&NULL);
        };
        return eval_integer_infix_expression(operator, int_left, int_right);
    }

    if left.obj_type() != right.obj_type() {
        return Objects::Error(new_error(&format!(
            "type mismatch: {} {} {}",
            left.obj_type(),
            operator,
            right.obj_type()
        )));
    }

    if left.obj_type() == STRING_OBJ && right.obj_type() == STRING_OBJ {
        let Objects::String(str_left) = left else {
            return Objects::Null(&NULL);
        };
        let Objects::String(str_right) = right else {
            return Objects::Null(&NULL);
        };
        return eval_string_infix_expression(operator, str_left.value, str_right.value);
    }

    match operator {
        "==" => {
            return Objects::Boolean(native_bool_to_boolean_obj(left == right));
        }
        "!=" => {
            return Objects::Boolean(native_bool_to_boolean_obj(left != right));
        }
        _ => Objects::Error(new_error(&format!(
            "unknown operator: {} {} {}",
            left.obj_type(),
            operator,
            right.obj_type()
        ))),
    }
}

fn eval_string_infix_expression(operator: &str, mut left: String, right: String) -> Objects {
    match operator {
        "+" => {
            left.push_str(&right);
            Objects::String(StringObject::new(&left))
        }
        "==" => Objects::Boolean(native_bool_to_boolean_obj(left == right)),
        "!=" => Objects::Boolean(native_bool_to_boolean_obj(left != right)),
        "*" => {
            let mut new_str = String::with_capacity(left.len().max(right.len()));
            let left_lower = left.to_ascii_lowercase();
            let right_lower = right.to_ascii_lowercase();
            for (c_l, c_r) in left_lower.chars().zip(right_lower.chars()) {
                let v = (c_l as u32) % (c_r as u32);
                new_str.push(char::from_u32(v).unwrap());
            }
            Objects::String(StringObject::new(&new_str))
        }
        _ => Objects::Error(new_error(&format!(
            "unknown operator: STRING {} STRING",
            operator
        ))),
    }
}

fn eval_integer_infix_expression(operator: &str, left: Integer, right: Integer) -> Objects {
    match operator {
        "+" => Objects::Integer(Integer::new(left.value + right.value)),
        "-" => Objects::Integer(Integer::new(left.value - right.value)),
        "*" => Objects::Integer(Integer::new(left.value * right.value)),
        "/" => Objects::Integer(Integer::new(left.value / right.value)),
        "<" => Objects::Boolean(native_bool_to_boolean_obj(left.value < right.value)),
        ">" => Objects::Boolean(native_bool_to_boolean_obj(left.value > right.value)),
        "==" => Objects::Boolean(native_bool_to_boolean_obj(left.value == right.value)),
        "!=" => Objects::Boolean(native_bool_to_boolean_obj(left.value != right.value)),
        _ => Objects::Error(new_error(&format!(
            "unknown operator: {} {} {}",
            left.obj_type(),
            operator,
            right.obj_type()
        ))),
    }
}

fn eval_if_expression(expr: &IfExpression, env: &Rc<RefCell<Environment>>) -> Objects {
    let expr_cond: Eval = expr.condition.clone().into();
    let condition = eval(&expr_cond, env);
    //

    if condition.is_error() {
        return condition;
    }

    if is_truthy(&condition) {
        return eval(&expr.consequence.clone().into(), env);
    } else if expr.alternative.is_some() {
        return eval(&expr.alternative.clone().unwrap().into(), env);
    }
    Objects::Null(&NULL)
}

fn eval_bang_operator_expression(right: Objects) -> Objects {
    match right {
        Objects::Boolean(b) => {
            if b.value {
                return Objects::Boolean(&FALSE);
            }
            Objects::Boolean(&TRUE)
        }
        Objects::Null(_) => Objects::Boolean(&TRUE),
        _ => Objects::Boolean(&FALSE),
    }
}

fn eval_minus_prefix_operator_expression(right: Objects) -> Objects {
    let int = if let Objects::Integer(int) = right {
        int
    } else {
        return Objects::Error(new_error(&format!(
            "unknown operator: -{}",
            right.obj_type()
        )));
    };
    let v = int.value;
    Objects::Integer(Integer::new(-v))
}

fn is_truthy(obj: &Objects) -> bool {
    match obj {
        Objects::Null(_) => false,
        Objects::Boolean(b) => {
            if b.value {
                return true;
            }
            false
        }
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{object::HashKey, parser::Parser};

    use super::*;

    fn test_eval(input: &'static str) -> Objects {
        let mut parser = Parser::new(input);
        let program = parser.parse_program();
        let env = Environment::new();
        eval(&Eval::Program(program), &env)
    }

    fn test_integer_object(obj: Objects, expected: i64) -> bool {
        let int_type = match obj {
            Objects::Integer(int) => int,
            _ => return false,
        };
        if int_type.value != expected {
            println!(
                "object has wrong value. got={}, want={}",
                int_type.value, expected
            );
            return false;
        }

        true
    }

    fn test_boolean_object(obj: Objects, expected: bool) -> bool {
        let bool_type = match obj {
            Objects::Boolean(bool_) => bool_,
            _ => return false,
        };

        if bool_type.value != expected {
            println!(
                "object has wrong value. got={}, want={}",
                bool_type.value, expected
            );
            return false;
        }

        true
    }

    fn test_null_object(obj: Objects) -> bool {
        matches!(obj, Objects::Null(_))
    }

    #[test]
    fn test_eval_integer_expression() {
        struct TestInt {
            input: &'static str,
            expected: i64,
        }

        impl TestInt {
            fn new(input: &'static str, expected: i64) -> Self {
                TestInt { input, expected }
            }
        }

        let tests: [TestInt; 15] = [
            TestInt::new("5", 5),
            TestInt::new("10", 10),
            TestInt::new("-5", -5),
            TestInt::new("-10", -10),
            TestInt::new("5+5+5+5-10", 10),
            TestInt::new("2*2*2*2*2", 32),
            TestInt::new("-50+100+-50", 0),
            TestInt::new("5*2+10", 20),
            TestInt::new("5+2*10", 25),
            TestInt::new("20+2*-10", 0),
            TestInt::new("50/2*2+10", 60),
            TestInt::new("2*(5+10)", 30),
            TestInt::new("3*3*3+10", 37),
            TestInt::new("3*(3*3) + 10", 37),
            TestInt::new("(5+10*2+15/3)*2+-10", 50),
        ];

        for t in tests.into_iter() {
            let evaluated = test_eval(t.input);

            assert!(test_integer_object(evaluated, t.expected));
        }
    }

    #[test]
    fn test_eval_bool_expression() {
        struct TestBool {
            input: &'static str,
            expected: bool,
        }

        impl TestBool {
            fn new(input: &'static str, expected: bool) -> Self {
                TestBool { input, expected }
            }
        }

        let tests: [TestBool; 18] = [
            TestBool::new("true", true),
            TestBool::new("false", false),
            TestBool::new("1 < 2", true),
            TestBool::new("1 > 2", false),
            TestBool::new("1 < 1", false),
            TestBool::new("1 > 1", false),
            TestBool::new("1 == 1", true),
            TestBool::new("1 != 1", false),
            TestBool::new("1 == 2", false),
            TestBool::new("1 != 2", true),
            TestBool::new("true == true", true),
            TestBool::new("false == false", true),
            TestBool::new("true == false", false),
            TestBool::new("true != false", true),
            TestBool::new("false != true", true),
            TestBool::new("(1 < 2) == true", true),
            TestBool::new("(1 < 2) == false", false),
            TestBool::new("(1 > 2) == true", false),
        ];

        for t in tests.into_iter() {
            let evaluated = test_eval(t.input);

            assert!(test_boolean_object(evaluated, t.expected));
        }
    }

    #[test]
    fn test_bang_operator() {
        struct TestBang {
            input: &'static str,
            expected: bool,
        }

        impl TestBang {
            fn new(input: &'static str, expected: bool) -> Self {
                TestBang { input, expected }
            }
        }

        let tests: [TestBang; 6] = [
            TestBang::new("!true", false),
            TestBang::new("!false", true),
            TestBang::new("!5", false),
            TestBang::new("!!true", true),
            TestBang::new("!!false", false),
            TestBang::new("!!5", true),
        ];

        for t in tests.into_iter() {
            let evaluated = test_eval(t.input);
            assert!(test_boolean_object(evaluated, t.expected));
        }
    }

    #[test]
    fn test_ifelse_expressions() {
        enum Expected {
            Integer(i64),
            Null,
        }

        struct IfElseTest {
            input: &'static str,
            expected: Expected,
        }

        let tests: [IfElseTest; 7] = [
            IfElseTest {
                input: "if (true) { 10 }",
                expected: Expected::Integer(10),
            },
            IfElseTest {
                input: "if (false) { 10 }",
                expected: Expected::Null,
            },
            IfElseTest {
                input: "if (1) { 10 }",
                expected: Expected::Integer(10),
            },
            IfElseTest {
                input: "if (1 < 2) { 10 }",
                expected: Expected::Integer(10),
            },
            IfElseTest {
                input: "if (1 > 2) { 10 }",
                expected: Expected::Null,
            },
            IfElseTest {
                input: "if (1 > 2) { 10 } else { 20 }",
                expected: Expected::Integer(20),
            },
            IfElseTest {
                input: "if (1 < 2) { 10 } else { 20 }",
                expected: Expected::Integer(10),
            },
        ];

        for t in tests.into_iter() {
            let eval = test_eval(t.input);
            match t.expected {
                Expected::Integer(int) => {
                    assert!(test_integer_object(eval, int));
                }
                Expected::Null => {
                    assert!(test_null_object(eval));
                }
            }
        }
    }

    #[test]
    fn test_return_statements() {
        struct ReturnTest {
            input: &'static str,
            expected: i64,
        }

        impl ReturnTest {
            fn new(input: &'static str, expected: i64) -> Self {
                ReturnTest { input, expected }
            }
        }

        let tests = [
            ReturnTest::new("return 10;", 10),
            ReturnTest::new("return 10; 9;", 10),
            ReturnTest::new("return 2 * 5; 9;", 10),
            ReturnTest::new("9; return 2 * 5; 9;", 10),
            ReturnTest::new(
                r#"
            if (10 > 1) {
                if (10 > 1) {
                    return 10;
                }
            
                return 1;
            }
            "#,
                10,
            ),
        ];

        for t in tests.into_iter() {
            let evaluated = test_eval(t.input);
            let int = match evaluated {
                Objects::ReturnValue(rtrn) => {
                    if let Objects::Integer(int) = rtrn.value {
                        int
                    } else {
                        panic!("Expected integer, got {:?}", rtrn.value);
                    }
                }
                Objects::Integer(int) => int,
                _ => panic!("Expected return value, got {:?}", evaluated),
            };
            assert!(test_integer_object(Objects::Integer(int), t.expected));
        }
    }

    #[test]
    fn test_error_handling() {
        struct TestError {
            input: &'static str,
            expected_message: &'static str,
        }

        impl TestError {
            fn new(input: &'static str, expected_message: &'static str) -> Self {
                TestError {
                    input,
                    expected_message,
                }
            }
        }

        let tests = [
            TestError::new("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            TestError::new("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            TestError::new("-true", "unknown operator: -BOOLEAN"),
            TestError::new("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            TestError::new("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            TestError::new(
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            TestError::new(
                r#"if (10 > 1) {
                    if (10 > 1) {
                        return true + false;
                    }
                    return 1;
                }"#,
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            TestError::new("foobar", "identifier not found: foobar"),
            TestError::new(r#""Hello" - "World""#, "unknown operator: STRING - STRING"),
            TestError::new(
                r#"{"name":"Monkey"}[fn(x) {x}];"#,
                r#"unusable as hash key: "FUNCTION""#,
            ),
        ];

        for t in tests.into_iter() {
            let evaluated = test_eval(t.input);
            match evaluated {
                Objects::Error(err) => {
                    assert_eq!(err.message, t.expected_message);
                }
                _ => panic!("Expected error, got {:?}", evaluated),
            }
        }
    }

    #[test]
    fn test_let_statements() {
        struct LetTest {
            input: &'static str,
            expected: i64,
        }

        impl LetTest {
            fn new(input: &'static str, expected: i64) -> Self {
                LetTest { input, expected }
            }
        }

        let tests = [
            LetTest::new("let a = 5; a;", 5),
            LetTest::new("let a = 5 * 5; a;", 25),
            LetTest::new("let a = 5; let b = a; b;", 5),
            LetTest::new("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for t in tests.into_iter() {
            let evaluated = test_eval(t.input);
            assert!(test_integer_object(evaluated, t.expected));
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";
        let evaluated = test_eval(input);
        match evaluated {
            Objects::Function(func) => {
                assert_eq!(func.parameters.len(), 1);
                assert_eq!(func.parameters[0].string(), "x");
                assert_eq!(func.body.string(), "(x + 2)");
            }
            _ => panic!("Expected function, got {:?}", evaluated),
        }
    }

    #[test]
    fn test_function_application() {
        struct TestFunction {
            input: &'static str,
            expected: i64,
        }

        impl TestFunction {
            fn new(input: &'static str, expected: i64) -> Self {
                TestFunction { input, expected }
            }
        }

        let tests = [
            TestFunction::new("let identity = fn(x) { x; }; identity(5);", 5),
            TestFunction::new("let identity = fn(x) { return x; }; identity(5);", 5),
            TestFunction::new("let double = fn(x) { x * 2; }; double(5);", 10),
            TestFunction::new("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            TestFunction::new("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            TestFunction::new("fn(x) { x; }(5)", 5),
        ];

        for t in tests.into_iter() {
            let evaluated = test_eval(t.input);
            assert!(test_integer_object(evaluated, t.expected));
        }
    }

    #[test]
    fn test_closure() {
        let input = r#"
        let newAdder = fn(x) {
            fn(y) { x + y };
        };
    
        let addTwo = newAdder(2);
        addTwo(2);
        "#;

        assert!(test_integer_object(test_eval(input), 4));
    }

    #[test]
    fn test_function_recursion() {
        let input = r#"
        let countDown = fn(x) {
            if (x == 0) {
                100
            } else {
                countDown(x - 1)
            }
        };
        countDown(3);
        "#;

        assert!(test_integer_object(test_eval(input), 100));
    }

    #[test]
    fn test_string_literal() {
        let input = r#""Hello World!""#;
        let evaluated = test_eval(input);
        match evaluated {
            Objects::String(string) => {
                assert_eq!(string.value, "Hello World!");
            }
            _ => panic!("Expected string, got {:?}", evaluated),
        }
    }

    #[test]
    fn test_string_concatenation() {
        let input = r#""Hello" + " " + "World!""#;
        let evaluated = test_eval(input);
        match evaluated {
            Objects::String(string) => {
                assert_eq!(string.value, "Hello World!");
            }
            _ => panic!("Expected string, got {:?}", evaluated),
        }
    }

    #[test]
    fn test_builtin_functions() {
        enum Expected {
            Int(i64),
            String(&'static str),
        }

        struct TestBuiltin {
            input: &'static str,
            expected: Expected,
        }

        impl TestBuiltin {
            fn new(input: &'static str, expected: Expected) -> Self {
                TestBuiltin { input, expected }
            }
        }

        let tests = [
            TestBuiltin::new(r#"len("")"#, Expected::Int(0)),
            TestBuiltin::new(r#"len("four")"#, Expected::Int(4)),
            TestBuiltin::new(r#"len("hello world")"#, Expected::Int(11)),
            TestBuiltin::new(
                r#"len(1)"#,
                Expected::String("argument to `len` not supported, got INTEGER"),
            ),
            TestBuiltin::new(
                r#"len("one", "two")"#,
                Expected::String("wrong number of arguments. got=2, want=1"),
            ),
        ];

        for t in tests.into_iter() {
            let evaluated = test_eval(t.input);
            match t.expected {
                Expected::Int(expected) => {
                    assert!(test_integer_object(evaluated, expected));
                }
                Expected::String(expected) => match evaluated {
                    Objects::Error(err) => {
                        assert_eq!(err.message, expected);
                    }
                    _ => panic!("Expected error, got {:?}", evaluated),
                },
            }
        }
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        let evaluated = test_eval(input);
        match evaluated {
            Objects::Array(array) => {
                assert_eq!(array.elements.len(), 3);
                assert!(test_integer_object(array.elements[0].clone(), 1));
                assert!(test_integer_object(array.elements[1].clone(), 4));
                assert!(test_integer_object(array.elements[2].clone(), 6));
            }
            _ => panic!("Expected array, got {:?}", evaluated),
        }
    }

    #[test]
    fn test_array_index_expressions() {
        enum ExpectRes {
            Int(i64),
            Null,
        }

        struct TestArrayIndex {
            input: &'static str,
            expected: ExpectRes,
        }

        impl TestArrayIndex {
            fn new(input: &'static str, expected: ExpectRes) -> Self {
                TestArrayIndex { input, expected }
            }
        }

        let tests = [
            TestArrayIndex::new("[1, 2, 3][0]", ExpectRes::Int(1)),
            TestArrayIndex::new("[1, 2, 3][1]", ExpectRes::Int(2)),
            TestArrayIndex::new("[1, 2, 3][2]", ExpectRes::Int(3)),
            TestArrayIndex::new("let i = 0; [1][i];", ExpectRes::Int(1)),
            TestArrayIndex::new("[1, 2, 3][1 + 1];", ExpectRes::Int(3)),
            TestArrayIndex::new("let myArray = [1, 2, 3]; myArray[2];", ExpectRes::Int(3)),
            TestArrayIndex::new(
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                ExpectRes::Int(6),
            ),
            TestArrayIndex::new(
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                ExpectRes::Int(2),
            ),
            TestArrayIndex::new("[1, 2, 3][3]", ExpectRes::Null),
            TestArrayIndex::new("[1, 2, 3][-1]", ExpectRes::Null),
        ];

        for t in tests.into_iter() {
            let evaluated = test_eval(t.input);
            match t.expected {
                ExpectRes::Int(expected) => {
                    assert!(test_integer_object(evaluated, expected));
                }
                ExpectRes::Null => {
                    assert_eq!(evaluated, Objects::Null(&NULL));
                }
            }
        }
    }

    #[test]
    fn test_hash_literals() {
        let input = r#"
        let two = "two";
        {
            "one": 10 - 9,
            two: 1 + 1,
            "thr" + "ee": 6 / 2,
            4: 4,
            true: 5,
            false: 6
        }
        "#;

        let evaluated = test_eval(input);
        match evaluated {
            Objects::HashMap(hash) => {
                let mut expected = HashMap::new();
                expected.insert(HashKey::String(StringObject::new("one")), 1);
                expected.insert(HashKey::String(StringObject::new("two")), 2);
                expected.insert(HashKey::String(StringObject::new("three")), 3);
                expected.insert(HashKey::Integer(Integer::new(4)), 4);
                expected.insert(HashKey::Boolean(Boolean::new(true)), 5);
                expected.insert(HashKey::Boolean(Boolean::new(false)), 6);

                assert_eq!(hash.pairs.0.len(), expected.len());

                for (key, value) in expected.iter() {
                    let pair = hash.pairs.0.get(key);
                    assert!(pair.is_some());
                    assert!(test_integer_object(pair.unwrap().clone().value, *value));
                }
            }
            _ => panic!("Expected hash, got {:?}", evaluated),
        }
    }

    #[test]
    fn test_hash_index_expressions() {
        enum ExpectRes {
            Int(i64),
            Null,
        }

        struct TestHashIndex {
            input: &'static str,
            expected: ExpectRes,
        }

        impl TestHashIndex {
            fn new(input: &'static str, expected: ExpectRes) -> Self {
                TestHashIndex { input, expected }
            }
        }

        let tests = [
            TestHashIndex::new(r#"{"foo": 5}["foo"]"#, ExpectRes::Int(5)),
            TestHashIndex::new(r#"{"foo": 5}["bar"]"#, ExpectRes::Null),
            TestHashIndex::new(r#"let key = "foo"; {"foo": 5}[key]"#, ExpectRes::Int(5)),
            TestHashIndex::new(r#"{}["foo"]"#, ExpectRes::Null),
            TestHashIndex::new(r#"{5: 5}[5]"#, ExpectRes::Int(5)),
            TestHashIndex::new(r#"{true: 5}[true]"#, ExpectRes::Int(5)),
            TestHashIndex::new(r#"{false: 5}[false]"#, ExpectRes::Int(5)),
        ];

        for t in tests.into_iter() {
            let evaluated = test_eval(t.input);
            match t.expected {
                ExpectRes::Int(expected) => {
                    assert!(test_integer_object(evaluated, expected));
                }
                ExpectRes::Null => {
                    assert_eq!(evaluated, Objects::Null(&NULL));
                }
            }
        }
    }
}
