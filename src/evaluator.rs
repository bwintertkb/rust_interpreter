use once_cell::sync::Lazy;

use crate::{
    ast::{
        BlockStatement, Boolean as BooleanLiteral, ExpressionStatement, Expressions, IfExpression,
        InfixExpression, IntegerLiteral, PrefixExpression, Program, ReturnStatement, Statements,
    },
    object::{
        Boolean, Integer, Null, Object, Objects, ReturnValue, INTEGER_OBJ, NULL, NULL_OBJ,
        RETURN_VALUE_OBJ,
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
            _ => {
                panic!("GOT UNSUPPORTED FROM<EXPRESSIONS>, received {:?}", value);
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

pub fn eval(node: &Eval) -> Objects {
    match node {
        Eval::Program(ref program) => eval_program(&program.statements),
        Eval::BlockStatement(ref block) => eval_block_statements(&block.statements),
        Eval::IfExpression(ref expr) => eval_if_expression(expr),
        Eval::ExpressionStatement(expr) => eval(&expr.clone().into()),
        Eval::PrefixExpression(expr) => {
            let right_expr = *expr.right.clone();
            let right = eval(&right_expr.into());
            eval_prefix_expression(&expr.operator, right)
        }
        Eval::InfixExpression(expr) => {
            let right_expr = *expr.right.clone();
            let left_expr = *expr.left.clone();
            let right = eval(&right_expr.into());
            let left = eval(&left_expr.into());
            eval_infix_expression(&expr.operator, left, right)
        }
        Eval::IntLit(int) => Objects::Integer(Integer::new(int.value)),
        Eval::BoolLit(b) => Objects::Boolean(native_bool_to_boolean_obj(b.value)),
        Eval::ReturnStatement(stmt) => {
            let val = eval(&stmt.return_value.clone().unwrap().into());
            Objects::ReturnValue(Box::new(ReturnValue::new(val)))
        }
    }
}

fn eval_program(statements: &[Statements]) -> Objects {
    let mut result = Objects::Null(&NULL);
    for s in statements.iter() {
        let expr = match s {
            Statements::Expression(expr) => Eval::from(expr.expression.clone().unwrap()),
            Statements::Return(rtrn) => {
                let eval_return = Eval::from(rtrn.clone());
                let res = eval(&eval_return);
                return res;
            }
            _ => panic!("Expected ExpressionStatement. Received {:?}", s),
        };
        let res = eval(&expr);
        result = res;
    }
    result
}

fn eval_block_statements(block: &[Statements]) -> Objects {
    let mut result = Objects::Null(&NULL);

    for s in block.iter() {
        let expr = match s {
            Statements::Expression(expr) => Eval::from(expr.expression.clone().unwrap()),
            Statements::Return(expr) => Eval::from(expr.clone()),
            _ => panic!("Expected ExpressionStatement"),
        };

        result = eval(&expr);

        if !result.is_null() && result.obj_type() == RETURN_VALUE_OBJ {
            println!("Returning {:?}", result);
            return result;
        }
    }
    result
}

fn native_bool_to_boolean_obj(input: bool) -> &'static Lazy<Boolean> {
    if input {
        return &TRUE;
    }
    &FALSE
}

fn eval_prefix_expression(operator: &str, right: Objects) -> Objects {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Objects::Null(&NULL),
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

    match operator {
        "==" => {
            return Objects::Boolean(native_bool_to_boolean_obj(left == right));
        }
        "!=" => {
            return Objects::Boolean(native_bool_to_boolean_obj(left != right));
        }
        _ => Objects::Null(&NULL),
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
        _ => Objects::Null(&NULL),
    }
}

fn eval_if_expression(expr: &IfExpression) -> Objects {
    let expr_cond: Eval = expr.condition.clone().into();
    let condition = eval(&expr_cond);

    if is_truthy(&condition) {
        return eval(&expr.consequence.clone().into());
    } else if expr.alternative.is_some() {
        return eval(&expr.alternative.clone().unwrap().into());
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
        return Objects::Null(&NULL);
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
    use crate::parser::Parser;

    use super::*;

    fn test_eval(input: &'static str) -> Objects {
        let mut parser = Parser::new(input);
        let program = parser.parse_program();
        eval(&Eval::Program(program))
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
}
