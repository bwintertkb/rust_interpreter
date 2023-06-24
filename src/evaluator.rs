use std::any::Any;

#[cfg(test)]
mod tests {
    use crate::{
        object::{Integer, Object},
        parser::Parser,
    };

    use super::*;

    fn test_eval(input: &'static str) {
        let mut parser = Parser::new(input);
        let program = parser.parse_program();
        // eval(program)
    }

    fn test_integer_object(obj: &dyn Any, expected: i64) -> bool {
        let int_type = match obj.downcast_ref::<Integer>() {
            Some(int) => int,
            None => return false,
        };

        true
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

        let tests: [TestInt; 2] = [TestInt::new("5", 5), TestInt::new("10", 10)];

        for t in tests.into_iter() {
            let evaluated = test_eval(t.input);
            // assert!(test_integer_object(&evaluated, t.expected));
        }
    }
}
