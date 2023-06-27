use crate::{
    environment::Environment,
    evaluator::{eval, Eval},
    object::Object,
    parser::Parser,
};

const PROMPT: &str = ">>";

pub fn start(mut in_: impl std::io::BufRead, out: &mut String) {
    let mut env = Environment::default();
    loop {
        let mut line = String::new();
        in_.read_line(&mut line).unwrap();
        if line.is_empty() {
            return;
        }
        let mut parser = Parser::new(&line);
        let program = parser.parse_program();

        if !parser.errors().is_empty() {
            parser.print_errors();
            continue;
        }

        let evaluated = eval(&Eval::Program(program), &mut env);
        if !evaluated.is_null() {
            out.push_str(&format!("{:?}\n", evaluated.inspect()));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_start() {
        let input = "
let a = 4;
let b = a > 3;
let c = a * 10;
c;
b;
a;
";
        let mut write_buffer = String::default();
        let reader = std::io::BufReader::new(input.as_bytes());
        start(reader, &mut write_buffer);
        println!("{}", write_buffer);
    }
}
