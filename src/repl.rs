use crate::{
    evaluator::{eval, Eval},
    object::Object,
    parser::Parser,
};

const PROMPT: &str = ">>";

pub fn start(mut in_: impl std::io::BufRead, out: &mut String) {
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

        let evaluated = eval(&Eval::Program(program));

        out.push_str(&format!("{:?}\n", evaluated.inspect()));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_start() {
        let input = "
if ((1000 / 2) + 250 * 2 == 1000) { 9999 };
";
        let mut write_buffer = String::default();
        let reader = std::io::BufReader::new(input.as_bytes());
        start(reader, &mut write_buffer);
        println!("{}", write_buffer);
    }
}
