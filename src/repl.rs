use std::io::BufRead;

use crate::lexer::{Lexer, Token};

const PROMPT: &str = ">>";

pub fn start(mut in_: impl std::io::BufRead, out: &mut impl std::io::Write) {
    loop {
        if out.write_all(PROMPT.as_bytes()).is_err() {
            return;
        }

        let mut line = String::new();
        in_.read_line(&mut line).unwrap();
        println!("{:?}", line);
        if line.is_empty() {
            return;
        }
        let mut lexer = Lexer::new(line);
        let mut token = lexer.next_token();
        out.write_fmt(format_args!("Token:{:?}\n", token)).unwrap();
        while token != Token::EOF {
            token = lexer.next_token();
            out.write_fmt(format_args!("Token:{:?}\n", token)).unwrap();
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::BufWriter;

    use super::*;

    #[test]
    fn test_start() {
        let input = "
if (5 < 10) { return true; }
else { return false; } 10 == 10; 10 != 9;
";
        let mut write_buffer = std::io::stdout();
        let reader = std::io::BufReader::new(input.as_bytes());
        start(reader, &mut write_buffer);
        println!("write_buffer = {:?}", write_buffer);
    }
}
