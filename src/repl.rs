use std::io::BufRead;

const PROMPT: &str = ">>";

pub fn start(in_: impl std::io::Read, mut out: impl std::io::Write) {
    let mut read_buffer = std::io::BufReader::new(in_);

    loop {
        if out.write_all(PROMPT.as_bytes()).is_err() {
            return;
        }

        let mut line = String::new();
        read_buffer.read_line(&mut line);
        println!("{:?}", line);
        if line.is_empty() {
            return;
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
        let write_buffer = BufWriter::new(Vec::new());
        let reader = std::io::BufReader::new(input.as_bytes());
        start(reader, write_buffer);
    }
}
