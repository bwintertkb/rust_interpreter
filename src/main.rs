use std::io::{BufRead, BufReader};

use rust_interpreter::{
    environment::Environment,
    evaluator::{eval, Eval},
    parser::Parser,
};

fn main() {
    let file = std::fs::File::open("./main.banana").unwrap();
    let mut source = String::with_capacity(file.metadata().unwrap().len() as usize);
    let reader = BufReader::new(file);
    for line in reader.lines() {
        source.push_str(&line.unwrap());
    }
    let mut env = Environment::new();
    let mut parser = Parser::new(&source);
    let program = parser.parse_program();
    eval(&Eval::Program(program), &env);

    // reader.lines()
}
