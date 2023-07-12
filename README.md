# Monkey Interpreter in Rust

This is an implementation of the Monkey programming language interpreter written in Rust, based on the book [Writing an Interpreter in Go](https://interpreterbook.com/) by Thorsten Ball.

## About Monkey Programming Language

The Monkey programming language is a simple language used as an educational tool to understand how interpreters work. Monkey features C-style syntax, and supports variable bindings, prefix and infix operators, control flow constructs, built-in functions, first-class and higher-order functions, and closures.

## Project Structure

This project is organized into the following main sections:

- `src/lexer.rs`: Contains the code for breaking Monkey source code into tokens.
- `src/parser.rs`: Converts the output of `lexer.rs` into an Abstract Syntax Tree (AST).
- `src/evaluator.rs`: Evaluates the AST produced by `parser.rs` to execute Monkey programs.
- `src/object.rs`: Contains the implementation for the objects that Monkey programs can manipulate.
- `src/main.rs`: The entry point of the interpreter that ties everything together.
