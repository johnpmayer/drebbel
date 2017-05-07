extern crate drebbel;
extern crate linefeed;

use linefeed::{Reader, ReadResult};
use std::collections::HashMap;
use drebbel::*;

fn main() {
    println!("Hello, drebbel!");

    let empty_program = &HashMap::new();

    let mut reader = Reader::new("drebbel").unwrap();

    let mut repl_scope = drebbel::Scope::default();

    reader.set_prompt("drebbel> ");

    while let Ok(ReadResult::Input(input)) = reader.read_line() {
        println!("got input {:?}", input);

        if let Ok(expr) = parse_Expression(input.as_str()) {
            println!("Got expression {:?}", expr);
            let value = evaluate_expression(empty_program, &mut repl_scope, &*expr);
            println!("Evaluates to {:?}", value)
        } else if let Ok(stmt) = parse_Statement(input.as_str()) {
            println!("Got statement {:?}", stmt);
            let result = evaluate_statement(empty_program, &mut repl_scope, &stmt);
            println!("{:?}", result)
        } else {
            println!("Syntax error")
        }

    }

    println!("Goodbye.");
}
