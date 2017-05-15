extern crate drebbel;
extern crate linefeed;

use linefeed::{Reader, ReadResult};
use std::collections::HashMap;
use drebbel::*;
use std::env::args;
use drebbel::ast::{Implementation, Builtin, SubroutineName, Subroutine, VariableName};

fn repl() {
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

fn compile(filename: &str) {
    let program = file_program(filename);

    for (ref sub_name, ref subroutine) in program.subroutines.iter() {

        match subroutine.implementation {
            Implementation::Block(ref compound_statement) => {
                println!("\n{:?}", sub_name);
                let sub_instructions = flatten_instruction_tree(transform_compound_statement(compound_statement));
                for insn in sub_instructions {
                    println!("{:?}", insn);
                }
            },
            _ => ()
        }

    }

    println!("\nMain");
    let main_instructions = flatten_instruction_tree(transform_compound_statement(&program.entry));
    for insn in main_instructions {
        println!("{:?}", insn);
    }
}

fn exec(filename: &str) {
    let mut program = file_program(filename);

    program.subroutines.insert(SubroutineName(String::from("print")), Subroutine{
        arguments: vec!(VariableName(String::from("value"))),
        implementation: Implementation::Builtin(Builtin::Print)
    });

    match exec::execute_program(&program) {
        Ok(()) => println!("Program terminated cleanly"),
        Err(err) => println!("Program crashed with error {:?}", err)
    }
}

fn main() {
    let argv: Vec<String> = args().collect();

    println!("Args: {:?}", argv);

    let command = argv[1].as_str();
    match command {
        "repl" => repl(),
        "compile" => {
            let filename = argv[2].as_str();
            compile(filename)
        },
        "exec" => {
            let filename = argv[2].as_str();
            exec(filename)
        }
        _ => panic!("Unknown command {}", command)
    }
}
