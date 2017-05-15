extern crate drebbel;
extern crate linefeed;

use drebbel::*;
use std::env::args;
use drebbel::ast::{Implementation, Builtin, SubroutineName, Subroutine, VariableName};

fn compile(filename: &str) {
    let program = file_program(filename).unwrap();

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
    let mut program = file_program(filename).unwrap();

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
