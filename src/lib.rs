
#![feature(slice_patterns)]

pub mod ast;
mod syntax; // lalrpop
pub mod intermediate;
pub mod exec;

#[cfg(test)]
use std::collections::HashMap;

pub use syntax::{parse_Expression, parse_Statement, parse_Program};
pub use intermediate::{transform_compound_statement, flatten_instruction_tree, jit};

#[cfg(test)]
use self::ast::*;

#[cfg(test)]
fn assert_ok<T, E>(r: &Result<T, E>) where E: std::fmt::Debug {
    match r {
        &Err(ref e) => panic!("Not Ok: {:?}", e),
        &Ok(_) => ()
    }
}

#[test]
fn test_expression_number() {
    assert_eq!(parse_Expression("1234"), Ok(Box::new(Expression::Lit(Literal::Number(1234)))));
}

#[test]
fn test_expression_group() {
    assert_eq!(parse_Expression("(1234)"), Ok(Box::new(Expression::Lit(Literal::Number(1234)))));
    // Arbitrarily nested
    assert_eq!(parse_Expression("(((1234)))"), Ok(Box::new(Expression::Lit(Literal::Number(1234)))));
    // Whitespace is ignored
    assert_eq!(parse_Expression("(  1234  )"), Ok(Box::new(Expression::Lit(Literal::Number(1234)))));
}

#[test]
fn test_expression_unary() {
    assert_eq!(parse_Expression("-3"), Ok(Box::new(Expression::ApplyUnOp(
        UnaryOperator::Minus,
        Box::new(Expression::Lit(Literal::Number(3)))
    ))))
}

#[test]
fn test_expression_infix_application() {
    assert_eq!(parse_Expression("1234 + 5678"), Ok(Box::new(Expression::ApplyInfixBinOp(
        Box::new(Expression::Lit(Literal::Number(1234))),
        InfixBinaryOperator::Add,
        Box::new(Expression::Lit(Literal::Number(5678))))
    )));
    // Mixes with groups
    assert!(parse_Expression("(123 + 456) - 789").is_ok());
}

#[test]
fn test_expression_infix_precedence() {
    assert_eq!(parse_Expression("1234 + 5678 * 3"), Ok(Box::new(Expression::ApplyInfixBinOp(
        Box::new(Expression::Lit(Literal::Number(1234))),
        InfixBinaryOperator::Add,
        Box::new(Expression::ApplyInfixBinOp(
            Box::new(Expression::Lit(Literal::Number(5678))),
            InfixBinaryOperator::Mul,
            Box::new(Expression::Lit(Literal::Number(3))))
        ))
    )));
}

#[test]
fn test_expression_infix_assoc() {
    assert_eq!(parse_Expression("1234 + 5678 + 3"), Ok(Box::new(Expression::ApplyInfixBinOp(
        Box::new(Expression::ApplyInfixBinOp(
            Box::new(Expression::Lit(Literal::Number(1234))),
            InfixBinaryOperator::Add,
            Box::new(Expression::Lit(Literal::Number(5678)))
        )),
        InfixBinaryOperator::Add,
        Box::new(Expression::Lit(Literal::Number(3)))
    ))));
    assert_eq!(parse_Expression("1234 + (5678 + 3)"), Ok(Box::new(Expression::ApplyInfixBinOp(
        Box::new(Expression::Lit(Literal::Number(1234))),
        InfixBinaryOperator::Add,
        Box::new(Expression::ApplyInfixBinOp(
            Box::new(Expression::Lit(Literal::Number(5678))),
            InfixBinaryOperator::Add,
            Box::new(Expression::Lit(Literal::Number(3)))
        ))
    ))));
}

mod programs {

    use syntax::parse_Program;
    use ast::Program;
    use std::fs::File;
    use std::io::prelude::*;
    use std::path::Path;

    #[cfg(test)]
    pub const SIMPLE_ASSIGNMENT: &str = "\
BEGIN \
    a := 5; \
    b := a \
END.\
    ";

    pub fn file_program(path: &str) -> Result<Program, String> {
        let path = Path::new(path);
        let mut file = File::open(&path).unwrap();
        let mut source = String::new();
        let _ = file.read_to_string(&mut source);
        match parse_Program(source.as_str()) {
            Ok(program) => Ok(program),
            Err(err) => Err(format!("Error parsing program: {:?}", err))
        }
    }

}

pub use programs::file_program;

#[cfg(test)]
fn assert_example_program(program_name: &str) {
    println!("Testing program {}", program_name);
    let program = programs::file_program(format!("examples/{}.drebbel", program_name).as_str());
    assert_ok(&program);
    let mut program: Program = program.unwrap();
    println!("  Transforming entry block");
    jit(&program.entry);
    for (ref sub_name, ref sub) in &program.subroutines {
        match sub.implementation {
            Implementation::Block(ref block) => {
                println!("  Transforming subroutine {:?} block", sub_name);
                jit(&block)
            },
            _ => panic!("Inconceivable!"),
        };
    }
    assert_ok(&exec::execute_program(&mut program))
}

#[test]
fn test_file_programs() {
    assert_example_program("program");
    assert_example_program("recursion");
    assert_example_program("loop");
    assert_example_program("generator");
    assert_example_program("exception");
    assert_example_program("references/simple");
    assert_example_program("references/lhs");
    assert_example_program("collections/array");
    assert_example_program("collections/hash");
}

#[test]
fn test_program() {
    assert_eq!(parse_Program(programs::SIMPLE_ASSIGNMENT), Ok(Program {
        subroutines: HashMap::new(),
        entry: CompoundStatement {
            statement_list: StatementList::Sequence(
                Statement::Assignment(Box::new(Expression::Var(VariableName(String::from("a")))), Box::new(Expression::Lit(Literal::Number(5)))),
                Box::new(StatementList::Single(
                    Statement::Assignment(Box::new(Expression::Var(VariableName(String::from("b")))), Box::new(Expression::Var(VariableName(String::from("a")))))
                ))
            )
        }
    }));
}
