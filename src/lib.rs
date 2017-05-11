
pub mod ast;
mod syntax; // lalrpop
pub mod eval;
pub mod intermediate;

#[cfg(test)]
use std::collections::HashMap;

pub use syntax::{parse_Expression, parse_Statement, parse_Program};
pub use eval::{Scope, evaluate_expression, evaluate_statement, evaluate_program};
pub use intermediate::{transform_compound_statement, flatten_instruction_tree};

#[cfg(test)]
use self::ast::*;

#[cfg(test)]
use self::eval::Value;

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

    pub fn file_program(path: &str) -> Program {
        let path = Path::new(path);
        let mut file = File::open(&path).unwrap();
        let mut source = String::new();
        let _ = file.read_to_string(&mut source);
        match parse_Program(source.as_str()) {
            Ok(program) => program,
            Err(err) => panic!("{:?}", err)
        }
    }

}

pub use programs::file_program;

#[test]
fn test_program() {
    assert_eq!(parse_Program(programs::SIMPLE_ASSIGNMENT), Ok(Program {
        subroutines: HashMap::new(),
        entry: CompoundStatement {
            statement_list: StatementList::Sequence(
                Statement::Assignment(VariableName(String::from("a")), Box::new(Expression::Lit(Literal::Number(5)))),
                Box::new(StatementList::Single(
                    Statement::Assignment(VariableName(String::from("b")), Box::new(Expression::Var(VariableName(String::from("a")))))
                ))
            )
        }
    }));
}

#[cfg(test)]
fn assert_example_program(program_name: &str) {
    let program = programs::file_program(format!("examples/{}.drebbel", program_name).as_str());
    let result_scope = evaluate_program(program);
    assert_ok(&result_scope);
    println!("{:?}", result_scope.unwrap())
}

#[test]
fn test_file_programs() {
    assert_example_program("program");
    assert_example_program("recursion");
}

#[test]
fn test_evaluate_literal() {
    let empty_program = &HashMap::new();
    let mut empty_scope = Scope::default();
    assert_eq!(evaluate_expression(empty_program, &mut empty_scope, &*parse_Expression("1234").unwrap()), Ok(Value::Number(1234)));
    assert_eq!(evaluate_expression(empty_program, &mut empty_scope, &*parse_Expression("1234 + 5678").unwrap()), Ok(Value::Number(6912)));
    assert_eq!(evaluate_expression(empty_program, &mut empty_scope, &*parse_Expression("(123 + 456) - 789").unwrap()), Ok(Value::Number(-210)));
}

