
mod ast;
mod eval;
mod syntax; // lalrpop

#[cfg(test)]
use std::collections::HashMap;

pub use eval::{Scope, evaluate_expression, evaluate_statement, evaluate_program};
pub use syntax::{parse_Expression, parse_Statement, parse_Program};

#[cfg(test)]
use self::ast::*;

#[cfg(test)]
use self::eval::Value;

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

    #[cfg(test)]
    use std::fs::File;
    #[cfg(test)]
    use std::io::prelude::*;
    #[cfg(test)]
    use std::path::Path;

    #[cfg(test)]
    pub const SIMPLE_ASSIGNMENT: &str = "\
BEGIN \
    a := 5; \
    b := a \
END.\
    ";

    #[cfg(test)]
    pub fn file_program(path: &str) -> String {
        let path = Path::new(path);
        let mut file = File::open(&path).unwrap();
        let mut source = String::new();
        let _ = file.read_to_string(&mut source);
        source
    }

}

#[test]
fn test_program() {
    assert_eq!(parse_Program(programs::SIMPLE_ASSIGNMENT), Ok(Program {
        subroutines: HashMap::new(),
        entry: CompoundStatement(
            StatementList::Sequence(
                Statement::Assignment(VariableName(String::from("a")), Box::new(Expression::Lit(Literal::Number(5)))),
                Box::new(StatementList::Single(
                    Statement::Assignment(VariableName(String::from("b")), Box::new(Expression::Var(VariableName(String::from("a")))))
                ))
            )
        )
    }));
}

#[test]
fn test_file_program() {
    let program_source = programs::file_program("examples/program.drebbel");
    let file_program = parse_Program(program_source.as_str());
    assert!(file_program.is_ok());
    let result_scope = evaluate_program(file_program.unwrap());
    assert!(result_scope.is_ok());
    println!("{:?}", result_scope.unwrap())
}

#[test]
fn test_evaluate_literal() {
    let empty_program = &HashMap::new();
    let mut empty_scope = Scope::default();
    assert_eq!(evaluate_expression(empty_program, &mut empty_scope, &*parse_Expression("1234").unwrap()), Ok(Value::Number(1234)));
    assert_eq!(evaluate_expression(empty_program, &mut empty_scope, &*parse_Expression("1234 + 5678").unwrap()), Ok(Value::Number(6912)));
    assert_eq!(evaluate_expression(empty_program, &mut empty_scope, &*parse_Expression("(123 + 456) - 789").unwrap()), Ok(Value::Number(-210)));
}

