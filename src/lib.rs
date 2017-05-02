
mod ast;
mod eval;
mod syntax; // lalrpop

pub use eval::evaluate;
pub use syntax::{parse_Expression, parse_Program};

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

#[test]
fn test_program() {
    let source = "\
    BEGIN \
        a := 5; \
        b := a \
    END. \
    ";
    assert_eq!(parse_Program(source), Ok(Program(CompoundStatement(
        StatementList::Sequence(
            Statement::Empty,
            Box::new(StatementList::Single(Statement::Empty))
        )
    ))));
}

#[test]
fn test_evaluate_literal() {
    assert_eq!(evaluate(*parse_Expression("1234").unwrap()), Value::Number(1234));
    assert_eq!(evaluate(*parse_Expression("1234 + 5678").unwrap()), Value::Number(6912));
    assert_eq!(evaluate(*parse_Expression("(123 + 456) - 789").unwrap()), Value::Number(-210));
}

