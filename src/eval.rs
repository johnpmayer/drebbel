
use ast::{Expression, InfixBinaryOperator, Literal, UnaryOperator};

#[derive(Debug, PartialEq)]
pub enum Value {
    Number(i64)
}

pub fn evaluate(expr: Expression) -> Value {
    match expr {
        Expression::Lit(Literal::Number(n)) => Value::Number(n),
        Expression::ApplyUnOp(UnaryOperator::Minus, expr) => {
            let Value::Number(n1) = evaluate(*expr);
            Value::Number(- n1)
        }
        Expression::ApplyInfixBinOp(expr1, InfixBinaryOperator::Add, expr2) => {
            let Value::Number(n1) = evaluate(*expr1);
            let Value::Number(n2) = evaluate(*expr2);
            Value::Number(n1 + n2)
        },
        Expression::ApplyInfixBinOp(expr1, InfixBinaryOperator::Sub, expr2) => {
            let Value::Number(n1) = evaluate(*expr1);
            let Value::Number(n2) = evaluate(*expr2);
            Value::Number(n1 - n2)
        },
        Expression::ApplyInfixBinOp(expr1, InfixBinaryOperator::Mul, expr2) => {
            let Value::Number(n1) = evaluate(*expr1);
            let Value::Number(n2) = evaluate(*expr2);
            Value::Number(n1 * n2)
        },
        Expression::ApplyInfixBinOp(expr1, InfixBinaryOperator::Div, expr2) => {
            let Value::Number(n1) = evaluate(*expr1);
            let Value::Number(n2) = evaluate(*expr2);
            Value::Number(n1 / n2)
        },
        Expression::ApplyInfixBinOp(expr1, InfixBinaryOperator::Mod, expr2) => {
            let Value::Number(n1) = evaluate(*expr1);
            let Value::Number(n2) = evaluate(*expr2);
            Value::Number(n1 % n2)
        }
    }
}