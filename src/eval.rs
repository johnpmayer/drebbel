
use std::collections::HashMap;
use ast::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Number(i64)
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub enum Symbol {
    Var(Variable)
}

#[derive(Debug, PartialEq)]
pub struct Scope {
    symbol_table: HashMap<Symbol, Value>
}

impl Default for Scope {
    fn default() -> Self {
        Scope {
            symbol_table: HashMap::new()
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum EvalError {
    NotInScope(Variable)
}

pub fn evaluate_expression(scope: &Scope, expr: Expression) -> Result<Value, EvalError> {
    match expr {
        Expression::Lit(Literal::Number(n)) => Ok(Value::Number(n)),
        Expression::Var(var) => match scope.symbol_table.get(&Symbol::Var(var.clone())) {
            None => Err(EvalError::NotInScope(var)),
            Some(val) => Ok(val.clone())
        },
        Expression::ApplyUnOp(UnaryOperator::Minus, expr) => {
            let Value::Number(n1) = evaluate_expression(scope, *expr)?;
            Ok(Value::Number(- n1))
        },
        Expression::ApplyInfixBinOp(expr1, InfixBinaryOperator::Add, expr2) => {
            let Value::Number(n1) = evaluate_expression(scope, *expr1)?;
            let Value::Number(n2) = evaluate_expression(scope, *expr2)?;
            Ok(Value::Number(n1 + n2))
        },
        Expression::ApplyInfixBinOp(expr1, InfixBinaryOperator::Sub, expr2) => {
            let Value::Number(n1) = evaluate_expression(scope, *expr1)?;
            let Value::Number(n2) = evaluate_expression(scope, *expr2)?;
            Ok(Value::Number(n1 - n2))
        },
        Expression::ApplyInfixBinOp(expr1, InfixBinaryOperator::Mul, expr2) => {
            let Value::Number(n1) = evaluate_expression(scope, *expr1)?;
            let Value::Number(n2) = evaluate_expression(scope, *expr2)?;
            Ok(Value::Number(n1 * n2))
        },
        Expression::ApplyInfixBinOp(expr1, InfixBinaryOperator::Div, expr2) => {
            let Value::Number(n1) = evaluate_expression(scope, *expr1)?;
            let Value::Number(n2) = evaluate_expression(scope, *expr2)?;
            Ok(Value::Number(n1 / n2))
        },
        Expression::ApplyInfixBinOp(expr1, InfixBinaryOperator::Mod, expr2) => {
            let Value::Number(n1) = evaluate_expression(scope, *expr1)?;
            let Value::Number(n2) = evaluate_expression(scope, *expr2)?;
            Ok(Value::Number(n1 % n2))
        }
    }
}

pub fn evaluate_statement(scope: &mut Scope, stmt: Statement) -> Result<(), EvalError> {
    match stmt {
        Statement::Empty => Ok(()),
        Statement::Assignment(var, expr) => {
            let val = evaluate_expression(scope, *expr)?;
            scope.symbol_table.insert(Symbol::Var(var.clone()), val.clone());
            Ok(())
        }
        Statement::Compound(compound_stmt) => evaluate_compound_statement(scope, *compound_stmt)
    }
}

fn evaluate_statement_list(scope: &mut Scope, statement_list: StatementList) -> Result<(), EvalError> {
    match statement_list {
        StatementList::Single(stmt) => evaluate_statement(scope, stmt),
        StatementList::Sequence(stmt_head, stmt_tail) => {
            evaluate_statement(scope, stmt_head)?;
            evaluate_statement_list(scope, *stmt_tail)
        }
    }
}

fn evaluate_compound_statement(scope: &mut Scope, compound_stmt: CompoundStatement) -> Result<(), EvalError> {
    let CompoundStatement(statement_list) = compound_stmt;
    evaluate_statement_list(scope, statement_list)
}

pub fn evaluate_program(program: Program) -> Result<Scope, EvalError> {
    let mut global_scope = Scope::default();
    let Program(compound_stmt) = program;
    evaluate_compound_statement(&mut global_scope, compound_stmt)?;
    Ok(global_scope)
}