
use std::collections::HashMap;
use ast::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Number(i64)
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub enum Symbol {
    Var(VariableName)
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
    NotInScope(VariableName)
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

#[derive(Debug)]
pub enum ControlFlow {
    Continue,
    Return(Option<Value>)
}

pub fn evaluate_statement(scope: &mut Scope, stmt: Statement) -> Result<ControlFlow, EvalError> {
    match stmt {
        Statement::Empty => Ok(ControlFlow::Continue),
        Statement::Assignment(var, expr) => {
            let val = evaluate_expression(scope, *expr)?;
            scope.symbol_table.insert(Symbol::Var(var.clone()), val.clone());
            Ok(ControlFlow::Continue)
        }
        Statement::Return(return_expression) => {
            match return_expression {
                None => Ok(ControlFlow::Return(None)),
                Some(expr) => {
                    let val = evaluate_expression(scope, *expr)?;
                    Ok(ControlFlow::Return(Some(val)))
                }
            }
        }
    }
}

fn evaluate_statement_list(scope: &mut Scope, statement_list: StatementList) -> Result<ControlFlow, EvalError> {
    match statement_list {
        StatementList::Single(stmt) => evaluate_statement(scope, stmt),
        StatementList::Sequence(stmt_head, stmt_tail) => {
            let control_flow = evaluate_statement(scope, stmt_head)?;
            match control_flow {
                ControlFlow::Continue => evaluate_statement_list(scope, *stmt_tail),
                ControlFlow::Return(_) => Ok(control_flow)
            }
        }
    }
}

fn evaluate_compound_statement(scope: &mut Scope, CompoundStatement(statement_list): CompoundStatement) -> Result<Option<Value>, EvalError> {
    let result = evaluate_statement_list(scope, statement_list)?;
    Ok(match result {
        ControlFlow::Continue => None,
        ControlFlow::Return(val) => val
    })
}

pub fn evaluate_program(program: Program) -> Result<Scope, EvalError> {
    let mut global_scope = Scope::default();
    evaluate_compound_statement(&mut global_scope, program.entry)?;
    Ok(global_scope)
}