
use std::collections::HashMap;
use ast::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Number(i64),
    Boolean(bool)
}

//#[derive(Debug, Hash, PartialEq, Eq)]
//pub enum Symbol {
//    Var(VariableName)
//}

#[derive(Debug, PartialEq)]
pub struct Scope {
    symbol_table: HashMap<VariableName, Value>
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
    NotInScope(VariableName),
    UnknownSubroutine(SubroutineName),
    WrongNumberOfArguments(SubroutineName, usize, usize),
    SubroutineNullReturn(SubroutineName),
    TypeError
}

pub fn evaluate_subroutine(subroutines: &HashMap<SubroutineName, Subroutine>,
                           scope: &mut Scope,
                           sub_name: &SubroutineName,
                           sub: &Subroutine,
                           arguments: &Vec<Box<Expression>>) -> Result<Option<Value>, EvalError> {
    if sub.parameters.len() != arguments.len() {
        Err(EvalError::WrongNumberOfArguments(sub_name.clone(), sub.parameters.len(), arguments.len()))
    } else {
        let evaluated_arguments: Result<Vec<Value>, EvalError> = arguments.iter().map(|argument| {
            evaluate_expression(subroutines, scope, argument)
        }).collect();
        let evaluated_arguments = evaluated_arguments?;
        let mut subroutine_scope = Scope {
            symbol_table: sub.parameters.iter().cloned().zip(evaluated_arguments).collect()
        };
        match sub.implementation {
            Implementation::Block(ref compount_statement) =>
                evaluate_compound_statement(subroutines, &mut subroutine_scope, compount_statement),
            Implementation::Builtin(_) => panic!("Not Implemented: evaluating builtin subroutines")
        }

    }
}

pub fn evaluate_expression(subroutines: &HashMap<SubroutineName, Subroutine>,
                           scope: &mut Scope,
                           expr: &Expression) -> Result<Value, EvalError> {
    match expr {
        &Expression::Lit(Literal::Boolean(b)) => Ok(Value::Boolean(b)),
        &Expression::Lit(Literal::Number(n)) => Ok(Value::Number(n)),
        &Expression::Var(ref var) => match scope.symbol_table.get(var) {
            None => Err(EvalError::NotInScope(var.clone())),
            Some(val) => Ok(val.clone())
        },
        &Expression::ApplyUnOp(UnaryOperator::Minus, ref expr) => {
            match evaluate_expression(subroutines, scope, expr)? {
                Value::Number(n1) => Ok(Value::Number(-n1)),
                _ => Err(EvalError::TypeError)
            }
        },
        &Expression::ApplyUnOp(UnaryOperator::Not, ref expr) => {
            match evaluate_expression(subroutines, scope, expr)? {
                Value::Boolean(b) => Ok(Value::Boolean(!b)),
                _ => Err(EvalError::TypeError)
            }
        },
        &Expression::ApplyInfixBinOp(ref expr1, InfixBinaryOperator::Add, ref expr2) => {
            let n1 = match evaluate_expression(subroutines, scope, expr1)? {
                Value::Number(n) => n,
                _ => return Err(EvalError::TypeError)
            };
            let n2 = match evaluate_expression(subroutines, scope, expr2)? {
                Value::Number(n) => n,
                _ => return Err(EvalError::TypeError)
            };
            Ok(Value::Number(n1 + n2))
        },
        &Expression::ApplyInfixBinOp(ref expr1, InfixBinaryOperator::Sub, ref expr2) => {
            let n1 = match evaluate_expression(subroutines, scope, expr1)? {
                Value::Number(n) => n,
                _ => return Err(EvalError::TypeError)
            };
            let n2 = match evaluate_expression(subroutines, scope, expr2)? {
                Value::Number(n) => n,
                _ => return Err(EvalError::TypeError)
            };
            Ok(Value::Number(n1 - n2))
        },
        &Expression::ApplyInfixBinOp(ref expr1, InfixBinaryOperator::Mul, ref expr2) => {
            let n1 = match evaluate_expression(subroutines, scope, expr1)? {
                Value::Number(n) => n,
                _ => return Err(EvalError::TypeError)
            };
            let n2 = match evaluate_expression(subroutines, scope, expr2)? {
                Value::Number(n) => n,
                _ => return Err(EvalError::TypeError)
            };
            Ok(Value::Number(n1 * n2))
        },
        &Expression::ApplyInfixBinOp(ref expr1, InfixBinaryOperator::Div, ref expr2) => {
            let n1 = match evaluate_expression(subroutines, scope, expr1)? {
                Value::Number(n) => n,
                _ => return Err(EvalError::TypeError)
            };
            let n2 = match evaluate_expression(subroutines, scope, expr2)? {
                Value::Number(n) => n,
                _ => return Err(EvalError::TypeError)
            };
            Ok(Value::Number(n1 / n2))
        },
        &Expression::ApplyInfixBinOp(ref expr1, InfixBinaryOperator::Mod, ref expr2) => {
            let n1 = match evaluate_expression(subroutines, scope, expr1)? {
                Value::Number(n) => n,
                _ => return Err(EvalError::TypeError)
            };
            let n2 = match evaluate_expression(subroutines, scope, expr2)? {
                Value::Number(n) => n,
                _ => return Err(EvalError::TypeError)
            };
            Ok(Value::Number(n1 % n2))
        },
        &Expression::ApplyInfixBinOp(ref expr1, InfixBinaryOperator::And, ref expr2) => {
            let b1 = match evaluate_expression(subroutines, scope, expr1)? {
                Value::Boolean(b) => b,
                _ => return Err(EvalError::TypeError)
            };
            let b2 = match evaluate_expression(subroutines, scope, expr2)? {
                Value::Boolean(b) => b,
                _ => return Err(EvalError::TypeError)
            };
            Ok(Value::Boolean(b1 && b2))
        },
        &Expression::ApplyInfixBinOp(ref expr1, InfixBinaryOperator::Or, ref expr2) => {
            let b1 = match evaluate_expression(subroutines, scope, expr1)? {
                Value::Boolean(b) => b,
                _ => return Err(EvalError::TypeError)
            };
            let b2 = match evaluate_expression(subroutines, scope, expr2)? {
                Value::Boolean(b) => b,
                _ => return Err(EvalError::TypeError)
            };
            Ok(Value::Boolean(b1 || b2))
        },
        &Expression::ApplyInfixBinOp(ref expr1, InfixBinaryOperator::Eq, ref expr2) => {
            let n1 = match evaluate_expression(subroutines, scope, expr1)? {
                Value::Number(n) => n,
                _ => return Err(EvalError::TypeError)
            };
            let n2 = match evaluate_expression(subroutines, scope, expr2)? {
                Value::Number(n) => n,
                _ => return Err(EvalError::TypeError)
            };
            Ok(Value::Boolean(n1 == n2))
        },
        &Expression::CallSubByValue(ref sub_name, ref arguments) => {
            match subroutines.get(sub_name) {
                None => Err(EvalError::UnknownSubroutine(sub_name.clone())),
                Some(sub) => {
                    match evaluate_subroutine(subroutines, scope, sub_name, sub, arguments)? {
                        None => Err(EvalError::SubroutineNullReturn(sub_name.clone())),
                        Some(value) => Ok(value)
                    }
                }
            }
        },
        &Expression::Conditional(ref test_expr, ref expr_true, ref expr_false) => {
            let test = match evaluate_expression(subroutines, scope, test_expr)? {
                Value::Boolean(b) => b,
                _ => return Err(EvalError::TypeError)
            };
            if test {
                evaluate_expression(subroutines, scope, expr_true)
            } else {
                evaluate_expression(subroutines, scope, expr_false)
            }
        }
    }
}

#[derive(Debug)]
pub enum ControlFlow {
    Continue,
    Return(Option<Value>)
}

pub fn evaluate_statement(subroutines: &HashMap<SubroutineName, Subroutine>, scope: &mut Scope, stmt: &Statement) -> Result<ControlFlow, EvalError> {
    match stmt {
        &Statement::Empty => Ok(ControlFlow::Continue),
        &Statement::Assignment(ref var, ref expr) => {
            let val = evaluate_expression(subroutines, scope, expr)?;
            scope.symbol_table.insert(var.clone(), val.clone());
            Ok(ControlFlow::Continue)
        },
        &Statement::EvaluateIgnore(ref expr) => {
            evaluate_expression(subroutines, scope, expr)?;
            Ok(ControlFlow::Continue)
        },
        &Statement::Return(ref return_expression) => {
            match return_expression {
                &None => Ok(ControlFlow::Return(None)),
                &Some(ref expr) => {
                    let val = evaluate_expression(subroutines, scope, expr)?;
                    Ok(ControlFlow::Return(Some(val)))
                }
            }
        },
    }
}

fn evaluate_statement_list(subroutines: &HashMap<SubroutineName, Subroutine>, scope: &mut Scope, statement_list: &StatementList) -> Result<ControlFlow, EvalError> {
    match statement_list {
        &StatementList::Single(ref stmt) => evaluate_statement(subroutines, scope, stmt),
        &StatementList::Sequence(ref stmt_head, ref stmt_tail) => {
            let control_flow = evaluate_statement(subroutines, scope, stmt_head)?;
            match control_flow {
                ControlFlow::Continue => evaluate_statement_list(subroutines, scope, &*stmt_tail),
                ControlFlow::Return(_) => Ok(control_flow)
            }
        }
    }
}

fn evaluate_compound_statement(subroutines: &HashMap<SubroutineName, Subroutine>, scope: &mut Scope, compound_statement: &CompoundStatement) -> Result<Option<Value>, EvalError> {
    let result = evaluate_statement_list(subroutines, scope, &compound_statement.statement_list)?;
    Ok(match result {
        ControlFlow::Continue => None,
        ControlFlow::Return(val) => val
    })
}

pub fn evaluate_program(program: Program) -> Result<Scope, EvalError> {
    let mut global_scope = Scope::default();
    evaluate_compound_statement(&program.subroutines, &mut global_scope, &program.entry)?;
    Ok(global_scope)
}