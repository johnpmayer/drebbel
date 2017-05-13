
use ast::*;
use intermediate::*;
use std::collections::HashMap;
use std::ops::DerefMut;

#[derive(Clone, Debug)]
enum Value {
    Unit,
    Number(i64),
    Boolean(bool)
}

struct Frame {
    parent: Option<(AssignTarget, Box<Frame>)>,
    instructions: Vec<Instruction>,
    program_counter: usize,
    scope: HashMap<AssignTarget, Value>
}

impl Frame {
    fn new(instructions: Vec<Instruction>) -> Self {
        Frame {
            parent: None,
            instructions: instructions,
            program_counter: 0,
            scope: HashMap::new()
        }
    }
}

pub enum ExecutionError {
    NotInScope(AssignTarget),
    UnknownSubroutine(SubroutineName),
    TypeError(String)
}

fn lookup_value(scope: &HashMap<AssignTarget, Value>, target: &AssignTarget) -> Result<Value, ExecutionError> {
    match scope.get(target) {
        None => Err(ExecutionError::NotInScope(target.clone())),
        Some(value) => Ok(value.clone())
    }
}

fn get_value(scope: &HashMap<AssignTarget, Value>, target: &ValueTarget) -> Result<Value, ExecutionError> {
    match target {
        &ValueTarget::Literal(Literal::Number(i)) => Ok(Value::Number(i)),
        &ValueTarget::Literal(Literal::Boolean(b)) => Ok(Value::Boolean(b)),
        &ValueTarget::Register(ref reg) => lookup_value(scope, &AssignTarget::Register(reg.clone())),
        &ValueTarget::Variable(ref var) => lookup_value(scope, &AssignTarget::Variable(var.clone()))
    }
}


pub fn execute_program(program: &Program) -> Result<(), ExecutionError> {

    let mut frame = Frame::new(jit(&program.entry));

    while true {

        let instruction = if frame.program_counter >= frame.instructions.len() {
            // Implicit return if we've reached the end of what to do
            Instruction::Return(None)
        } else {
            frame.instructions[frame.program_counter].clone()
        };

        println!("Executing instruciton {:?}", instruction);

        match instruction {
            Instruction::Assign(ref assign_tgt, ref value_tgt) => {
                let value = get_value(&frame.scope, value_tgt)?;
                frame.scope.insert(assign_tgt.clone(), value);
            },
            Instruction::ApplyUnOp(ref assign_tgt, ref op, ref value_tgt) => {
                let value = get_value(&frame.scope, value_tgt)?;
                let result = match (op, &value) {
                    (&UnaryOperator::Minus, &Value::Number(i)) => Value::Number(-i),
                    (&UnaryOperator::Not, &Value::Boolean(b)) => Value::Boolean(!b),
                    _ => return Err(ExecutionError::TypeError(format!("Can't apply {:?} to {:?}", op, value)))
                };
                frame.scope.insert(assign_tgt.clone(), result);
            },
            Instruction::ApplyBinOp(ref assign_tgt, ref l_value_tgt, ref op, ref r_value_tgt) => {
                let l_value = get_value(&frame.scope, l_value_tgt)?;
                let r_value = get_value(&frame.scope, r_value_tgt)?;
                let result = match (&l_value, op, &r_value) {
                    (&Value::Number(l), &InfixBinaryOperator::Add, &Value::Number(r)) => Value::Number(l + r),
                    (&Value::Number(l), &InfixBinaryOperator::Sub, &Value::Number(r)) => Value::Number(l - r),
                    (&Value::Number(l), &InfixBinaryOperator::Mul, &Value::Number(r)) => Value::Number(l * r),
                    (&Value::Number(l), &InfixBinaryOperator::Div, &Value::Number(r)) => Value::Number(l / r),
                    (&Value::Number(l), &InfixBinaryOperator::Mod, &Value::Number(r)) => Value::Number(l % r),
                    (&Value::Boolean(l), &InfixBinaryOperator::And, &Value::Boolean(r)) => Value::Boolean(l && r),
                    (&Value::Boolean(l), &InfixBinaryOperator::Or, &Value::Boolean(r)) => Value::Boolean(l || r),
                    (&Value::Number(l), &InfixBinaryOperator::Eq, &Value::Number(r)) => Value::Boolean(l == r),
                    _ => return Err(ExecutionError::TypeError(format!("Can't apply {:?} to {:?} and {:?}", l_value, op, r_value)))
                };
                frame.scope.insert(assign_tgt.clone(), result);
            },
            Instruction::Return(value_tgt) => {
                let value = match value_tgt {
                    None => Value::Unit,
                    Some(ref value_tgt) => get_value(&frame.scope, value_tgt)?
                };
                match frame.parent {
                    None => {
                        // TODO: support integer exit codes for main?
                        break;
                    },
                    Some((assign_tgt, mut parent_frame)) => {
                        frame = *parent_frame;
                        frame.scope.insert(assign_tgt.clone(), value);
                    }
                }
            },
            Instruction::ConditionalJumpRelative(ref value_tgt, offset) => {
                let test_value = match get_value(&frame.scope, value_tgt)? {
                    Value::Boolean(b) => b,
                    v => return Err(ExecutionError::TypeError(format!("Can't use {:?} for conditional", v)))
                };
                if test_value {
                    frame.program_counter += offset;
                    continue;
                }
            },
            Instruction::CallSubroutine(ref assign_tgt, ref sub_name, ref argument_targets) => {
                let argument_values: Result<Vec<Value>, ExecutionError> = argument_targets.iter().map(|ref target| {
                    get_value(&frame.scope, target)
                }).collect();

                match program.subroutines.get(sub_name) {
                    None => return Err(ExecutionError::UnknownSubroutine(sub_name.clone())),
                    Some(ref subroutine) => {
                        let subroutine_scope = subroutine.arguments.iter().map(|arg_name| {
                            AssignTarget::Variable(arg_name.clone())
                        }).zip(argument_values?);

                        panic!("TODO push new stack frame")
                    }
                }
            }
        }

        frame.program_counter += 1;

    }

    Ok(())

}