
use ast::*;
use intermediate::*;
use std::rc::Rc;
use std::cell::Cell;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Debug;

#[derive(Clone, Debug)]
enum Value {
    Unit,
    Number(i64),
    Boolean(bool),
    Cont(Continuation),
}

#[derive(Clone)]
struct Continuation {
    symbol: Symbol,
    frame: Rc<Cell<Frame>>,
    // frame: Frame,
}

impl Debug for Continuation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Continuation({:?})", self.symbol)
    }
}

#[derive(Clone)]
struct Frame {
    parent: Option<(AssignTarget, Box<Frame>)>,
    continuation: Option<Symbol>,
    instructions: Vec<Instruction>,
    program_counter: usize,
    scope: HashMap<AssignTarget, Value>,
}

impl Frame {
    fn new(instructions: Vec<Instruction>) -> Self {
        Frame {
            parent: None,
            continuation: None,
            instructions: instructions,
            program_counter: 0,
            scope: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub enum ExecutionError {
    NotInScope(AssignTarget),
    UnknownSubroutine(SubroutineName),
    TypeError(String),
    JumpOutOfBounds,
}

fn lookup_value(scope: &HashMap<AssignTarget, Value>, target: &AssignTarget) -> Result<Value, ExecutionError> {
    match scope.get(target) {
        None => Err(ExecutionError::NotInScope(target.clone())),
        Some(value) => Ok(value.clone()),
    }
}

fn get_value(scope: &HashMap<AssignTarget, Value>, target: &ValueTarget) -> Result<Value, ExecutionError> {
    match target {
        &ValueTarget::Literal(Literal::Number(i)) => Ok(Value::Number(i)),
        &ValueTarget::Literal(Literal::Boolean(b)) => Ok(Value::Boolean(b)),
        &ValueTarget::Register(ref reg) => lookup_value(scope, &AssignTarget::Register(reg.clone())),
        &ValueTarget::Variable(ref var) => lookup_value(scope, &AssignTarget::Variable(var.clone())),
    }
}

fn execute_builtin(builtin: &Builtin, arguments: Vec<Value>) -> Result<Value, ExecutionError> {
    match *builtin {
        Builtin::Print => {
            for arg in arguments {
                println!("--> {:?}", arg);
            }
            Ok(Value::Unit)
        },
    }
}

pub fn execute_program(program: &Program) -> Result<(), ExecutionError> {

    let mut frame = Frame::new(jit(&program.entry));

    let mut subroutine_instruction_cache: HashMap<SubroutineName, Vec<Instruction>> = HashMap::new();

    loop {

        let instruction = if frame.program_counter >= frame.instructions.len() {
            // Implicit return if we've reached the end of what to do
            Instruction::Return(None)
        } else {
            frame.instructions[frame.program_counter].clone()
        };

        println!("### Executing instruction {:?}", instruction);

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
                    (&Value::Number(l), &InfixBinaryOperator::Lt, &Value::Number(r)) => Value::Boolean(l < r),
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
                        break;
                    },
                    Some((assign_tgt, parent_frame)) => {
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
                    let new_counter = (frame.program_counter as isize) + offset;
                    if new_counter < 0 {
                        return Err(ExecutionError::JumpOutOfBounds);
                    }
                    frame.program_counter = new_counter as usize;
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

                        let argument_values: Vec<Value> = argument_values?;

                        match subroutine.implementation {
                            Implementation::Builtin(ref builtin) => {
                                let value = execute_builtin(builtin, argument_values)?;
                                frame.scope.insert(assign_tgt.clone(), value);
                            }
                            Implementation::Block(ref compound_statement) => {
                                let subroutine_scope: HashMap<AssignTarget, Value> = subroutine.arguments.iter().map(|arg_name| {
                                    AssignTarget::Variable(arg_name.clone())
                                }).zip(argument_values).collect();
                                let subroutine_instructions = subroutine_instruction_cache.entry(sub_name.clone()).or_insert({
                                    jit(compound_statement)
                                }).clone();
                                let subroutine_frame = Frame {
                                    parent: Some((assign_tgt.clone(), Box::new(frame))),
                                    continuation: None,
                                    instructions: subroutine_instructions,
                                    program_counter: 0,
                                    scope: subroutine_scope,
                                };
                                frame = subroutine_frame;
                                continue;
                            }
                        }
                    }
                }
            },
            Instruction::MakeCont(ref assign_tgt, ref symbol, ref sub_name, ref argument_targets) => {
                let argument_values: Result<Vec<Value>, ExecutionError> = argument_targets.iter().map(|ref target| {
                    get_value(&frame.scope, target)
                }).collect();

                match program.subroutines.get(sub_name) {
                    None => return Err(ExecutionError::UnknownSubroutine(sub_name.clone())),
                    Some(ref subroutine) => {

                        let argument_values: Vec<Value> = argument_values?;

                        match subroutine.implementation {
                            Implementation::Builtin(_) => panic!("Can't create continuation of a builtin"),
                            Implementation::Block(ref compound_statement) => {
                                let subroutine_scope: HashMap<AssignTarget, Value> = subroutine.arguments.iter().map(|arg_name| {
                                    AssignTarget::Variable(arg_name.clone())
                                }).zip(argument_values).collect();
                                let subroutine_instructions = subroutine_instruction_cache.entry(sub_name.clone()).or_insert({
                                    jit(compound_statement)
                                }).clone();
                                let subroutine_frame = Frame {
                                    parent: None,
                                    continuation: Some(symbol.clone()),
                                    instructions: subroutine_instructions,
                                    program_counter: 0,
                                    scope: subroutine_scope,
                                };
                                let continuation = Continuation {
                                    symbol: symbol.clone(),
                                    frame: Rc::new(Cell::new(subroutine_frame)),
                                };
                                frame.scope.insert(assign_tgt.clone(), Value::Cont(continuation));
                            }
                        }
                    },
                }
            },
            Instruction::RunCont(ref assign_tgt, ref value_tgt) => {
                panic!("TODO")
            },
            Instruction::IsDoneCont(ref assign_tgt, ref value_tgt) => {
                panic!("TODO")
            },
            Instruction::SuspendCont(ref assign_tgt, ref symbol, None) => {
                panic!("TODO")
            },
            Instruction::SuspendCont(ref assign_tgt, ref symbol, Some(ref value_tgt)) => {
                panic!("TODO")
            },
        }

        frame.program_counter += 1;

    }

    for (target, value) in frame.scope.iter() {
        println!("### Final value of {:?} = {:?}", target, value)
    }

    Ok(())

}