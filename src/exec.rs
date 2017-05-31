
use ast::*;
use intermediate::*;
// use std::rc::Rc;
// use std::cell::Cell;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Debug;

use std::io::stdin;

#[derive(Clone, Debug)]
enum Value {
    Unit,
    Number(i64),
    Boolean(bool),
    Cont(Box<Continuation>),
}

#[derive(Clone)]
struct Continuation {
    done: bool,
    symbol: Symbol,
    last_value: Value,
    frames: Vec<Frame>,
}

impl Debug for Continuation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Continuation({:?})", self.symbol)
    }
}

fn lookup_value(scope: &HashMap<AssignTarget, Value>, target: &AssignTarget) -> Result<Value, ExecutionError> {
    match scope.get(target) {
        None => Err(ExecutionError::NotInScope(target.clone())),
        Some(value) => Ok(value.clone()),
    }
}

#[derive(Clone)]
struct Frame {
    running_continuation: Option<Symbol>,
    call_target: Option<AssignTarget>,
    instructions: Vec<Instruction>,
    program_counter: usize,
    scope: HashMap<AssignTarget, Value>,
}

impl Frame {
    fn new(instructions: Vec<Instruction>) -> Self {
        Frame {
            running_continuation: None,
            call_target: None,
            instructions: instructions,
            program_counter: 0,
            scope: HashMap::new(),
        }
    }

    fn current_instruction(&self) -> Instruction {
        if self.program_counter >= self.instructions.len() {
            // Implicit return if we've reached the end of what to do
            Instruction::Return(None)
        } else {
            self.instructions[self.program_counter].clone()
        }
    }

    fn assign(&mut self, target: &AssignTarget, value: Value) {
        self.scope.insert(target.clone(), value);
    }

    fn assign_subroutine(&mut self, value: Value) -> Result<(), ExecutionError> {
        let target = match self.call_target {
            None => return Err(ExecutionError::UnsetReturnTarget),
            Some(ref target) => target.clone()
        };
        self.assign(&target, value);
        self.call_target = None;
        self.running_continuation = None; // TODO: verify this is always correct...
        Ok(())
    }

    // TODO: maybe better to use Rc<Value> here?
    fn get_value(&self, target: &ValueTarget) -> Result<Value, ExecutionError> {
        match target {
            &ValueTarget::Literal(Literal::Number(i)) => Ok(Value::Number(i)),
            &ValueTarget::Literal(Literal::Boolean(b)) => Ok(Value::Boolean(b)),
            &ValueTarget::Register(ref reg) => lookup_value(&self.scope, &AssignTarget::Register(reg.clone())),
            &ValueTarget::Variable(ref var) => lookup_value(&self.scope, &AssignTarget::Variable(var.clone())),
        }
    }
}

struct Stack {
    frames: Vec<Frame>,
}

impl Stack {
    fn new(frame: Frame) -> Stack {
        Stack {
            frames: vec!(frame),
        }
    }

    fn current_frame(&mut self) -> Result<&Frame, ExecutionError> {
        match self.frames.last() {
            None => Err(ExecutionError::StackUnderflow),
            Some(ref frame) => Ok(frame)
        }
    }

    fn with_current_frame<F, T>(&mut self, f: F) -> Result<T, ExecutionError> where F: FnOnce(&mut Frame) -> Result<T, ExecutionError> {
        match self.frames.last_mut() {
            None => Err(ExecutionError::StackUnderflow),
            Some(mut frame) => f(frame)
        }
    }

    fn assign_current_frame(&mut self, target: &AssignTarget, value: Value) -> Result<(), ExecutionError> {
        self.with_current_frame(|mut frame| {
            frame.assign(target, value);
            Ok(())
        })
    }

    fn move_current_program_counter(&mut self, offset: isize) -> Result<(), ExecutionError> {
        self.with_current_frame(|mut current_frame| {
            let new_counter = (current_frame.program_counter as isize) + offset;
            if new_counter < 0 {
                return Err(ExecutionError::JumpOutOfBounds);
            }
            current_frame.program_counter = new_counter as usize;
            Ok(())
        })
    }

    fn advance_current_program_counter(&mut self) -> Result<(), ExecutionError> {
        self.move_current_program_counter(1)
    }

    fn pop_subroutine_frame(&mut self, value: Value) -> Result<Option<Frame>, ExecutionError> {
        match self.frames.pop() {
            None => return Err(ExecutionError::JumpOutOfBounds),
            Some(exited_frame) => {
                if self.frames.is_empty() {
                    Ok(Some(exited_frame))
                } else {
                    let assign_value = match self.current_frame()?.running_continuation.clone() {
                        None => value,
                        Some(ref symbol) => Value::Cont(Box::new(Continuation {
                            done: true,
                            symbol: symbol.clone(),
                            last_value: value,
                            frames: vec!(),
                        })),
                    };
                    self.with_current_frame(|mut frame| frame.assign_subroutine(assign_value))?;
                    Ok(None)
                }
            },
        }
    }

    fn push_subroutine_frame(&mut self, assign_tgt: &AssignTarget, frame: Frame) -> Result<(), ExecutionError> {
        self.with_current_frame(|mut frame| {
            frame.call_target = Some(assign_tgt.clone());
            Ok(())
        })?;
        self.frames.push(frame);
        Ok(())
    }

    fn push_coroutine_frames(&mut self, assign_tgt: &AssignTarget, symbol: &Symbol, frames: &Vec<Frame>) -> Result<(), ExecutionError> {
        self.with_current_frame(|mut frame| {
            frame.call_target = Some(assign_tgt.clone());
            frame.running_continuation = Some(symbol.clone());
            Ok(())
        })?;
        self.frames.extend_from_slice(frames.as_slice());
        Ok(())
    }

    fn unwind_coroutine(&mut self, symbol: &Symbol, value: Value) -> Result<(), ExecutionError> {
        let innermost_symbol_frame_index = self.frames.iter().rposition(|ref frame| {
            match frame.running_continuation {
                None => false,
                Some(ref frame_symbol) => *frame_symbol == *symbol
            }
        }).ok_or(ExecutionError::UncaughtSuspension(symbol.clone()))?;
        
        let cont_frames = self.frames.split_off(innermost_symbol_frame_index + 1);
        let continuation = Continuation {
            done: false,
            symbol: symbol.clone(),
            last_value: value,
            frames: cont_frames
        };
    
        let cont_assign_target: AssignTarget = self.current_frame()?.call_target.clone().ok_or(ExecutionError::UnsetReturnTarget)?;

        self.assign_current_frame(&cont_assign_target, Value::Cont(Box::new(continuation)))
    }
}

#[derive(Debug)]
pub enum ExecutionError {
    NotInScope(AssignTarget),
    UnknownSubroutine(SubroutineName),
    TypeError(String),
    JumpOutOfBounds,
    UnsetReturnTarget,
    StackUnderflow,
    UncaughtSuspension(Symbol),
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

    let mut stack = Stack::new(Frame::new(jit(&program.entry)));

    let mut subroutine_instruction_cache: HashMap<SubroutineName, Vec<Instruction>> = HashMap::new();
    // let mut final_frame: Option<Frame> = None;

    loop {

        // let current_frame: &mut Frame = stack.current_frame()?;
        let instruction = {
            stack.current_frame()?.current_instruction().clone()
        };
        // let current_frame: &mut Frame = panic!("MUTABLE");

        println!("### Executing instruction {:?}", instruction);

        let _: () = match instruction {
            Instruction::Assign(ref assign_tgt, ref value_tgt) => {
                let value = stack.current_frame()?.get_value(value_tgt)?;
                stack.assign_current_frame(assign_tgt, value)?;
            },
            Instruction::ApplyUnOp(ref assign_tgt, ref op, ref value_tgt) => {
                let value = stack.current_frame()?.get_value(value_tgt)?;
                let result = match (op, &value) {
                    (&UnaryOperator::Minus, &Value::Number(i)) => Value::Number(-i),
                    (&UnaryOperator::Not, &Value::Boolean(b)) => Value::Boolean(!b),
                    _ => return Err(ExecutionError::TypeError(format!("Can't apply {:?} to {:?}", op, value)))
                };
                stack.assign_current_frame(assign_tgt, result)?;
            },
            Instruction::ApplyBinOp(ref assign_tgt, ref l_value_tgt, ref op, ref r_value_tgt) => {
                let l_value = stack.current_frame()?.get_value(l_value_tgt)?;
                let r_value = stack.current_frame()?.get_value(r_value_tgt)?;
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
                stack.assign_current_frame(assign_tgt, result)?;
            },
            Instruction::Return(value_tgt) => {
                let value = match value_tgt {
                    None => Value::Unit,
                    Some(ref value_tgt) => stack.current_frame()?.get_value(value_tgt)?
                };
                if let Some(final_frame) = stack.pop_subroutine_frame(value)? {
                    // Dump frame and quit execution
                    for (target, value) in final_frame.scope.iter() {
                        println!("### Final value of {:?} = {:?}", target, value)
                    }
                    break
                }
            },
            Instruction::ConditionalJumpRelative(ref value_tgt, offset) => {
                let test_value = match stack.current_frame()?.get_value(value_tgt)? {
                    Value::Boolean(b) => b,
                    v => return Err(ExecutionError::TypeError(format!("Can't use {:?} for conditional", v)))
                };
                if test_value {
                    stack.move_current_program_counter(offset)?;
                    continue;
                }
            },
            Instruction::CallSubroutine(ref assign_tgt, ref sub_name, ref argument_targets) => {
                let argument_values: Result<Vec<Value>, ExecutionError> = argument_targets.iter().map(|ref target| {
                    stack.current_frame()?.get_value(target)
                }).collect();

                match program.subroutines.get(sub_name) {
                    None => return Err(ExecutionError::UnknownSubroutine(sub_name.clone())),
                    Some(ref subroutine) => {

                        let argument_values: Vec<Value> = argument_values?;

                        match subroutine.implementation {
                            Implementation::Builtin(ref builtin) => {
                                let value = execute_builtin(builtin, argument_values)?;
                                stack.assign_current_frame(assign_tgt, value)?
                            }
                            Implementation::Block(ref compound_statement) => {
                                let subroutine_scope: HashMap<AssignTarget, Value> = subroutine.arguments.iter().map(|arg_name| {
                                    AssignTarget::Variable(arg_name.clone())
                                }).zip(argument_values).collect();
                                let subroutine_instructions = subroutine_instruction_cache.entry(sub_name.clone()).or_insert({
                                    jit(compound_statement)
                                }).clone();
                                let subroutine_frame = Frame {
                                    call_target: None,
                                    running_continuation: None,
                                    instructions: subroutine_instructions,
                                    program_counter: 0,
                                    scope: subroutine_scope,
                                };
                                stack.push_subroutine_frame(assign_tgt, subroutine_frame)?;
                                continue;
                            }
                        }
                    }
                }
            },
            Instruction::MakeCont(ref assign_tgt, ref symbol, ref sub_name, ref argument_targets) => {
                let argument_values: Result<Vec<Value>, ExecutionError> = argument_targets.iter().map(|ref target| {
                    stack.current_frame()?.get_value(target)
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
                                    call_target: None,
                                    running_continuation: None,
                                    instructions: subroutine_instructions,
                                    program_counter: 0,
                                    scope: subroutine_scope,
                                };
                                let continuation = Continuation {
                                    done: false,
                                    symbol: symbol.clone(),
                                    last_value: Value::Unit,
                                    frames: vec!(subroutine_frame),
                                };
                                stack.assign_current_frame(assign_tgt, Value::Cont(Box::new(continuation)))?
                            }
                        }
                    },
                }
            },
            Instruction::RunCont(ref assign_tgt, ref value_tgt) => {
                let cont = match stack.current_frame()?.get_value(value_tgt)? {
                    Value::Cont(cont) => cont,
                    _ => return Err(ExecutionError::TypeError(format!("RUN can only be applied to continuation values"))),
                };
                stack.push_coroutine_frames(assign_tgt, &cont.symbol, &cont.frames)?;
                continue
            },
            Instruction::IsDoneCont(ref assign_tgt, ref value_tgt) => {
                let value = stack.current_frame()?.get_value(value_tgt)?;;
                let done = match value {
                    Value::Cont(cont) => cont.done,
                    _ => return Err(ExecutionError::TypeError(format!("ISDONE can only be applied to continuation values"))),
                };
                stack.assign_current_frame(assign_tgt, Value::Boolean(done))?
            },
            Instruction::LastValueCont(ref assign_tgt, ref value_tgt) => {
                let value = stack.current_frame()?.get_value(value_tgt)?;;
                let last_value = match value {
                    Value::Cont(cont) => cont.last_value,
                    _ => return Err(ExecutionError::TypeError(format!("LASTVALUE can only be applied to continuation values"))),
                };
                stack.assign_current_frame(assign_tgt, last_value)?
            },
            Instruction::SuspendCont(ref symbol, ref value_tgt) => {
                let value: Value = match value_tgt {
                    &None => Value::Unit,
                    &Some(ref value_tgt) => stack.current_frame()?.get_value(value_tgt)?
                };
                stack.advance_current_program_counter()?;
                stack.unwind_coroutine(symbol, value)?;
            },
        };

        stack.advance_current_program_counter()?;

        // {
        //     let mut input = String::new();
        //     stdin().read_line(&mut input).unwrap();
        // }
    }

    Ok(())

}