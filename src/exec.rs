
use ast::*;
use intermediate::*;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Debug;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum IndexValue {
    Unit,
    Number(i64),
    Boolean(bool)
}

#[derive(Clone, Debug)]
enum Value {
    Unit,
    Number(i64),
    Boolean(bool),
    ContRef(Rc<RefCell<Continuation>>),
    Ref(Rc<RefCell<Value>>),
    ArrayRef(Rc<RefCell<Vec<Value>>>),
    HashRef(Rc<RefCell<HashMap<IndexValue, Value>>>),
}

impl Value {
    fn as_number(&self) -> Result<i64, ExecutionError> {
        match self {
            &Value::Number(n) => Ok(n),
            _ => Err(ExecutionError::TypeError(format!("Not a number: {:?}", self)))
        }
    }

    fn as_index(&self) -> Result<IndexValue, ExecutionError> {
        match self {
            &Value::Unit => Ok(IndexValue::Unit),
            &Value::Number(n) => Ok(IndexValue::Number(n)),
            &Value::Boolean(b) => Ok(IndexValue::Boolean(b)),
            _ => Err(ExecutionError::TypeError(format!("Only Unit, Number, and Boolean can be used as indexes to hash ({:?})", self))),
        }
    }
}

// TODO: RUN should return a hash of last_value and done
// PREREQ: Symbol values (with literals)
#[derive(Clone)]
struct Continuation {
    done: bool,
    last_value: Value,
    frames: Vec<Frame>,
}

impl Debug for Continuation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Continuation")
    }
}

fn lookup_value(scope: &HashMap<Identifier, Value>, target: &Identifier) -> Result<Value, ExecutionError> {
    match scope.get(target) {
        None => Err(ExecutionError::NotInScope(target.clone())),
        Some(value) => Ok(value.clone()),
    }
}

#[derive(Clone)]
enum FrameState {
    Active,
    RunningSubroutine(Identifier),
    RunningContinuation(Identifier, Symbol, Rc<RefCell<Continuation>>),
}

#[derive(Clone)]
struct Frame {
    state: FrameState,
    instructions: Vec<Instruction>,
    program_counter: usize,
    scope: HashMap<Identifier, Value>,
}

impl Frame {
    fn new(instructions: Vec<Instruction>) -> Self {
        Frame {
            state: FrameState::Active,
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

    fn assign_local(&mut self, id: &Identifier, value: Value) {
        self.scope.insert(id.clone(), value);
    }

    fn on_pop(&mut self, value: Value) -> Result<(), ExecutionError> {
        match self.state {
            FrameState::Active => return Err(ExecutionError::UnsetReturnTarget),
            FrameState::RunningSubroutine(ref id) => {
                self.scope.insert(id.clone(), value);
            },
            FrameState::RunningContinuation(ref id, _, ref rc_cont) => {
                *rc_cont.borrow_mut() = Continuation {
                    done: true,
                    last_value: value,
                    frames: vec!(),
                };
                self.scope.insert(id.clone(), Value::Unit);
            },
        };
        self.state = FrameState::Active;
        Ok(())
    }

    fn get_value(&self, target: &ValueTarget) -> Result<Value, ExecutionError> {
        match target {
            &ValueTarget::Literal(Literal::Number(i)) => Ok(Value::Number(i)),
            &ValueTarget::Literal(Literal::Boolean(b)) => Ok(Value::Boolean(b)),
            &ValueTarget::Local(ref id) => lookup_value(&self.scope, id),
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

    fn assign_current_frame(&mut self, target: &Identifier, value: Value) -> Result<(), ExecutionError> {
        self.with_current_frame(|mut frame| {
            frame.assign_local(target, value); 
            Ok(())
        })
    }

    // TODO: still not great, but better. The second two blocks don't need a mutable reference
    fn assign(&mut self, target: &AssignTarget, value: Value) -> Result<(), ExecutionError> {
        match target {
            &AssignTarget::Local(ref id) => {
                self.assign_current_frame(id, value)
            },
            &AssignTarget::Reference(ref reference_value_tgt) => {
                let reference_value = self.current_frame()?.get_value(reference_value_tgt)?;
                match reference_value {
                    Value::Ref(ref rc_value) => {
                        *rc_value.borrow_mut() = value;
                        Ok(())
                    }
                    _ => return Err(ExecutionError::TypeError(format!("Cannot assign to dereferenced non-reference value."))),
                }
            },
            &AssignTarget::AtIndex(ref object_value_tgt, ref index_value_tgt) => {
                let object_value = self.current_frame()?.get_value(object_value_tgt)?;
                match object_value {
                    Value::ArrayRef(ref rc) => {
                        let idx_value = self.current_frame()?.get_value(index_value_tgt)?.as_number()?;
                        let mut vec = rc.borrow_mut();
                        if idx_value < 0 || idx_value as usize > vec.len() {
                            return Err(ExecutionError::ArrayIndexOutOfBounds(idx_value))
                        } else {
                            let index = idx_value as usize;
                            vec[index] = value;
                            Ok(())
                        }
                    },
                    Value::HashRef(ref rc) => {
                        let index_value = self.current_frame()?.get_value(index_value_tgt)?.as_index()?;
                        rc.borrow_mut().insert(index_value, value);
                        Ok(())
                    },
                    _ => return Err(ExecutionError::TypeError(format!("Cannot assign at index of non-object (array/hash) value."))),
                }
            }
        }
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
                    self.with_current_frame(|mut frame| frame.on_pop(value))?;
                    Ok(None)
                }
            },
        }
    }

    fn push_subroutine_frame(&mut self, assign_id: &Identifier, frame: Frame) -> Result<(), ExecutionError> {
        self.with_current_frame(|mut frame| {
            frame.state = FrameState::RunningSubroutine(assign_id.clone());
            Ok(())
        })?;
        self.frames.push(frame);
        Ok(())
    }

    fn push_coroutine_frames(&mut self, assign_id: &Identifier, symbol: &Symbol, cont_ref: Rc<RefCell<Continuation>>) -> Result<(), ExecutionError> {
        self.with_current_frame(|mut frame| {
            frame.state = FrameState::RunningContinuation(assign_id.clone(), symbol.clone(), cont_ref.clone());
            Ok(())
        })?;
        // I don't like cloning the frames here, makes more sense to "swap", and "replace" on an unwind or pop
        let frames = cont_ref.borrow().frames.clone();
        self.frames.extend_from_slice(frames.as_slice());
        Ok(())
    }

    fn unwind_coroutine(&mut self, symbol: &Symbol, value: Value) -> Result<(), ExecutionError> {
        let innermost_symbol_frame_index = self.frames.iter().rposition(|ref frame| {
            match frame.state {
                FrameState::RunningContinuation(_, ref frame_symbol, _) => *frame_symbol == *symbol,
                _ => false,
            }
        }).ok_or(ExecutionError::UncaughtSuspension(symbol.clone()))?;
        
        let cont_frames = self.frames.split_off(innermost_symbol_frame_index + 1);
        let continuation = Continuation {
            done: false,
            last_value: value,
            frames: cont_frames
        };

        self.with_current_frame(|mut frame| {
            let id = match frame.state {
                FrameState::RunningContinuation(ref id, _, ref cont_ref) => {
                    *cont_ref.borrow_mut() = continuation;
                    id.clone()
                },
                _ => return Err(ExecutionError::Inconceivable(format!("Split frame wasn't running a continuation?"))),
            };
            frame.state = FrameState::Active;
            frame.assign_local(&id, Value::Unit);
            Ok(())
        })
    }
}

#[derive(Debug)]
pub enum ExecutionError {
    NotInScope(Identifier),
    UnknownSubroutine(SubroutineName),
    TypeError(String),
    JumpOutOfBounds,
    UnsetReturnTarget,
    StackUnderflow,
    UncaughtSuspension(Symbol),
    ArrayIndexOutOfBounds(i64),
    HashNoSuchElemtent(IndexValue),
    Inconceivable(String),
}

fn execute_builtin(builtin: &Builtin, arguments: Vec<Value>) -> Result<Value, ExecutionError> {
    match *builtin {
        Builtin::Print => {
            for arg in arguments {
                println!("--> {:?}", arg);
            }
            Ok(Value::Unit)
        },
        Builtin::NewArrayRef =>
            Ok(Value::ArrayRef(Rc::new(RefCell::new(Vec::new())))),
        Builtin::NewHashRef => 
            Ok(Value::HashRef(Rc::new(RefCell::new(HashMap::new())))),
        Builtin::Push =>
            match arguments.as_slice() {
                &[Value::ArrayRef(ref rc_array), ref elem] => {
                    rc_array.borrow_mut().push(elem.clone());
                    Ok(Value::Unit)
                }
                _ => Err(ExecutionError::TypeError(format!("Can only push onto an array ref")))
            },
    }
}

pub fn attach_builtins(program: &mut Program) {
    program.subroutines.insert(SubroutineName(String::from("print")), Subroutine{
        arguments: vec!(VariableName(String::from("value"))),
        implementation: Implementation::Builtin(Builtin::Print),
    });

    program.subroutines.insert(SubroutineName(String::from("newArrayRef")), Subroutine{
        arguments: vec!(),
        implementation: Implementation::Builtin(Builtin::NewArrayRef),
    });

    program.subroutines.insert(SubroutineName(String::from("newHashRef")), Subroutine{
        arguments: vec!(),
        implementation: Implementation::Builtin(Builtin::NewHashRef),
    });

    program.subroutines.insert(SubroutineName(String::from("push")), Subroutine{
        arguments: vec!(VariableName(String::from("arrayref")), VariableName(String::from("elem"))),
        implementation: Implementation::Builtin(Builtin::Push),
    });
}

pub fn execute_program(program: &mut Program) -> Result<(), ExecutionError> {

    attach_builtins(program);

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
                stack.assign(assign_tgt, value)?;
            },
            Instruction::ApplyUnOp(ref assign_tgt, ref op, ref value_tgt) => {
                let value = stack.current_frame()?.get_value(value_tgt)?;
                let result = match (op, &value) {
                    (&UnaryOperator::Minus, &Value::Number(i)) => Value::Number(-i),
                    (&UnaryOperator::Not, &Value::Boolean(b)) => Value::Boolean(!b),
                    (&UnaryOperator::Deref, &Value::Ref(ref rc)) => rc.borrow().clone(),
                    (&UnaryOperator::NewRef, v) => Value::Ref(Rc::new(RefCell::new(v.clone()))),
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
            Instruction::IndexInto(ref assign_tgt, ref obj_value_tgt, ref idx_value_tgt) => {
                let obj_value = stack.current_frame()?.get_value(obj_value_tgt)?;
                let result = match obj_value {
                    Value::ArrayRef(ref rc) => {
                        let idx_value: i64 = stack.current_frame()?.get_value(idx_value_tgt)?.as_number()?;
                        let vec = rc.borrow();
                        if idx_value < 0 || idx_value as usize > vec.len() {
                            return Err(ExecutionError::ArrayIndexOutOfBounds(idx_value))
                        } else {
                            let index = idx_value as usize;
                            vec[index].clone()
                        }
                    },
                    Value::HashRef(ref rc) => {
                        let idx_value: IndexValue = stack.current_frame()?.get_value(idx_value_tgt)?.as_index()?;
                        let hash = rc.borrow();
                        match hash.get(&idx_value) {
                            None => return Err(ExecutionError::HashNoSuchElemtent(idx_value)),
                            Some(v) => v.clone()
                        }
                    },
                    _ => return Err(ExecutionError::TypeError(format!("Only array refs and hash refs can be indexed into!"))),
                };
                stack.assign_current_frame(assign_tgt, result)?;
            }
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
            // TODO DRY call subroutine and make continuation
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
                                let subroutine_scope: HashMap<Identifier, Value> = subroutine.arguments.iter().map(|arg_name| {
                                    Identifier::Variable(arg_name.clone())
                                }).zip(argument_values).collect();
                                let subroutine_instructions = subroutine_instruction_cache.entry(sub_name.clone()).or_insert({
                                    jit(compound_statement)
                                }).clone();
                                let subroutine_frame = Frame {
                                    state: FrameState::Active,
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
            Instruction::MakeCont(ref assign_tgt, ref sub_name, ref argument_targets) => {
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
                                let subroutine_scope: HashMap<Identifier, Value> = subroutine.arguments.iter().map(|arg_name| {
                                    Identifier::Variable(arg_name.clone())
                                }).zip(argument_values).collect();
                                let subroutine_instructions = subroutine_instruction_cache.entry(sub_name.clone()).or_insert({
                                    jit(compound_statement)
                                }).clone();
                                let subroutine_frame = Frame {
                                    state: FrameState::Active,
                                    instructions: subroutine_instructions,
                                    program_counter: 0,
                                    scope: subroutine_scope,
                                };
                                let continuation = Continuation {
                                    done: false,
                                    last_value: Value::Unit,
                                    frames: vec!(subroutine_frame),
                                };
                                stack.assign_current_frame(assign_tgt, Value::ContRef(Rc::new(RefCell::new(continuation))))?
                            }
                        }
                    },
                }
            },
            Instruction::RunCont(ref assign_tgt, ref symbol, ref value_tgt) => {
                let cont = match stack.current_frame()?.get_value(value_tgt)? {
                    Value::ContRef(cont) => cont,
                    _ => return Err(ExecutionError::TypeError(format!("RUN can only be applied to continuation values"))),
                };
                stack.push_coroutine_frames(assign_tgt, symbol, cont)?;
                continue
            },
            Instruction::IsDoneCont(ref assign_tgt, ref value_tgt) => {
                let value = stack.current_frame()?.get_value(value_tgt)?;;
                let done = match value {
                    Value::ContRef(cont) => cont.borrow().done,
                    _ => return Err(ExecutionError::TypeError(format!("ISDONE can only be applied to continuation values"))),
                };
                stack.assign_current_frame(assign_tgt, Value::Boolean(done))?
            },
            Instruction::LastValueCont(ref assign_tgt, ref value_tgt) => {
                let value = stack.current_frame()?.get_value(value_tgt)?;
                let last_value = match value {
                    Value::ContRef(cont) => cont.borrow().last_value.clone(),
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