
use ast::*;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct RegisterName(pub i64);

#[derive(Clone, Debug, PartialEq)]
pub enum ValueTarget {
    Literal(Literal),
    Register(RegisterName),
    Variable(VariableName)
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum AssignTarget {
    Register(RegisterName),
    Variable(VariableName)
}

#[derive(Clone, Debug, PartialEq)]
pub enum InstructionTree {
    Noop,
    Assign(AssignTarget, ValueTarget),
    ApplyUnOp(AssignTarget, UnaryOperator, ValueTarget),
    ApplyBinOp(AssignTarget, ValueTarget, InfixBinaryOperator, ValueTarget),
    Conditional(ValueTarget, Box<InstructionTree>, Box<InstructionTree>),
    CompoundInstruction(Vec<InstructionTree>),
    CallSubroutine(AssignTarget, SubroutineName, Vec<ValueTarget>),
    Return(Option<ValueTarget>),
    Loop(Box<InstructionTree>, ValueTarget, Vec<InstructionTree>),
    MakeCont(AssignTarget, Symbol, SubroutineName, Vec<ValueTarget>),
    RunCont(AssignTarget, ValueTarget),
    IsDoneCont(AssignTarget, ValueTarget),
    LastValueCont(AssignTarget, ValueTarget),
    SuspendCont(Symbol, Option<ValueTarget>),
}

fn next_register(register_counter: &mut i64) -> RegisterName {
    let next_register = RegisterName(*register_counter);
    *register_counter += 1;
    next_register
}

fn transform_expression(register_counter: &mut i64,
                        expression: &Expression) -> (ValueTarget, InstructionTree) {
    match expression {
        &Expression::Lit(ref lit) => (ValueTarget::Literal(lit.clone()), InstructionTree::Noop),
        &Expression::Var(ref var) => (ValueTarget::Variable(var.clone()), InstructionTree::Noop),
        &Expression::ApplyInfixBinOp(ref l_expr, ref op, ref r_expr) => {
            let (l_value, l_tree) = transform_expression(register_counter, l_expr);
            let (r_value, r_tree) = transform_expression(register_counter, r_expr);
            let next_register = next_register(register_counter);
            let tree = InstructionTree::CompoundInstruction(
                vec!( l_tree
                    , r_tree
                    , InstructionTree::ApplyBinOp(AssignTarget::Register(next_register.clone()), l_value, op.clone(), r_value))
            );
            (ValueTarget::Register(next_register), tree)
        },
        &Expression::ApplyUnOp(ref op, ref r_expr) => {
            let (r_value, r_tree) = transform_expression(register_counter, r_expr);
            let next_register = next_register(register_counter);
            let tree = InstructionTree::CompoundInstruction(
                vec!( r_tree
                    , InstructionTree::ApplyUnOp(AssignTarget::Register(next_register.clone()), op.clone(), r_value))
            );
            (ValueTarget::Register(next_register), tree)
        },
        &Expression::Conditional(ref test_expr, ref truthy_expr, ref falsey_expr) => {
            let (test_value, test_tree) = transform_expression(register_counter, test_expr);
            let result_register = next_register(register_counter);
            let result_target = AssignTarget::Register(result_register.clone());
            let (truthy_value, truthy_tree) = transform_expression(register_counter, truthy_expr);
            let (falsey_value, falsey_tree) = transform_expression(register_counter, falsey_expr);
            let tree = InstructionTree::CompoundInstruction(
                vec!( test_tree
                    , InstructionTree::Conditional(
                        test_value,
                        Box::new(InstructionTree::CompoundInstruction(
                            vec!( truthy_tree
                                , InstructionTree::Assign(result_target.clone(), truthy_value))
                        )),
                        Box::new(InstructionTree::CompoundInstruction(
                            vec!( falsey_tree
                                , InstructionTree::Assign(result_target, falsey_value))
                        ))
                    ))
            );
            (ValueTarget::Register(result_register), tree)
        },
        &Expression::CallSubByValue(ref sub_name, ref arguments) => {
            let mut instructions = Vec::new();
            let mut argument_values = Vec::new();
            for ref arg_expr in arguments {
                let (arg_value, arg_tree) = transform_expression(register_counter, arg_expr);
                instructions.push(arg_tree);
                argument_values.push(arg_value.clone())
            }
            let result_register = next_register(register_counter);
            let result_target = AssignTarget::Register(result_register.clone());
            instructions.push(InstructionTree::CallSubroutine(result_target,
                                                              sub_name.clone(),
                                                              argument_values));
            let tree = InstructionTree::CompoundInstruction(instructions);
            (ValueTarget::Register(result_register), tree)
        },
        &Expression::MakeCont(ref symbol, ref sub_name, ref arguments) => {
            // TODO un-DRY with the CallSubByValue. just arguments -> instructions
            let mut instructions = Vec::new();
            let mut argument_values = Vec::new();
            for ref arg_expr in arguments {
                let (arg_value, arg_tree) = transform_expression(register_counter, arg_expr);
                instructions.push(arg_tree);
                argument_values.push(arg_value.clone())
            }
            let result_register = next_register(register_counter);
            let result_target = AssignTarget::Register(result_register.clone());
            instructions.push(InstructionTree::MakeCont(result_target,
                                                        symbol.clone(),
                                                        sub_name.clone(),
                                                        argument_values));
            let tree = InstructionTree::CompoundInstruction(instructions);
            (ValueTarget::Register(result_register), tree)
        },
        &Expression::RunCont(ref expr) => {
            let (expr_value, expr_tree) = transform_expression(register_counter, expr);
            let result_register = next_register(register_counter);
            let result_target = AssignTarget::Register(result_register.clone());
            let tree = InstructionTree::CompoundInstruction(
                vec!( expr_tree
                    , InstructionTree::RunCont(result_target, expr_value))
            );
            (ValueTarget::Register(result_register), tree)
        },
        &Expression::IsDoneCont(ref expr) => {
            let (expr_value, expr_tree) = transform_expression(register_counter, expr);
            let result_register = next_register(register_counter);
            let result_target = AssignTarget::Register(result_register.clone());
            let tree = InstructionTree::CompoundInstruction(
                vec!( expr_tree
                    , InstructionTree::IsDoneCont(result_target, expr_value)));
            (ValueTarget::Register(result_register), tree)
        },
        &Expression::LastValueCont(ref expr) => {
            let (expr_value, expr_tree) = transform_expression(register_counter, expr);
            let result_register = next_register(register_counter);
            let result_target = AssignTarget::Register(result_register.clone());
            let tree = InstructionTree::CompoundInstruction(
                vec!( expr_tree
                    , InstructionTree::LastValueCont(result_target, expr_value)));
            (ValueTarget::Register(result_register), tree)
        },
    }
}

fn transform_statement(register_counter: &mut i64, statement: &Statement) -> InstructionTree {
    match statement {
        &Statement::Empty => InstructionTree::Noop,
        &Statement::Assignment(ref variable_name, ref expression) => {
            let (value, expression_tree) = transform_expression(register_counter, expression);
            InstructionTree::CompoundInstruction(
                vec!( expression_tree
                    , InstructionTree::Assign(AssignTarget::Variable(variable_name.clone()), value))
            )
        },
        &Statement::EvaluateIgnore(ref expression) => {
            let (_, tree) = transform_expression(register_counter, expression);
            tree
        },
        &Statement::Return(None) => {
            InstructionTree::Return(None)
        },
        &Statement::Return(Some(ref expression)) => {
            let (value, tree) = transform_expression(register_counter, expression);
            InstructionTree::CompoundInstruction(
                vec!( tree
                    , InstructionTree::Return(Some(value)))
            )
        },
        &Statement::Loop(ref test_expression, ref block) => {
            let (test_value_target, test_tree) = transform_expression(register_counter, test_expression);
            let body_trees = transform_statement_list(register_counter, block);
            InstructionTree::Loop(Box::new(test_tree),
                                  test_value_target,
                                  body_trees)
        },
        &Statement::SuspendCont(ref symbol, None) => {
            InstructionTree::SuspendCont(symbol.clone(), None)
        },
        &Statement::SuspendCont(ref symbol, Some(ref expression)) => {
            let (expr_value, expr_tree) = transform_expression(register_counter, expression);
            InstructionTree::CompoundInstruction(
                vec!( expr_tree
                    , InstructionTree::SuspendCont(symbol.clone(), Some(expr_value))))
        },
    }
}

fn transform_statement_list(register_counter: &mut i64, statement_list: &StatementList) -> Vec<InstructionTree> {

    let mut instructions = Vec::new();
    let mut statments = statement_list.iter();

    while let Some(ref stmt) = statments.next() {
        let tree = transform_statement(register_counter, stmt);
        instructions.push(tree)
    }

    instructions

}

pub fn transform_compound_statement(compound_statement: &CompoundStatement) -> Vec<InstructionTree> {

    let mut register_counter = 0;
    transform_statement_list(&mut register_counter, &compound_statement.statement_list)

}

#[derive(Clone, Debug)]
pub enum Instruction {
    Assign(AssignTarget, ValueTarget),
    ApplyUnOp(AssignTarget, UnaryOperator, ValueTarget),
    ApplyBinOp(AssignTarget, ValueTarget, InfixBinaryOperator, ValueTarget),
    ConditionalJumpRelative(ValueTarget, isize),
    CallSubroutine(AssignTarget, SubroutineName, Vec<ValueTarget>),
    Return(Option<ValueTarget>),
    MakeCont(AssignTarget, Symbol, SubroutineName, Vec<ValueTarget>),
    RunCont(AssignTarget, ValueTarget),
    IsDoneCont(AssignTarget, ValueTarget),
    LastValueCont(AssignTarget, ValueTarget),
    SuspendCont(Symbol, Option<ValueTarget>),
}

const ALWAYS: ValueTarget = ValueTarget::Literal(Literal::Boolean(true));

pub fn flatten_instruction_tree(instruction_tree: Vec<InstructionTree>) -> Vec<Instruction> {

    let mut instructions: Vec<Instruction> = Vec::new();

    for instruction in instruction_tree.into_iter() {
        match instruction {
            InstructionTree::Noop => (),

            InstructionTree::Assign(tgt, val) =>
                instructions.push(Instruction::Assign(tgt, val)),
            InstructionTree::ApplyUnOp(tgt, op, val) =>
                instructions.push(Instruction::ApplyUnOp(tgt, op, val)),
            InstructionTree::ApplyBinOp(tgt, l_val, op, r_val) =>
                instructions.push(Instruction::ApplyBinOp(tgt, l_val, op, r_val)),

            InstructionTree::Conditional(test_val, truthy_tree, falsey_tree) => {
                let mut falsey_insns = flatten_instruction_tree(vec!(*falsey_tree));
                let falsey_jump_distance = (falsey_insns.len() as isize) + 2;
                // If true, jump past the falsey code + the insn which always jumps past the truthy code
                instructions.push(Instruction::ConditionalJumpRelative(test_val, falsey_jump_distance));
                // Execute the falsey code
                instructions.append(&mut falsey_insns);
                let mut truthy_insns = flatten_instruction_tree(vec!(*truthy_tree));
                let truthy_jump_distance = (truthy_insns.len() as isize) + 1;
                // Always jump past the truthy code
                instructions.push(Instruction::ConditionalJumpRelative(ALWAYS, truthy_jump_distance));
                // Execute the truthy code
                instructions.append(&mut truthy_insns);
                // Continue program execution
            }

            InstructionTree::Loop(test_tree, test_val, body_trees) => {
                let mut body_insns = flatten_instruction_tree(body_trees);
                let body_jump_distance = body_insns.len() as isize;
                // Initially, jump past the body
                instructions.push(Instruction::ConditionalJumpRelative(ALWAYS, body_jump_distance + 1));
                // Execute the body
                instructions.append(&mut body_insns);
                let mut test_insns = flatten_instruction_tree(vec!(*test_tree));
                let while_jump_distance = - (body_jump_distance + (test_insns.len() as isize));
                // Execute the test code
                instructions.append(&mut test_insns);
                // If true, jump backwards to the start of the body, otherwise break
                instructions.push(Instruction::ConditionalJumpRelative(test_val, while_jump_distance));
                // Continue program execution
            }

            InstructionTree::CompoundInstruction(insns) =>
                instructions.append(&mut flatten_instruction_tree(insns)),
            InstructionTree::CallSubroutine(tgt, sub_name, args) =>
                instructions.push(Instruction::CallSubroutine(tgt, sub_name, args)),
            InstructionTree::Return(val) =>
                instructions.push(Instruction::Return(val)),

            InstructionTree::MakeCont(tgt, symbol, sub_name, args) =>
                instructions.push(Instruction::MakeCont(tgt, symbol, sub_name, args)),
            InstructionTree::RunCont(tgt, val) =>
                instructions.push(Instruction::RunCont(tgt, val)),
            InstructionTree::IsDoneCont(tgt, val) =>
                instructions.push(Instruction::IsDoneCont(tgt, val)),
            InstructionTree::LastValueCont(tgt, val) =>
                instructions.push(Instruction::LastValueCont(tgt, val)),
            InstructionTree::SuspendCont(symbol, opt_val) =>
                instructions.push(Instruction::SuspendCont(symbol, opt_val)),

        }
    }

    instructions

}

pub fn jit(block: &CompoundStatement) -> Vec<Instruction> {
    let tree = transform_compound_statement(block);
    let instructions = flatten_instruction_tree(tree);
    instructions
}
