
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
    Return(Option<ValueTarget>)
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
        }
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
        }
        &Statement::Return(None) => {
            InstructionTree::Return(None)
        }
        &Statement::Return(Some(ref expression)) => {
            let (value, tree) = transform_expression(register_counter, expression);
            InstructionTree::CompoundInstruction(
                vec!( tree
                    , InstructionTree::Return(Some(value)))
            )
        }
    }
}

pub fn transform_compound_statement(compound_statement: &CompoundStatement) -> Vec<InstructionTree> {

    let mut register_counter = 0;
    let mut instructions = Vec::new();

    let mut statments = compound_statement.statement_list.iter();

    while let Some(ref stmt) = statments.next() {
        let tree = transform_statement(&mut register_counter, stmt);
        instructions.push(tree)
    }

    instructions

}

#[derive(Clone, Debug)]
pub enum Instruction {
    Assign(AssignTarget, ValueTarget),
    ApplyUnOp(AssignTarget, UnaryOperator, ValueTarget),
    ApplyBinOp(AssignTarget, ValueTarget, InfixBinaryOperator, ValueTarget),
    ConditionalJumpRelative(ValueTarget, usize),
    CallSubroutine(AssignTarget, SubroutineName, Vec<ValueTarget>),
    Return(Option<ValueTarget>)
}

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
                let mut truthy_insns = flatten_instruction_tree(vec!(*truthy_tree));
                let mut falsey_insns = flatten_instruction_tree(vec!(*falsey_tree));
                let truthy_jump_distance = truthy_insns.len();
                let falsey_jump_distance = falsey_insns.len();
                // If true, jump past the falsey code + the insn which always jumps past the truthy code
                instructions.push(Instruction::ConditionalJumpRelative(test_val, falsey_jump_distance + 2));
                // Execute the falsey code
                instructions.append(&mut falsey_insns);
                // Always jump past the truthy code
                instructions.push(Instruction::ConditionalJumpRelative(ValueTarget::Literal(Literal::Boolean(true)), truthy_jump_distance + 1));
                // Execute the truthy code
                instructions.append(&mut truthy_insns);
                // Continue program execution
            }

            InstructionTree::CompoundInstruction(insns) =>
                instructions.append(&mut flatten_instruction_tree(insns)),
            InstructionTree::CallSubroutine(tgt, sub_name, args) =>
                instructions.push(Instruction::CallSubroutine(tgt, sub_name, args)),
            InstructionTree::Return(val) =>
                instructions.push(Instruction::Return(val))

        }
    }

    instructions

}

pub fn jit(block: &CompoundStatement) -> Vec<Instruction> {
    let tree = transform_compound_statement(block);
    let instructions = flatten_instruction_tree(tree);
    instructions
}
