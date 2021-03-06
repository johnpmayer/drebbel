
use ast::*;

// TODO impl copy, get rid of clone?
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct RegisterName(pub i64);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Identifier {
    Register(RegisterName),
    Variable(VariableName),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ValueTarget {
    Literal(Literal),
    Local(Identifier),
}

impl ValueTarget {
    pub fn lit(literal: &Literal) -> ValueTarget {
        ValueTarget::Literal(literal.clone())
    }

    pub fn reg(reg_name: &RegisterName) -> ValueTarget {
        ValueTarget::Local(Identifier::Register(reg_name.clone()))
    }

    pub fn var(var_name: &VariableName) -> ValueTarget {
        ValueTarget::Local(Identifier::Variable(var_name.clone()))
    }
}

#[derive(Clone, Debug)]
pub enum AssignTarget {
    Local(Identifier),
    Reference(ValueTarget),
    AtIndex(ValueTarget, ValueTarget),
}

impl AssignTarget {
    pub fn reg(register: &RegisterName) -> AssignTarget {
        AssignTarget::Local(Identifier::Register(register.clone()))
    }

    pub fn var(var_name: &VariableName) -> AssignTarget {
        AssignTarget::Local(Identifier::Variable(var_name.clone()))
    }
}

// TODO: can this intermediate type be removed by combining the transform functions?
#[derive(Clone, Debug, PartialEq)]
pub enum AssignmentExpression {
    Var(VariableName),
    Deref(Box<Expression>),
    AtIndex(Box<Expression>, Box<Expression>),
}

#[derive(Clone, Debug)]
pub enum InstructionTree {
    Noop,
    Assign(AssignTarget, ValueTarget),
    ApplyUnOp(Identifier, UnaryOperator, ValueTarget),
    ApplyBinOp(Identifier, ValueTarget, InfixBinaryOperator, ValueTarget),
    IndexInto(Identifier, ValueTarget, ValueTarget),
    Conditional(ValueTarget, Box<InstructionTree>, Box<InstructionTree>),
    CompoundInstruction(Vec<InstructionTree>),
    CallSubroutine(Identifier, SubroutineName, Vec<ValueTarget>),
    Return(Option<ValueTarget>),
    Loop(Box<InstructionTree>, ValueTarget, Vec<InstructionTree>),
    MakeCont(Identifier, SubroutineName, Vec<ValueTarget>),
    RunCont(Identifier, Symbol, ValueTarget),
    SuspendCont(Symbol, Option<ValueTarget>),
}

fn next_register(register_counter: &mut i64) -> Identifier {
    let next_register = RegisterName(*register_counter);
    *register_counter += 1;
    Identifier::Register(next_register)
}

fn transform_expression(register_counter: &mut i64,
                        expression: &Expression) -> (ValueTarget, InstructionTree) {
    match expression {
        &Expression::Lit(ref lit) => (ValueTarget::lit(lit), InstructionTree::Noop),
        &Expression::Var(ref var) => (ValueTarget::var(var), InstructionTree::Noop),
        &Expression::ApplyInfixBinOp(ref l_expr, ref op, ref r_expr) => {
            let (l_value, l_tree) = transform_expression(register_counter, l_expr);
            let (r_value, r_tree) = transform_expression(register_counter, r_expr);
            let next_register = next_register(register_counter);
            let tree = InstructionTree::CompoundInstruction(
                vec!( l_tree
                    , r_tree
                    , InstructionTree::ApplyBinOp(next_register.clone(), l_value, op.clone(), r_value)
                    )
            );
            (ValueTarget::Local(next_register), tree)
        },
        &Expression::ApplyUnOp(ref op, ref r_expr) => {
            let (r_value, r_tree) = transform_expression(register_counter, r_expr);
            let next_register = next_register(register_counter);
            let tree = InstructionTree::CompoundInstruction(
                vec!( r_tree
                    , InstructionTree::ApplyUnOp(next_register.clone(), op.clone(), r_value)
                    )
            );
            (ValueTarget::Local(next_register), tree)
        },
        &Expression::Subscript(ref obj_expr, ref idx_expr) => {
            let (obj_value, obj_tree) = transform_expression(register_counter, obj_expr);
            let (idx_value, idx_tree) = transform_expression(register_counter, idx_expr);
            let next_register = next_register(register_counter);
            let tree = InstructionTree::CompoundInstruction(
                vec!( obj_tree
                    , idx_tree
                    , InstructionTree::IndexInto(next_register.clone(), obj_value, idx_value)
                    )
            );
            (ValueTarget::Local(next_register), tree)
        },
        &Expression::Conditional(ref test_expr, ref truthy_expr, ref falsey_expr) => {
            let (test_value, test_tree) = transform_expression(register_counter, test_expr);
            let result_register = next_register(register_counter);
            let result_target = AssignTarget::Local(result_register.clone());
            let (truthy_value, truthy_tree) = transform_expression(register_counter, truthy_expr);
            let (falsey_value, falsey_tree) = transform_expression(register_counter, falsey_expr);
            let tree = InstructionTree::CompoundInstruction(
                // Seems wrong that test_value is required for conditional?
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
                        )
                    )
            );
            (ValueTarget::Local(result_register), tree)
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
            instructions.push(InstructionTree::CallSubroutine(result_register.clone(),
                                                              sub_name.clone(),
                                                              argument_values));
            let tree = InstructionTree::CompoundInstruction(instructions);
            (ValueTarget::Local(result_register), tree)
        },
        &Expression::MakeCont(ref sub_name, ref arguments) => {
            // TODO un-DRY with the CallSubByValue. just arguments -> instructions
            let mut instructions = Vec::new();
            let mut argument_values = Vec::new();
            for ref arg_expr in arguments {
                let (arg_value, arg_tree) = transform_expression(register_counter, arg_expr);
                instructions.push(arg_tree);
                argument_values.push(arg_value.clone())
            }
            let result_register = next_register(register_counter);
            instructions.push(InstructionTree::MakeCont(result_register.clone(),
                                                        sub_name.clone(),
                                                        argument_values));
            let tree = InstructionTree::CompoundInstruction(instructions);
            (ValueTarget::Local(result_register), tree)
        },
        &Expression::RunCont(ref symbol, ref expr) => {
            let (expr_value, expr_tree) = transform_expression(register_counter, expr);
            let result_register = next_register(register_counter);
            let tree = InstructionTree::CompoundInstruction(
                vec!( expr_tree
                    , InstructionTree::RunCont(result_register.clone(), symbol.clone(), expr_value))
            );
            (ValueTarget::Local(result_register), tree)
        },
    }
}

fn transform_assignment_expression(register_counter: &mut i64, assignment_expression: &AssignmentExpression) -> (AssignTarget, InstructionTree) {
    match assignment_expression {
        &AssignmentExpression::Var(ref variable_name) => (AssignTarget::var(variable_name), InstructionTree::Noop),
        &AssignmentExpression::Deref(ref deref_assignment_expression) => {
            let (expr_value_tgt, expr_tree) = transform_expression(register_counter, deref_assignment_expression);
            (AssignTarget::Reference(expr_value_tgt), expr_tree)
        },
        &AssignmentExpression::AtIndex(ref obj_expression, ref idx_expression) => {
            let (obj_value_tgt, obj_tree) = transform_expression(register_counter, obj_expression);
            let (idx_value_tgt, idx_tree) = transform_expression(register_counter, idx_expression);
            let tree = InstructionTree::CompoundInstruction(vec!( obj_tree, idx_tree));
            (AssignTarget::AtIndex(obj_value_tgt, idx_value_tgt), tree)
        }
    }
}

fn validate_assignment_lhs(expression: &Expression) -> AssignmentExpression {
    match expression {
        &Expression::Var(ref var) => AssignmentExpression::Var(var.clone()),
        &Expression::ApplyUnOp(UnaryOperator::Deref, ref expr) =>
            AssignmentExpression::Deref(expr.clone()),
        &Expression::Subscript(ref obj_expr, ref idx_expr) =>
            AssignmentExpression::AtIndex(obj_expr.clone(), idx_expr.clone()),
        _ => panic!("Invalid LHS expression: {:?}", expression),
    }
}

fn transform_statement(register_counter: &mut i64, statement: &Statement) -> InstructionTree {
    match statement {
        &Statement::Empty => InstructionTree::Noop,
        &Statement::Assignment(ref lhs_expression, ref expression) => {
            let assignment_expression = validate_assignment_lhs(lhs_expression);
            let (assign_tgt, assignment_expression_tree) = transform_assignment_expression(register_counter, &assignment_expression);
            let (value_tgt, expression_tree) = transform_expression(register_counter, expression);
            InstructionTree::CompoundInstruction(
                vec!( expression_tree
                    , assignment_expression_tree
                    , InstructionTree::Assign(assign_tgt, value_tgt)
                    )
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
        &Statement::Conditional(ref test_expr, ref truthy_stmts, ref falsey_stmts) => {
            let (test_value, test_tree) = transform_expression(register_counter, test_expr);
            let truthy_trees = transform_statement_list(register_counter, truthy_stmts);
            let falsey_trees = transform_statement_list(register_counter, falsey_stmts);
            let tree = InstructionTree::CompoundInstruction(
                vec!( test_tree
                    , InstructionTree::Conditional(
                        test_value,
                        Box::new(InstructionTree::CompoundInstruction(truthy_trees)),
                        Box::new(InstructionTree::CompoundInstruction(falsey_trees))
                    ))
            );
            tree
        }
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
    ApplyUnOp(Identifier, UnaryOperator, ValueTarget),
    ApplyBinOp(Identifier, ValueTarget, InfixBinaryOperator, ValueTarget),
    IndexInto(Identifier, ValueTarget, ValueTarget),
    ConditionalJumpRelative(ValueTarget, isize),
    CallSubroutine(Identifier, SubroutineName, Vec<ValueTarget>),
    Return(Option<ValueTarget>),
    MakeCont(Identifier, SubroutineName, Vec<ValueTarget>),
    RunCont(Identifier, Symbol, ValueTarget),
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
            InstructionTree::IndexInto(tgt, obj_val, idx_val) =>
                instructions.push(Instruction::IndexInto(tgt, obj_val, idx_val)),

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

            InstructionTree::MakeCont(tgt, sub_name, args) =>
                instructions.push(Instruction::MakeCont(tgt, sub_name, args)),
            InstructionTree::RunCont(tgt, symbol, val) =>
                instructions.push(Instruction::RunCont(tgt, symbol, val)),
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
