
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Number(i64)
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct VariableName(pub String);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SubroutineName(pub String);

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOperator {
    Minus, Not
}

#[derive(Clone, Debug, PartialEq)]
pub enum InfixBinaryOperator {
    Add, Sub, Mul, Div, Mod, And, Or, Eq
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Var(VariableName),
    Lit(Literal),
    ApplyUnOp(UnaryOperator, Box<Expression>),
    ApplyInfixBinOp(Box<Expression>, InfixBinaryOperator, Box<Expression>),
    CallSubByValue(SubroutineName, Vec<Box<Expression>>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>)
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    EvaluateIgnore(Box<Expression>),
    Assignment(VariableName, Box<Expression>),
    Return(Option<Box<Expression>>),
    Empty
}

#[derive(Clone, Debug, PartialEq)]
pub enum StatementList {
    Single(Statement),
    Sequence(Statement, Box<StatementList>)
}

#[derive(Clone, Debug, PartialEq)]
pub struct CompoundStatement(pub StatementList);

#[derive(Clone, Debug, PartialEq)]
pub enum Builtin {
    Print
}

#[derive(Clone, Debug, PartialEq)]
pub enum Implementation {
    Builtin(Builtin),
    Block(CompoundStatement)
}

#[derive(Clone, Debug, PartialEq)]
pub struct Subroutine {
    pub parameters: Vec<VariableName>,
    pub implementation: Implementation
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub subroutines: HashMap<SubroutineName, Subroutine>,
    pub entry: CompoundStatement
}