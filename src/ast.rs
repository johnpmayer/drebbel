
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
    Minus
}

#[derive(Clone, Debug, PartialEq)]
pub enum InfixBinaryOperator {
    Add, Sub, Mul, Div, Mod
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Var(VariableName),
    Lit(Literal),
    ApplyUnOp(UnaryOperator, Box<Expression>),
    ApplyInfixBinOp(Box<Expression>, InfixBinaryOperator, Box<Expression>)
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
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
pub struct Subroutine {
    pub parameters: Vec<VariableName>,
    pub block: CompoundStatement
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub subroutines: HashMap<SubroutineName, Subroutine>,
    pub entry: CompoundStatement
}