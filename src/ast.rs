
#[derive(Debug, PartialEq)]
pub enum Literal {
    Number(i64)
}

#[derive(Debug, PartialEq)]
pub struct Variable(pub String);

#[derive(Debug, PartialEq)]
pub enum UnaryOperator {
    Minus
}

#[derive(Debug, PartialEq)]
pub enum InfixBinaryOperator {
    Add, Sub, Mul, Div, Mod
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Var(Variable),
    Lit(Literal),
    ApplyUnOp(UnaryOperator, Box<Expression>),
    ApplyInfixBinOp(Box<Expression>, InfixBinaryOperator, Box<Expression>)
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Assignment(Variable, Box<Expression>),
    Compound(Box<CompoundStatement>),
    Empty
}

#[derive(Debug, PartialEq)]
pub enum StatementList {
    Single(Statement),
    Sequence(Statement, Box<StatementList>)
}

#[derive(Debug, PartialEq)]
pub struct CompoundStatement(pub StatementList);

#[derive(Debug, PartialEq)]
pub struct Program (pub CompoundStatement);