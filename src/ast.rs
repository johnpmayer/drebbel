
#[derive(Debug, PartialEq)]
pub enum Literal {
    Number(i64)
}

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
    Lit(Literal),
    ApplyUnOp(UnaryOperator, Box<Expression>),
    ApplyInfixBinOp(Box<Expression>, InfixBinaryOperator, Box<Expression>)
}