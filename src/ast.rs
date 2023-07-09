use kera_macros::Node;

use crate::token::Token;

pub trait Node {
    fn token(&self) -> &Token;
}

#[derive(Debug)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

impl Node for Statement {
    fn token(&self) -> &Token {
        match self {
            Statement::LetStatement(s) => &s.token,
            Statement::ReturnStatement(s) => &s.token,
            Statement::ExpressionStatement(s) => &s.token,
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
}

impl Node for Expression {
    fn token(&self) -> &Token {
        match self {
            Expression::Identifier(e) => &e.token,
        }
    }
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[allow(dead_code)]
impl Program {
    pub fn statements(&self) -> &Vec<Statement> {
        &self.statements
    }
}

#[derive(Debug, Node)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Debug, Node)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expression,
}

#[derive(Debug, Clone, Node)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

#[derive(Debug, Node)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}
