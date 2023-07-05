use derive_more::Display;

use crate::token::Token;

pub trait Node {
    fn token(&self) -> &Token;
}

#[derive(Debug)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
}

impl Node for Statement {
    fn token(&self) -> &Token {
        match self {
            Statement::LetStatement(s) => &s.token,
            Statement::ReturnStatement(s) => &s.token,
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

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn token(&self) -> Option<&Token> {
        self.statements.get(0).and_then(|s| Some(s.token()))
    }

    pub fn statements(&self) -> &Vec<Statement> {
        &self.statements
    }
}

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expression,
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}
