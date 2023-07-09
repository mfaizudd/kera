use std::rc::Rc;

use kera_macros::Node;

use crate::token::Token;

pub trait Node {
    fn token(&self) -> &Token;
}

macro_rules! define_statements {
    ($($name:ident)*) => {
        #[derive(Debug)]
        pub enum Statement {
            $($name($name),)*
        }

        impl Node for Statement {
            fn token(&self) -> &Token {
                match self {
                    $(Statement::$name(s) => s.token(),)*
                }
            }
        }
    };
}

macro_rules! define_expressions {
    ($($name:ident)*) => {
        #[derive(Debug)]
        #[allow(dead_code)]
        pub enum Expression {
            $($name($name),)*
        }

        impl Node for Expression {
            fn token(&self) -> &Token {
                match self {
                    $(Expression::$name(e) => e.token(),)*
                }
            }
        }
    };
}

define_statements! {
    LetStatement
    ReturnStatement
    Expression
}

define_expressions! {
    Identifier
    IntegerLiteral
    Prefix
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
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

#[derive(Debug, Node)]
pub struct Prefix {
    pub token: Token,
    pub right: Rc<Expression>,
}
