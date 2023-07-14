use std::{fmt::Display, rc::Rc};

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
    Let
    Return
    Expression
}

define_expressions! {
    Identifier
    IntegerLiteral
    Prefix
    Infix
    Boolean
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Let(s) => write!(f, "misal {} = {}", s.name.value, s.value),
            Statement::Return(s) => write!(f, "kembalikan {}", s.return_value),
            Statement::Expression(s) => write!(f, "{}", s),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(e) => write!(f, "{}", e.value),
            Expression::IntegerLiteral(e) => write!(f, "{}", e.value),
            Expression::Prefix(e) => write!(f, "({}{})", e.token().literal(), e.right),
            Expression::Infix(e) => write!(f, "({} {} {})", e.left, e.token().literal(), e.right),
            Expression::Boolean(e) => write!(f, "{}", if e.value { "benar" } else { "salah" }),
        }
    }
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = String::new();
        for statement in self.statements() {
            result.push_str(format!("{}", statement).as_str());
        }
        write!(f, "{}", result)
    }
}

#[allow(dead_code)]
impl Program {
    pub fn statements(&self) -> &Vec<Statement> {
        &self.statements
    }
}

#[derive(Debug, Node)]
pub struct Let {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Debug, Node)]
pub struct Return {
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

#[derive(Debug, Node)]
pub struct Infix {
    pub token: Token,
    pub left: Rc<Expression>,
    pub right: Rc<Expression>,
}

#[derive(Debug, Node)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}
