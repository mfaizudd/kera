use std::{fmt::Display, rc::Rc};

use derive_more::Display;
use kera_macros::Node;

use crate::token::Token;

pub trait Node {
    fn token(&self) -> &Token;
}

macro_rules! define_statements {
    ($($name:ident)*) => {
        #[derive(Debug, Display)]
        #[allow(dead_code)]
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
        #[derive(Debug, Display)]
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
    Block
}

define_expressions! {
    Identifier
    IntegerLiteral
    Prefix
    Infix
    BooleanLiteral
    If
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

#[derive(Debug, Node, Display)]
#[display(fmt = "misal {} = {}", "name.value", "value")]
pub struct Let {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Debug, Node, Display)]
#[display(fmt = "kembalikan {}", return_value)]
pub struct Return {
    pub token: Token,
    pub return_value: Expression,
}

#[derive(Debug, Clone, Node, Display)]
#[display(fmt = "{}", value)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

#[derive(Debug, Node, Display)]
#[display(fmt = "{}", value)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

#[derive(Debug, Node, Display)]
#[display(fmt = "({}{})", "token.literal()", "right")]
pub struct Prefix {
    pub token: Token,
    pub right: Rc<Expression>,
}

#[derive(Debug, Node, Display)]
#[display(fmt = "({} {} {})", left, "token.literal()", right)]
pub struct Infix {
    pub token: Token,
    pub left: Rc<Expression>,
    pub right: Rc<Expression>,
}

#[derive(Debug, Node, Display)]
#[display(fmt = "{}", "token.literal()")]
pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

#[derive(Debug, Node)]
pub struct If {
    pub token: Token,
    pub condition: Rc<Expression>,
    pub consequence: Block,
    pub alternative: Option<Block>,
}

impl Display for If {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = format!("jika {} {} ", self.condition, self.consequence);
        if let Some(alternative) = &self.alternative {
            result.push_str(&format!("else {}", alternative));
        }
        write!(f, "{}", result)
    }
}

#[derive(Debug, Node)]
pub struct Block {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = String::new();
        for statement in self.statements.iter() {
            result.push_str(&format!("{}", statement))
        }
        write!(f, "{}", result)
    }
}
