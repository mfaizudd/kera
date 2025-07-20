use std::{collections::HashMap, fmt::Display, hash::Hash, rc::Rc};

use derive_more::Display;
use kera_macros::TokenContainer;

use crate::token::{Token, TokenContainer};

pub enum Node<'a> {
    Program(Program),
    Statement(&'a Statement),
    Expression(Rc<Expression>),
}

impl<'a> TokenContainer for Node<'a> {
    fn token(&self) -> &Token {
        match self {
            Node::Program(p) => p.token(),
            Node::Statement(s) => s.token(),
            Node::Expression(e) => e.token(),
        }
    }
}

macro_rules! define_statements {
    ($($name:ident $type:ty)*) => {
        #[derive(Debug, Display)]
        #[allow(dead_code)]
        pub enum Statement {
            $($name($type),)*
        }

        impl TokenContainer for Statement {
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

        impl TokenContainer for Expression {
            fn token(&self) -> &Token {
                match self {
                    $(Expression::$name(e) => e.token(),)*
                }
            }
        }
    };
}

define_statements! {
    Let Let
    Return Return
    Expression Rc<Expression>
    Block Block
}

define_expressions! {
    Identifier
    IntegerLiteral
    StringLiteral
    Prefix
    Infix
    BooleanLiteral
    If
    FunctionLiteral
    Call
    ArrayLiteral
    Index
    HashLiteral
}

impl Hash for Statement {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Statement::Let(let_stmt) => {
                state.write_u8(1);
                let_stmt.name.value.hash(state);
                let_stmt.value.hash(state);
            }
            Statement::Return(return_stmt) => {
                state.write_u8(2);
                return_stmt.return_value.hash(state);
            }
            Statement::Expression(expression) => {
                state.write_u8(3);
                expression.hash(state);
            }
            Statement::Block(block) => {
                state.write_u8(4);
                for ele in &block.statements {
                    ele.hash(state);
                }
            }
        }
    }
}

impl Hash for Expression {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Expression::Identifier(identifier) => {
                state.write_u8(1);
                identifier.value.hash(state);
            }
            Expression::IntegerLiteral(integer_literal) => {
                state.write_u8(2);
                integer_literal.value.hash(state);
            }
            Expression::StringLiteral(string_literal) => {
                state.write_u8(3);
                string_literal.value.hash(state);
            }
            Expression::Prefix(prefix) => {
                state.write_u8(4);
                prefix.right.hash(state);
            }
            Expression::Infix(infix) => {
                state.write_u8(5);
                infix.left.hash(state);
                infix.right.hash(state);
            }
            Expression::BooleanLiteral(boolean_literal) => {
                state.write_u8(6);
                boolean_literal.value.hash(state);
            }
            Expression::If(if_expr) => {
                state.write_u8(7);
                if_expr.condition.hash(state);
            }
            Expression::FunctionLiteral(function_literal) => {
                state.write_u8(8);
                for param in &*function_literal.parameters {
                    param.value.hash(state);
                }
                function_literal.body.hash(state);
            }
            Expression::Call(call) => {
                state.write_u8(9);
                call.function_ident.hash(state);
                for ele in &call.arguments {
                    ele.hash(state);
                }
            }
            Expression::ArrayLiteral(array_literal) => {
                state.write_u8(10);
                for ele in &array_literal.elements {
                    ele.hash(state);
                }
            }
            Expression::Index(index) => {
                state.write_u8(11);
                index.left.hash(state);
                index.index.hash(state);
            }
            Expression::HashLiteral(hash_literal) => {
                state.write_u8(12);
                for (k, v) in &hash_literal.pairs {
                    k.hash(state);
                    v.hash(state);
                }
            }
        }
    }
}

#[cfg_attr(debug_assertions, allow(dead_code, unused_imports))]
impl Expression {
    pub fn new_str(val: &str) -> Self {
        Expression::StringLiteral(StringLiteral {
            token: Token::String(val.into()),
            value: val.into(),
        })
    }

    pub fn new_int(val: i64) -> Self {
        Expression::IntegerLiteral(IntegerLiteral {
            token: Token::Int(val),
            value: val,
        })
    }

    pub fn new_bool(val: bool) -> Self {
        let token = if val { Token::True } else { Token::False };
        Expression::BooleanLiteral(BooleanLiteral { token, value: val })
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

impl TokenContainer for Program {
    fn token(&self) -> &Token {
        if let Some(statement) = self.statements.first() {
            return statement.token();
        }
        &Token::Illegal
    }
}

#[allow(dead_code)]
impl Program {
    pub fn statements(&self) -> &Vec<Statement> {
        &self.statements
    }
}

#[derive(Debug, TokenContainer, Display)]
#[display(fmt = "misal {} = {}", "name.value", "value")]
pub struct Let {
    pub token: Token,
    pub name: Identifier,
    pub value: Rc<Expression>,
}

#[derive(Debug, TokenContainer, Display)]
#[display(fmt = "kembalikan {}", return_value)]
pub struct Return {
    pub token: Token,
    pub return_value: Rc<Expression>,
}

#[derive(Debug, Clone, TokenContainer, Display)]
#[display(fmt = "{}", value)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

#[derive(Debug, TokenContainer, Display)]
#[display(fmt = "{}", value)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

#[derive(Debug, TokenContainer, Display)]
#[display(fmt = "{}", value)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

#[derive(Debug, TokenContainer, Display)]
#[display(fmt = "({}{})", "token.literal()", "right")]
pub struct Prefix {
    pub token: Token,
    pub right: Rc<Expression>,
}

#[derive(Debug, TokenContainer, Display)]
#[display(fmt = "({} {} {})", left, "token.literal()", right)]
pub struct Infix {
    pub token: Token,
    pub left: Rc<Expression>,
    pub right: Rc<Expression>,
}

#[derive(Debug, TokenContainer, Display)]
#[display(fmt = "{}", "token.literal()")]
pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

#[derive(Debug, TokenContainer)]
pub struct If {
    pub token: Token,
    pub condition: Rc<Expression>,
    pub consequence: Rc<Statement>,
    pub alternative: Option<Rc<Statement>>,
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

#[derive(Debug, TokenContainer)]
pub struct Block {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = String::new();
        for statement in self.statements.iter() {
            result.push_str(&format!("{};", statement))
        }
        write!(f, "{{ {} }}", result)
    }
}

#[derive(Debug, TokenContainer)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Rc<Vec<Identifier>>,
    pub body: Rc<Statement>,
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let parameters = self
            .parameters
            .iter()
            .map(|p| p.value.as_ref())
            .collect::<Vec<&str>>()
            .join(", ");
        write!(f, "{}({}) {}", self.token(), parameters, self.body)
    }
}

#[derive(Debug, TokenContainer)]
pub struct Call {
    pub token: Token,
    pub function_ident: Rc<Expression>,
    pub arguments: Vec<Rc<Expression>>,
}

impl Display for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let arguments = self
            .arguments
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "{}({})", self.function_ident, arguments)
    }
}

#[derive(Debug, TokenContainer)]
pub struct ArrayLiteral {
    pub token: Token,
    pub elements: Vec<Rc<Expression>>,
}

impl Display for ArrayLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let elements = self
            .elements
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "[{}]", elements)
    }
}

#[derive(Debug, TokenContainer)]
pub struct Index {
    pub token: Token,
    pub left: Rc<Expression>,
    pub index: Rc<Expression>,
}

impl Display for Index {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}[{}])", self.left, self.index)
    }
}

#[derive(Debug, TokenContainer)]
pub struct HashLiteral {
    pub token: Token,
    pub pairs: HashMap<u64, Rc<Expression>>,
}

impl Display for HashLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let pairs = self
            .pairs
            .iter()
            .map(|(k, v)| format!("{k}: {v}"))
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "{{{}}}", pairs)
    }
}
