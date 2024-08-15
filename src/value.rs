use std::{collections::HashMap, rc::Rc};

use crate::ast::{Block, Identifier};

#[derive(Debug)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Return(Rc<Value>),
    Function(Rc<Function>),
    Error(String),
    None,
}

pub const TRUE: Value = Value::Boolean(true);
pub const FALSE: Value = Value::Boolean(false);
pub const NONE: Value = Value::None;

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Integer(a), Self::Integer(b)) => a == b,
            (Self::Boolean(a), Self::Boolean(b)) => a == b,
            (Self::Return(a), Self::Return(b)) => a == b,
            (Self::Error(a), Self::Error(b)) => a == b,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        if value {
            TRUE
        } else {
            FALSE
        }
    }
}

impl Value {
    pub fn inspect(&self) -> String {
        match self {
            Value::Integer(v) => v.to_string(),
            Value::Boolean(v) => v.to_string(),
            Value::Return(v) => v.inspect(),
            Value::Function(v) => {
                let parameters = v
                    .parameters
                    .iter()
                    .map(|p| p.value.as_ref())
                    .collect::<Vec<&str>>()
                    .join(", ");
                format!("fungsi({}) {}", parameters, v.body)
            }
            Value::None => String::from("Nihil"),
            Value::Error(s) => format!("Kesalahan: {s}"),
        }
    }

    pub fn value_type(&self) -> &str {
        match self {
            Value::Integer(_) => "Bilangan bulat",
            Value::Boolean(_) => "Boolean",
            Value::Return(_) => "Kembalian",
            Value::Function(_) => "Fungsi",
            Value::Error(_) => "Kesalahan",
            Value::None => "Nihil",
        }
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Value::Integer(v) => Value::Integer(v.clone()),
            Value::Boolean(v) => (*v).into(),
            Value::Return(v) => Value::Return(v.clone()),
            Value::Function(v) => Value::Function(v.clone()),
            Value::None => NONE,
            Value::Error(msg) => Value::Error(msg.clone()),
        }
    }
}

#[derive(Debug)]
pub struct Environment {
    store: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, name: String) -> Option<&Value> {
        self.store.get(&name)
    }

    pub fn set(&mut self, name: String, val: Value) {
        self.store.insert(name, val);
    }
}

#[derive(Debug)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: Block,
    pub env: Environment,
}
