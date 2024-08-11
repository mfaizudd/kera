use std::{collections::HashMap, rc::Rc};

#[derive(Debug, PartialEq)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Return(Rc<Value>),
    Error(String),
    None,
}

pub const TRUE: Value = Value::Boolean(true);
pub const FALSE: Value = Value::Boolean(false);
pub const NONE: Value = Value::None;

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
            Value::None => String::from("Nihil"),
            Value::Error(s) => format!("Kesalahan: {s}"),
        }
    }

    pub fn value_type(&self) -> &str {
        match self {
            Value::Integer(_) => "Bilangan bulat",
            Value::Boolean(_) => "Boolean",
            Value::Return(_) => "Kembalian",
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
            Value::None => Value::None,
            Value::Error(msg) => Value::Error(msg.clone()),
        }
    }
}

pub struct Environment {
    store: HashMap<String, Value>
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new()
        }
    }

    pub fn get(&self, name: String) -> Option<&Value> {
        self.store.get(&name)
    }

    pub fn set(&mut self, name: String, val: Value) {
        self.store.insert(name, val);
    }
}
