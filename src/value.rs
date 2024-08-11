use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Return(Rc<Value>),
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
        }
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Self::Integer(v) => Self::Integer(v.clone()),
            Self::Boolean(v) => (*v).into(),
            Self::Return(v) => Self::Return(v.clone()),
            Self::None => Self::None,
        }
    }
}
