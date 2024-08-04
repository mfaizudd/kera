#[derive(Debug)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    None,
}

const TRUE: Value = Value::Boolean(true);
const FALSE: Value = Value::Boolean(false);

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
            Value::None => String::from("Nihil"),
        }
    }
}
