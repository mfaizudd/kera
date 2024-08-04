#[derive(Debug)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    None
}

impl Value {
    fn inspect(&self) -> String {
        match self {
            Value::Integer(v) => v.to_string(),
            Value::Boolean(v) => v.to_string(),
            Value::None => String::from("Nihil"),
        }
    }
}