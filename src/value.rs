use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::Display,
    hash::{DefaultHasher, Hash, Hasher},
    rc::Rc,
};

use phf::phf_map;

use crate::ast::{Identifier, Statement};

#[derive(Debug)]
pub enum Value {
    Integer(i64),
    String(String),
    Boolean(bool),
    Return(Rc<Value>),
    Function(Rc<Function>),
    Builtin(Rc<BuiltinFunction>),
    Error(String),
    Array(Rc<Array>),
    Hash(Rc<HMap>),
    None,
}

pub const TRUE: Value = Value::Boolean(true);
pub const FALSE: Value = Value::Boolean(false);
pub const NONE: Value = Value::None;

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Integer(a), Self::Integer(b)) => a == b,
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Boolean(a), Self::Boolean(b)) => a == b,
            (Self::Return(a), Self::Return(b)) => a == b,
            (Self::Error(a), Self::Error(b)) => a == b,
            (Self::Array(a), Self::Array(b)) => a == b,
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
            Value::String(v) => format!("\"{v}\""),
            Value::Boolean(v) => {
                if *v {
                    "benar".into()
                } else {
                    "salah".into()
                }
            }
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
            Value::Builtin(_) => "Fungsi bawaan".into(),
            Value::None => String::from("Nihil"),
            Value::Error(s) => format!("Kesalahan: {s}"),
            Value::Array(v) => v.to_string(),
            Value::Hash(h) => h.to_string(),
        }
    }

    pub fn value_type(&self) -> &str {
        match self {
            Value::Integer(_) => "Integer",
            Value::String(_) => "Untai",
            Value::Boolean(_) => "Boolean",
            Value::Return(_) => "Kembalian",
            Value::Function(_) => "Fungsi",
            Value::Builtin(_) => "Bawaan",
            Value::Error(_) => "Kesalahan",
            Value::None => "Nihil",
            Value::Array(_) => "Larik",
            Value::Hash(_) => "Tabel",
        }
    }

    pub fn hash(&self) -> Result<HashKey, String> {
        let mut hasher = DefaultHasher::new();
        match self {
            Value::Integer(i) => {
                i.hash(&mut hasher);
                Ok(HashKey(Value::Integer(i.clone())))
            }
            Value::String(s) => {
                s.hash(&mut hasher);
                Ok(HashKey(Value::String(s.clone())))
            }
            Value::Boolean(b) => {
                b.hash(&mut hasher);
                Ok(HashKey(Value::Boolean(b.clone())))
            }
            _ => Err(format!(
                "Tidak bisa melakukan proses hash pada tipe {}",
                self.value_type()
            )),
        }
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Value::Integer(v) => Value::Integer(*v),
            Value::String(v) => Value::String(v.clone()),
            Value::Boolean(v) => (*v).into(),
            Value::Return(v) => Value::Return(v.clone()),
            Value::Function(v) => Value::Function(v.clone()),
            Value::Builtin(v) => Value::Builtin(v.clone()),
            Value::None => NONE,
            Value::Error(msg) => Value::Error(msg.clone()),
            Value::Array(v) => Value::Array(v.clone()),
            Value::Hash(v) => Value::Hash(v.clone()),
        }
    }
}

#[derive(Debug)]
pub struct Environment {
    store: HashMap<String, Rc<Value>>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(env: Rc<RefCell<Environment>>) -> Self {
        Environment {
            store: HashMap::new(),
            outer: Some(env),
        }
    }

    pub fn get(&self, name: String) -> Option<Rc<Value>> {
        let val = self.store.get(&name).map(|v| (*v).clone());
        if val.is_some() {
            return val;
        }
        let binding = self.outer.as_ref()?.clone();
        let outer = (*binding).borrow();
        outer.get(name)
    }

    pub fn set(&mut self, name: String, val: Rc<Value>) {
        self.store.insert(name, val);
    }
}

#[derive(Debug)]
pub struct Function {
    pub parameters: Rc<Vec<Identifier>>,
    pub body: Rc<Statement>,
    pub env: Rc<RefCell<Environment>>,
}

#[derive(Debug, PartialEq)]
pub struct Array {
    pub elements: Vec<Value>,
}

impl Display for Array {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let elements = self
            .elements
            .iter()
            .map(|v| v.inspect())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "[{}]", elements)
    }
}

#[derive(Debug, PartialEq)]
pub struct HashKey(Value);

impl Eq for HashKey {}

impl Hash for HashKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match &self.0 {
            Value::Integer(i) => i.hash(state),
            Value::String(s) => s.hash(state),
            Value::Boolean(b) => b.hash(state),
            _ => panic!("Unsupported value type: {}", self.0.value_type()),
        }
    }
}

#[derive(Debug)]
pub struct HMap {
    pub pairs: HashMap<HashKey, Value>,
}

impl Display for HMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let elements = self
            .pairs
            .iter()
            .map(|(k, v)| format!("{}: {}", k.0.inspect(), v.inspect()))
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "[{}]", elements)
    }
}

type BuiltinFunction = fn(Vec<Value>) -> Value;

pub static BUILTINS: phf::Map<&'static str, BuiltinFunction> = phf_map! {
    "panjang" => |args| {
        if args.len() != 1 {
            return Value::Error(format!("Jumlah argumen salah. Dapat {}, seharusnya 1", args.len()))
        }
        match &args[0] {
            Value::String(val) => Value::Integer(val.len().try_into().unwrap()),
            Value::Array(arr) => Value::Integer(arr.elements.len().try_into().unwrap()),
            other => Value::Error(format!("Argumen untuk `panjang` tidak didukung ({})", other.value_type()))
        }
    },
    "pertama" => |args| {
        if args.len() != 1 {
            return Value::Error(format!("Jumlah argumen salah. Dapat {}, seharusnya 1", args.len()))
        }
        let Value::Array(arr) = &args[0] else {
            return Value::Error(format!("Argument untuk `pertama` tidak didukung ({})", &args[0].value_type()))
        };
        match arr.elements.first() {
            Some(val) => val.clone(),
            None => Value::None,
        }
    },
    "terakhir" => |args| {
        if args.len() != 1 {
            return Value::Error(format!("Jumlah argumen salah. Dapat {}, seharusnya 1", args.len()))
        }
        let Value::Array(arr) = &args[0] else {
            return Value::Error(format!("Argument untuk `terakhir` tidak didukung ({})", &args[0].value_type()))
        };
        match arr.elements.last() {
            Some(val) => val.clone(),
            None => Value::None,
        }
    },
    "sisa" => |args| {
        if args.len() != 1 {
            return Value::Error(format!("Jumlah argumen salah. Dapat {}, seharusnya 1", args.len()))
        }
        let Value::Array(arr) = &args[0] else {
            return Value::Error(format!("Argument untuk `sisa` tidak didukung ({})", &args[0].value_type()))
        };
        if arr.elements.is_empty() {
            return Value::None;
        }
        Value::Array(Rc::new(Array { elements: arr.elements[1..].to_vec() }))
    },
    "tambah" => |args| {
        if args.len() < 2 {
            return Value::Error(format!("Jumlah argumen salah. Dapat {}, minimal 2", args.len()))
        }
        let Value::Array(arr) = &args[0] else {
            return Value::Error(format!("Argument ke-1 untuk `tambah` tidak didukung ({})", &args[0].value_type()))
        };
        if arr.elements.is_empty() {
            return Value::None;
        }
        let mut elements = arr.elements.clone();
        for arg in &args[1..] {
            if let Value::Error(_) = arg {
                return Value::Error("Tidak dapat menambahkan nilai kesalahan ke dalam larik".into())
            }
            elements.push(arg.clone())
        }
        Value::Array(Rc::new(Array { elements }))
    },
    "tipe_dari" => |args| {
        if args.len() != 1 {
            return Value::Error(format!("Jumlah argumen salah. Dapat {}, seharusnya 1", args.len()))
        }
        return Value::String(args[0].value_type().into())
    }
};
