use crate::reference::Reference;

pub type Width = u64;
pub type Length = u64;

#[derive(Debug, Clone)]
pub struct TypeDef {
    pub name: String,
    pub values: Vec<(String, Value)>,
}

impl TypeDef {
    pub fn value_of(&self, name: &str) -> Option<Value> {
        for (other_name, value) in &self.values {
            if name == other_name {
                return Some(value.clone());
            }
        }
        None
    }

    pub fn width(&self) -> Width {
        let mut max_width = 0;
        for (_name, value) in &self.values {
            if let Value::Word(w, _n) = value {
                if *w > max_width {
                    max_width = *w;
                }
            } else {
                panic!("Values of typedefs must only be Words.")
            }
        }
        max_width
    }
}

#[derive(Clone, PartialEq)]
pub enum Type {
    Word(Width),
    Vec(Box<Type>, Length),
    TypeDef(Reference<TypeDef>),
}

impl Type {
    pub fn bitwidth(&self) -> Width {
        match self {
            Type::Word(n) => *n,
            Type::Vec(typ, n) => typ.bitwidth() * n,
            Type::TypeDef(_typename) => todo!(), //...;
        }
    }
}

#[derive(Clone, Default, PartialEq)]
pub enum Value {
    #[default]
    X,
    Word(Width, u64),
    Vec(Vec<Value>),
    Enum(Reference<TypeDef>, String),
}

impl Value {
    pub fn is_x(&self) -> bool {
        if let Value::X = self {
            true
        } else {
            false
        }
    }

    pub fn to_u64(&self) -> Option<u64> {
        match self {
            Value::X => None,
            Value::Word(w, n) => Some(n & ((1 << w) - 1)),
            Value::Vec(_vs) => panic!(),
            Value::Enum(_typedef, _name) => panic!(),
        }
    }

    pub fn to_bool(&self) -> Option<bool> {
        match self {
            Value::Word(1, 0) => Some(false),
            Value::Word(1, 1) => Some(true),
            _ => None,
        }
    }
}

#[test]
fn value_to_usize() {
    let v: Value = Value::Word(4, 7);
    assert_eq!(v.to_u64(), Some(7));
    let v: Value = Value::Word(2, 7);
    assert_eq!(v.to_u64(), Some(3));
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Type::Word(n) => write!(f, "Word<{n}>"),
            Type::Vec(typ, n) => write!(f, "Vec<{typ:?}, {n}>"),
            Type::TypeDef(reference) => write!(f, "{}", reference.name()),
        }
    }
}

impl From<bool> for Value {
    fn from(x: bool) -> Value {
        Value::Word(1, if x { 1 } else { 0 })
    }
}

impl TryFrom<Value> for bool {
    type Error = ();
    fn try_from(value: Value) -> Result<bool, Self::Error> {
        match value {
            Value::Word(1, n) => Ok(n == 1),
            _ => Err(()),
        }
    }
}

impl TryFrom<Value> for u8 {
    type Error = ();
    fn try_from(value: Value) -> Result<u8, Self::Error> {
        match value {
            Value::Word(_w, n) => Ok(n as u8), // TODO
            _ => Err(()),
        }
    }
}

impl TryFrom<Value> for u64 {
    type Error = ();
    fn try_from(value: Value) -> Result<u64, Self::Error> {
        match value {
            Value::X => Err(()),
            Value::Word(_w, n) => Ok(n),
            Value::Enum(_typedef, _name) => Err(()),
            _ => Err(()),
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Value::X => write!(f, "XXX"),
            Value::Word(w, n) => write!(f, "{n}w{w}"),
            Value::Vec(vs) => {
                write!(f, "[")?;
                for (i, v) in vs.iter().enumerate() {
                    if i + 1 < vs.len() {
                        write!(f, "{v:?}, ")?;
                    } else {
                        write!(f, "{v:?}")?;
                    }
                }
                write!(f, "]")
            },
            Value::Enum(typedef, name) => write!(f, "{}::{}", typedef.name(), name),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Value::X => write!(f, "XXX"),
            Value::Word(w, n) => write!(f, "{n}w{w}"),
            Value::Vec(vs) => {
                write!(f, "[")?;
                for (i, v) in vs.iter().enumerate() {
                    if i + 1 < vs.len() {
                        write!(f, "{v:?}, ")?;
                    } else {
                        write!(f, "{v:?}")?;
                    }
                }
                write!(f, "]")
            },
            Value::Enum(typedef, name) => write!(f, "{}::{}", typedef.name(), name),
        }
    }
}

impl std::fmt::LowerHex for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::X => write!(f, "XXX"),
            Value::Word(w, _n) => write!(f, "0x{:x}w{w}", self.to_u64().unwrap()),
            Value::Vec(vs) => {
                write!(f, "[")?;
                for (i, v) in vs.iter().enumerate() {
                    if i + 1 < vs.len() {
                        write!(f, "{v:?}, ")?;
                    } else {
                        write!(f, "{v:?}")?;
                    }
                }
                write!(f, "]")
            },
            Value::Enum(typedef, name) => write!(f, "{}::{}", typedef.name(), name),
        }
    }
}

impl std::fmt::UpperHex for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::X => write!(f, "XXX"),
            Value::Word(w, _n) => write!(f, "0x{:X}w{w}", self.to_u64().unwrap()),
            Value::Vec(vs) => {
                write!(f, "[")?;
                for (i, v) in vs.iter().enumerate() {
                    if i + 1 < vs.len() {
                        write!(f, "{v:?}, ")?;
                    } else {
                        write!(f, "{v:?}")?;
                    }
                }
                write!(f, "]")
            },
            Value::Enum(typedef, name) => write!(f, "{}::{}", typedef.name(), name),
        }
    }
}

impl std::fmt::Binary for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::X => write!(f, "XXX"),
            Value::Word(w, _n) => write!(f, "0b{:b}w{w}", self.to_u64().unwrap()),
            Value::Vec(vs) => {
                write!(f, "[")?;
                for (i, v) in vs.iter().enumerate() {
                    if i + 1 < vs.len() {
                        write!(f, "{v:?}, ")?;
                    } else {
                        write!(f, "{v:?}")?;
                    }
                }
                write!(f, "]")
            },
            Value::Enum(typedef, name) => write!(f, "{}::{}", typedef.name(), name),
        }
    }
}
