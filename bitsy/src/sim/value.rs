use crate::types::*;

/// A value used in the simulator (see [`crate::sim::Sim`]).
#[derive(Clone, Default)]
pub enum Value {
    /// An undefined value.
    #[default]
    X,
    /// An element of `Word[n]`.
    Word(Width, u64),
    /// An element of `Vec[T, n]`.
    Vec(Vec<Value>),
    /// An element of `Valid[T]`.
    Ctor(String, Vec<Value>),
    /// An element of a user-defined `enum`.
    Enum(Type, String),
    Struct(Type, Vec<(String, Value)>),
}

impl PartialEq for Value {
    #[rustfmt::skip]
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::X, Value::X) => true,
            (Value::Word(w1, v1), Value::Word(w2, v2)) => w1 == w2 && v1 == v2,
            (Value::Vec(vs1), Value::Vec(vs2)) => {
                assert_eq!(vs1.len(), vs2.len());
                for (v1, v2) in vs1.iter().zip(vs2.iter()) {
                    if v1 != v2 {
                        return false;
                    }
                }
                true
            },
            (Value::Enum(typ1, name1), Value::Enum(typ2, name2)) => typ1.equals(typ2) && name1 == name2,
            (Value::Struct(typ1, fields1), Value::Struct(typ2, fields2)) => {
                if !typ1.equals(typ2) {
                    return false;
                }
                assert_eq!(fields1.len(), fields2.len());
                for ((name1, v1), (name2, v2)) in fields1.iter().zip(fields2.iter()) {
                    assert_eq!(name1, name2);
                    if v1 != v2 {
                        return false;
                    }
                }
                true
            },
            (Value::Ctor(ctor1, vs1), Value::Ctor(ctor2, vs2)) => {
                if ctor1 != ctor2 {
                    return false;
                }
                for (v1, v2) in vs1.iter().zip(vs2.iter()) {
                    if v1 != v2 {
                        return false;
                    }
                }
                true
            },
            _ => false,
        }
    }
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
            Value::Struct(_typedef, _fields) => panic!(),
            Value::Ctor(_ctor, _e) => None,
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
    // TODO move this to tests.
    let v: Value = Value::Word(4, 7);
    assert_eq!(v.to_u64(), Some(7));
    let v: Value = Value::Word(2, 7);
    assert_eq!(v.to_u64(), Some(3));
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
            Value::Enum(typ, name) => write!(f, "{}::{}", typ.name(), name),
            Value::Struct(_typedef, fields) => {
                let field_strs: Vec<_> = fields.iter().map(|(name, val)| format!("{name} = {val:?}")).collect();
                write!(f, "{{ {} }}", field_strs.join(", "))
            },
            Value::Ctor(ctor, vs) => {
                write!(f, "@{ctor}")?;
                if vs.len() > 0 {
                    write!(f, "(")?;
                    for (i, v) in vs.iter().enumerate() {
                        write!(f, "{v:?}")?;
                        if i + 1 < vs.len() {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, ")")
                } else {
                    Ok(())
                }
            },
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
            Value::Struct(_typedef, _fields) => write!(f, "{self}"),
            Value::Ctor(ctor, vs) => {
                write!(f, "@{ctor}")?;
                if vs.len() > 0 {
                    write!(f, "(")?;
                    for (i, v) in vs.iter().enumerate() {
                        write!(f, "{v:?}")?;
                        if i + 1 < vs.len() {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, ")")
                } else {
                    Ok(())
                }
            },
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
            Value::Struct(_typedef, _fields) => write!(f, "{self}"), // TODO
            Value::Ctor(ctor, vs) => {
                write!(f, "@{ctor}")?;
                if vs.len() > 0 {
                    write!(f, "(")?;
                    for (i, v) in vs.iter().enumerate() {
                        write!(f, "{v:?}")?;
                        if i + 1 < vs.len() {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, ")")
                } else {
                    Ok(())
                }
            },
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
            Value::Struct(_typedef, _fields) => write!(f, "{self}"),
            Value::Ctor(ctor, vs) => {
                write!(f, "@{ctor}")?;
                if vs.len() > 0 {
                    write!(f, "(")?;
                    for (i, v) in vs.iter().enumerate() {
                        write!(f, "{v:?}")?;
                        if i + 1 < vs.len() {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, ")")
                } else {
                    Ok(())
                }
            },
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
            Value::Struct(_typedef, _fields) => write!(f, "{self}"),
            Value::Ctor(ctor, vs) => {
                write!(f, "@{ctor}")?;
                if vs.len() > 0 {
                    write!(f, "(")?;
                    for (i, v) in vs.iter().enumerate() {
                        write!(f, "{v:?}")?;
                        if i + 1 < vs.len() {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, ")")
                } else {
                    Ok(())
                }
            },
        }
    }
}
