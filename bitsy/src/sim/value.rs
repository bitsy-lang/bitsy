use num_bigint::BigUint;
use crate::types::*;

/// A value used in the simulator (see [`crate::sim::Sim`]).
#[derive(Clone, Default, PartialEq)]
pub enum Value {
    /// An undefined value.
    #[default]
    X,
    /// An element of `Word<n>`.
    Word(Width, BigUint),
    /// An element of `Vec<T, n>`.
    Vec(Vec<Value>),
    /// An element of `Valid<T>`.
    Ctor(String, Vec<Value>),
    /// An element of a user-defined `enum`.
    Enum(Type, String),
    Struct(Type, Vec<(String, Value)>),
}

impl Value {
    pub fn is_x(&self) -> bool {
        if let Value::X = self {
            true
        } else {
            false
        }
    }

    pub fn to_word(&self) -> Option<BigUint> {
        match self {
            Value::X => None,
            Value::Word(w, n) => Some(n & ((BigUint::from(1u64) << w) - BigUint::from(1u64))),
            Value::Vec(_vs) => panic!(),
            Value::Enum(_typedef, _name) => panic!(),
            Value::Struct(_typedef, _fields) => panic!(),
            Value::Ctor(_ctor, _e) => None,
        }
    }

    pub fn to_bool(&self) -> Option<bool> {
        match self {
            Value::Word(1, n) => Some(n == &BigUint::from(1u64)),
            _ => None,
        }
    }
}

#[test]
fn value_to_usize() {
    // TODO move this to tests.
    let v: Value = Value::Word(4, BigUint::from(7u64));
    assert_eq!(v.to_word(), Some(BigUint::from(7u64)));
    let v: Value = Value::Word(2, BigUint::from(7u64));
    assert_eq!(v.to_word(), Some(BigUint::from(3u64)));
}

impl From<bool> for Value {
    fn from(x: bool) -> Value {
        Value::Word(1, if x { BigUint::from(1u64) } else { BigUint::from(0u64) })
    }
}

impl TryFrom<Value> for bool {
    type Error = ();
    fn try_from(value: Value) -> Result<bool, Self::Error> {
        match value {
            Value::Word(1, n) => Ok(n == BigUint::from(1u64)),
            _ => Err(()),
        }
    }
}

/*
// TODO
impl TryFrom<Value> for u8 {
    type Error = ();
    fn try_from(value: Value) -> Result<u8, Self::Error> {
        match value {
            Value::Word(_w, n) => Ok(n as u8), // TODO
            _ => Err(()),
        }
    }
}
*/

/*
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
*/

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
            Value::Word(w, _n) => write!(f, "0x{:x}w{w}", self.to_word().unwrap()),
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
            Value::Word(w, _n) => write!(f, "0x{:X}w{w}", self.to_word().unwrap()),
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
            Value::Word(w, _n) => write!(f, "0b{:b}w{w}", self.to_word().unwrap()),
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
