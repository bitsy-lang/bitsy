use super::*;
use crate::reference::Reference;

/// The bitwidth of a [`Type::Word`].
pub type Width = u64;

/// The length of a [`Type::Vec`].
pub type Length = u64;

/// A user-defined `enum` type.
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

/// A type classifier for [`Value`]s.
#[derive(Clone, PartialEq)]
pub enum Type {
    /// An n-bit two's complement integer. Nominally unsigned. Written `Word<n>`.
    Word(Width),
    /// A n-element vector. Written `Vec<T, n>`.
    Vec(Box<Type>, Length),
    /// A reference to a user-defined `enum`.
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


