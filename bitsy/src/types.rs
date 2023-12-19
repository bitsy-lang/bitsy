use std::sync::Arc;

pub use crate::ast::WordLit; // re-export

/// The bitwidth of a [`Type::Word`].
pub type Width = u64;

/// The length of a [`Type::Vec`].
pub type Length = u64;

/// A type classifier for [`Value`]s.
#[derive(Clone, PartialEq)]
pub enum Type {
    /// An n-bit two's complement integer. Nominally unsigned. Written `Word<n>`.
    Word(Width),
    /// A n-element vector. Written `Vec<T, n>`.
    Vec(Box<Type>, Length),
    /// An optional value. Written `Valid<T>`.
    Valid(Box<Type>),
    /// A user-defined `enum`.
    Enum(Arc<EnumTypeDef>),
    /// A user-defined `struct`.
    Struct(Arc<StructTypeDef>),
}

impl Type {
    pub fn name(&self) -> &str {
        match self {
            Type::Word(_width) => "Word",
            Type::Vec(_typ, _length) => "Vec",
            Type::Valid(_typ) => "Valid",
            Type::Enum(typedef) => &typedef.name,
            Type::Struct(typedef) => &typedef.name,
        }
    }

    pub fn word(w: Width) -> Type {
        Type::Word(w)
    }

    pub fn vec(typ: Type, n: Length) -> Type {
        Type::Vec(Box::new(typ), n)
    }

    pub fn valid(typ: Type) -> Type {
        Type::Valid(Box::new(typ))
    }

    pub fn bitwidth(&self) -> Width {
        match self {
            Type::Word(n) => *n,
            Type::Valid(typ) => typ.bitwidth() + 1,
            Type::Vec(typ, n) => typ.bitwidth() * n,
            Type::Enum(typedef) => typedef.bitwidth(),
            Type::Struct(typedef) => typedef.bitwidth(),
        }
    }
}

/// A user-defined `enum` type.
#[derive(Debug, Clone, PartialEq)]
pub struct EnumTypeDef {
    pub name: String,
    pub values: Vec<(String, WordLit)>,
}

/// A user-defined `struct` type.
#[derive(Debug, Clone, PartialEq)]
pub struct StructTypeDef {
    pub name: String,
    pub fields: Vec<(String, Type)>,
}

impl StructTypeDef {
    pub fn bitwidth(&self) -> Width {
        self.fields.iter().map(|(_name, typ)| typ.bitwidth()).sum()
    }

    pub fn type_of_field(&self, fieldname: &str) -> Option<Type> {
        for (name, typ) in &self.fields {
            if name == fieldname {
                return Some(typ.clone())
            }
        }
        None
    }
}

impl EnumTypeDef {
    pub fn value_of(&self, name: &str) -> Option<u64> {
        for (other_name, WordLit(_w, value)) in &self.values {
            if name == other_name {
                return Some(*value);
            }
        }
        None
    }

    pub fn bitwidth(&self) -> Width {
        // TODO
        let mut max_width = None;
        for (_name, value) in &self.values {
            if let WordLit(Some(w), _n) = value {
                if let Some(max_w) = max_width {
                    assert_eq!(*w, max_w);
                } else {
                    max_width = Some(*w);
                }
             }
        }
        // TODO
        max_width.unwrap()
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Type::Word(n) => write!(f, "Word<{n}>"),
            Type::Valid(typ) => write!(f, "Valid<{typ:?}>"),
            Type::Vec(typ, n) => write!(f, "Vec<{typ:?}, {n}>"),
            Type::Struct(typedef) => write!(f, "{}", typedef.name),
            Type::Enum(typedef) => write!(f, "{}", typedef.name),
        }
    }
}
