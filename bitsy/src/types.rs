use crate::reference::Reference;
use std::sync::Arc;

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
    Vec(Arc<Type>, Length),
    /// An optional value. Written `Valid<T>`.
    Valid(Arc<Type>),
    /// A reference to a user-defined `enum`.
    TypeDef(Reference<TypeDef>),
}

impl Type {
    pub fn word(w: Width) -> Arc<Type> {
        Arc::new(Type::Word(w))
    }

    pub fn vec(typ: Arc<Type>, n: Length) -> Arc<Type> {
        Arc::new(Type::Vec(typ, n))
    }

    pub fn type_def(typedef: Reference<TypeDef>) -> Arc<Type> {
        Arc::new(Type::TypeDef(typedef))
    }

    pub fn bitwidth(&self) -> Width {
        match self {
            Type::Word(n) => *n,
            Type::Valid(typ) => typ.bitwidth() + 1,
            Type::Vec(typ, n) => typ.bitwidth() * n,
            Type::TypeDef(_typename) => todo!(), //...;
        }
    }
}

#[derive(Debug, Clone)]
pub struct WordLit(pub Width, pub u64);

/// A user-defined `enum` type.
#[derive(Debug, Clone)]
pub struct TypeDef {
    pub name: String,
    pub values: Vec<(String, WordLit)>,
}

impl TypeDef {
    pub fn value_of(&self, name: &str) -> Option<WordLit> {
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
            let WordLit(w, _n) = value;
            if *w > max_width {
                max_width = *w;
            }
        }
        max_width
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Type::Word(n) => write!(f, "Word<{n}>"),
            Type::Valid(typ) => write!(f, "Valid<{typ:?}>"),
            Type::Vec(typ, n) => write!(f, "Vec<{typ:?}, {n}>"),
            Type::TypeDef(reference) => write!(f, "{}", reference.name()),
        }
    }
}
