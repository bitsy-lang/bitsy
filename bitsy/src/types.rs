use super::Context;
use super::Expr;
use super::Path;
use super::loc::Span;

use std::sync::Arc;

pub use crate::ast::WordLit; // re-export

/// The bitwidth of a [`Type::Word`].
pub type Width = u64;

/// The length of a [`Type::Vec`].
pub type Length = u64;

/// A type classifier for [`crate::sim::Value`]s.
#[derive(Clone)]
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
    /// A user-defined `alt`.
    Alt(Arc<AltTypeDef>),
}

impl Type {
    pub fn name(&self) -> &str {
        match self {
            Type::Word(_width) => "Word",
            Type::Vec(_typ, _length) => "Vec",
            Type::Valid(_typ) => "Valid",
            Type::Enum(typedef) => &typedef.name,
            Type::Struct(typedef) => &typedef.name,
            Type::Alt(typedef) => &typedef.name,
        }
    }

    #[rustfmt::skip]
    pub fn equals(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Word(width1),      Type::Word(width2)) => width1 == width2,
            (Type::Vec(typ1, len1),   Type::Vec(typ2, len2)) => len1 == len2 && typ1.equals(typ2),
            (Type::Valid(typ1),       Type::Valid(typ2)) => typ1.equals(typ2),
            (Type::Enum(typedef1),    Type::Enum(typedef2)) => Arc::ptr_eq(typedef1, typedef2),
            (Type::Struct(typedef1),  Type::Struct(typedef2)) => Arc::ptr_eq(typedef1, typedef2),
            (Type::Alt(typedef1),     Type::Alt(typedef2)) => Arc::ptr_eq(typedef1, typedef2),
            _ => false,
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
            Type::Alt(typedef) => todo!(), // TODO typedef.bitwidth(),
        }
    }
}

/// A user-defined `enum` type.
#[derive(Debug, Clone)]
pub struct EnumTypeDef {
    pub name: String,
    pub values: Vec<(String, WordLit)>,
    pub span: Span,
}

/// A user-defined `struct` type.
#[derive(Debug, Clone)]
pub struct StructTypeDef {
    pub name: String,
    pub fields: Vec<(String, Type)>,
    pub span: Span,
}

/// A user-defined `alt` type.
#[derive(Debug, Clone)]
pub struct AltTypeDef {
    pub name: String,
    pub alts: Vec<(String, Vec<Type>)>,
    pub span: Span,
}

impl AltTypeDef {
    pub fn alt(&self, name: &str) -> Option<Vec<Type>> {
        for (nam, typs) in &self.alts {
            if name == nam {
                return Some(typs.clone())
            }
        }
        None
    }
}

/// A user-defined `fn` function.
#[derive(Debug, Clone)]
pub struct FnDef {
    pub span: Span,
    pub name: String,
    pub args: Vec<(String, Type)>,
    pub ret: Type,
    pub body: Arc<Expr>,
}

impl FnDef {
    pub fn context(&self) -> Context<Path, Type> {
        Context::from(self.args.iter().map(|(arg_name, arg_type)| (arg_name.to_string().into(), arg_type.clone())).collect::<Vec<_>>())
    }
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
            Type::Alt(typedef) => write!(f, "{}", typedef.name),
        }
    }
}
