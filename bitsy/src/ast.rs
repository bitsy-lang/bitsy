use super::error::BitsyError;
use super::loc::Loc;
use super::loc::SourceInfo;

use super::{BinOp, Length, Name, UnOp, Width, Pat};

use lalrpop_util::lalrpop_mod;
use lalrpop_util::ParseError;
lalrpop_mod!(grammar);

/// A `Package` is a compilation unit built from a single file.
#[derive(Debug, Clone)]
pub struct Package {
    pub items: Vec<Item>,
}

/// A top-level declaration in a [`Package`].
#[derive(Debug, Clone)]
pub enum Item {
    ModDef(ModDef),
    ExtDef(ModDef),
    EnumTypeDef(EnumTypeDef),
    StructTypeDef(StructTypeDef),
}

impl Item {
    pub fn name(&self) -> &str {
        match self {
            Item::ModDef(ModDef(_loc, name, _decls)) => name,
            Item::ExtDef(ModDef(_loc, name, _decls)) => name,
            Item::EnumTypeDef(typedef) => typedef.name.as_str(),
            Item::StructTypeDef(typedef) => typedef.name.as_str(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ModDef(pub Loc, pub Name, pub Vec<Decl>);

/// A [`Component`] is a declaration that lives inside of a `mod` or `ext` definiton.
#[derive(Debug, Clone)]
pub enum Decl {
    Mod(Loc, Name, Vec<Decl>),
    ModInst(Loc, Name, Name),
    Incoming(Loc, Name, Type),
    Outgoing(Loc, Name, Type),
    Node(Loc, Name, Type),
    Reg(Loc, Name, Type, Option<Box<Expr>>),
    Wire(Loc, Wire),
    When(Loc, When),
}

/// A user-defined `enum` type.
#[derive(Debug, Clone)]
pub struct EnumTypeDef {
    pub name: String,
    pub values: Vec<(String, WordLit)>,
}

/// A user-defined `struct` type.
#[derive(Debug, Clone)]
pub struct StructTypeDef {
    pub name: String,
    pub fields: Vec<(String, Type)>,
}

/// An expression.
#[derive(Debug, Clone)]
pub enum Expr {
    /// A referenec to a port, reg, or node.
    Ref(Loc, Target),
    /// A literal Word.
    Word(Loc, Option<Width>, u64),
    /// A literal enum value.
    Enum(Loc, Type, String),
    /// A constructor for a struct type.
    Struct(Loc, Vec<(String, Box<Expr>)>),
    /// A constructor for a Vec
    Vec(Loc, Vec<Expr>),
    /// A call-like expression, including `cat` and constructors like `@Valid`.
    Call(Loc, String, Vec<Expr>),
    /// Let binding. Eg, `let x = a + b in x + x`.
    Let(Loc, String, Box<Expr>, Box<Expr>),
    /// A unary operation. Eg, `!0b101w3`.
    UnOp(Loc, UnOp, Box<Expr>),
    /// A binary operation. Eg, `1w8 + 1w8`.
    BinOp(Loc, BinOp, Box<Expr>, Box<Expr>),
    /// An `if` expression.
    If(Loc, Box<Expr>, Box<Expr>, Box<Expr>),
    /// A `match` expression.
    Match(Loc, Box<Expr>, Vec<MatchArm>),
    /// A field index. Eg, `foo->bar`.
    IdxField(Loc, Box<Expr>, String),
    /// A static index. Eg, `foo[0]`.
    Idx(Loc, Box<Expr>, u64),
    /// A static index over a range. Eg, `foo[8..4]`.
    IdxRange(Loc, Box<Expr>, u64, u64),
    /// A hole. Eg, `?foo`.
    Hole(Loc, Option<String>),
}

/// A reference to a hardware component, either in this module, or in a child module.
#[derive(Debug, Clone)]
pub enum Target {
    Local(Name),
    Nonlocal(Name, Name),
}

/// The different kinds of [`Wire`]s in Bitsy.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WireType {
    /// Direct wire. Written `:=` in the syntax. Connects one terminal to another.
    Direct,
    /// Latched wire. Written `<=` in the syntax. Connects one terminal to the data pin of a register.
    Latch,
    /// Procedural. Written `<=!` in the syntax. Connects one terminal to the data pin of a register.
    Proc,
}

/// [`Wire`]s drive the value of port, node, or register.
#[derive(Debug, Clone)]
pub struct Wire(pub Loc, pub Target, pub Box<Expr>, pub WireType);

/// A [`MatchArm`] is a case for a match expression.
#[derive(Clone, Debug)]
pub struct MatchArm(pub Pat, pub Box<Expr>);

/// A [`WordLit`] is a literal for a hardware integer with an optional width ascription.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WordLit(pub Option<Width>, pub u64);

/// A type classifier for values.
#[derive(Clone, Debug)]
pub enum Type {
    /// An n-bit two's complement integer. Nominally unsigned. Written `Word<n>`.
    Word(Width),
    /// A n-element vector. Written `Vec<T, n>`.
    Vec(Box<Type>, Length),
    /// An optional value. Written `Valid<T>`.
    Valid(Box<Type>),
    /// An unresolved reference to a user-defined type.
    TypeRef(String),
}

/// A [`When`] statement drives procedural logic.
#[derive(Debug, Clone)]
pub struct When(pub Box<Expr>, pub Vec<Wire>);

pub fn parse_package_from_string(package_text: &str) -> Result<Package, Vec<BitsyError>> {
    let source_info = SourceInfo::from_string(package_text);
    match grammar::PackageParser::new().parse(&source_info, &package_text) {
        Err(ParseError::UnrecognizedToken { token, expected }) => {
            let start_idx = token.0;
            let end_idx = token.2;
            let loc = Loc::from(&source_info, start_idx, end_idx);

            let message = format!("Parse error: Expected one of {}", expected.join(" "));
            return Err(vec![BitsyError::ParseError(loc, message)]);
        },
        Err(ParseError::InvalidToken { location }) => {
            let loc = Loc::from(&source_info, location, location + 1);
            let message = format!("Parse error");
            return Err(vec![BitsyError::ParseError(loc, message)]);
        },
        Err(ParseError::ExtraToken { token }) => {
            let start_idx = token.0;
            let end_idx = token.2;
            let loc = Loc::from(&source_info, start_idx, end_idx);
            let message = format!("Parse error: extra token: {token:?}");
            return Err(vec![BitsyError::ParseError(loc, message)]);
        },
        Err(ParseError::UnrecognizedEof { location, expected }) => {
            let loc = Loc::from(&source_info, location, location + 1);
            let message = format!("Parse error: Unexpected end of file: Expected {expected:?}");
            return Err(vec![BitsyError::ParseError(loc, message)]);
        },
        Err(ParseError::User { error }) => {
            let message = format!("Parse error: {error:?}");
            return Err(vec![BitsyError::ParseError(Loc::unknown(), message)]);
        },
        Ok(package) => Ok(package),
    }
}
