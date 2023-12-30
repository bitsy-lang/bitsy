use super::error::BitsyError;
use super::loc::Loc;
use super::loc::HasLoc;
use super::loc::SourceInfo;

use super::{BinOp, Length, Name, UnOp, Width, Pat};

use lalrpop_util::lalrpop_mod;
use lalrpop_util::ParseError;
lalrpop_mod!(grammar);

/// A `Package` is a compilation unit built from a single file.
#[derive(Debug, Clone)]
pub struct Package {
    pub imports: Vec<String>,
    pub items: Vec<Item>,
}

/// A top-level declaration in a [`Package`].
#[derive(Debug, Clone)]
pub enum Item {
    ModDef(ModDef),
    ExtDef(ModDef),
    EnumTypeDef(EnumTypeDef),
    StructTypeDef(StructTypeDef),
    AltTypeDef(AltTypeDef),
    FnDef(FnDef),
}

impl Item {
    pub fn name(&self) -> &str {
        match self {
            Item::ModDef(ModDef(_loc, name, _decls)) => name.as_str(),
            Item::ExtDef(ModDef(_loc, name, _decls)) => name.as_str(),
            Item::EnumTypeDef(typedef) => typedef.name.as_str(),
            Item::StructTypeDef(typedef) => typedef.name.as_str(),
            Item::AltTypeDef(typedef) => typedef.name.as_str(),
            Item::FnDef(fndef) => fndef.name.as_str(),
        }
    }
}

impl HasLoc for Item {
    fn loc(&self) -> Loc {
        match self {
            Item::ModDef(ModDef(loc, _name, _decls)) => loc.clone(),
            Item::ExtDef(ModDef(loc, _name, _decls)) => loc.clone(),
            Item::EnumTypeDef(typedef) => typedef.loc.clone(),
            Item::StructTypeDef(typedef) => typedef.loc.clone(),
            Item::AltTypeDef(typedef) => typedef.loc.clone(),
            Item::FnDef(fndef) => fndef.loc.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ModDef(pub Loc, pub Ident, pub Vec<Decl>);

/// A [`Decl`] is a declaration that lives inside of a `mod` or `ext` definiton.
#[derive(Debug, Clone)]
pub enum Decl {
    Mod(Loc, Ident, Vec<Decl>),
    ModInst(Loc, Ident, Ident),
    Incoming(Loc, Ident, Type),
    Outgoing(Loc, Ident, Type),
    Node(Loc, Ident, Type),
    Reg(Loc, Ident, Type, Option<Box<Expr>>),
    Wire(Loc, Wire),
    When(Loc, When),
}

/// A user-defined `enum` type.
#[derive(Debug, Clone)]
pub struct EnumTypeDef {
    pub name: Ident,
    pub values: Vec<(Ident, WordLit)>,
    pub loc: Loc,
}

/// A user-defined `struct` type.
#[derive(Debug, Clone)]
pub struct StructTypeDef {
    pub name: Ident,
    pub fields: Vec<(Ident, Type)>,
    pub loc: Loc,
}

/// A user-defined `alt` type.
#[derive(Debug, Clone)]
pub struct AltTypeDef {
    pub name: Ident,
    pub alts: Vec<(Ident, Vec<Type>)>,
    pub loc: Loc,
}

/// A user-defined function.
#[derive(Debug, Clone)]
pub struct FnDef {
    pub name: Ident,
    pub args: Vec<(Ident, Type)>,
    pub ret: Type,
    pub body: Expr,
    pub loc: Loc,
}

/// An expression.
#[derive(Debug, Clone)]
pub enum Expr {
    /// A referenec to a port, reg, or node.
    Ident(Loc, Ident),
    /// A dotted expression. Eg, `foo.bar`.
    Dot(Loc, Box<Expr>, Ident),
    /// A literal Word.
    Word(Loc, Option<Width>, u64),
    /// A literal enum value.
    Enum(Loc, Type, String),
    /// A constructor for a struct type.
    Struct(Loc, Vec<(String, Box<Expr>)>),
    /// A constructor for a Vec
    Vec(Loc, Vec<Expr>),
    /// A call-like expression, including `cat` and constructors like `@Valid`.
    Call(Loc, Ident, Vec<Expr>),
    /// Let binding. Eg, `let x = a + b in x + x`.
    Let(Loc, Ident, Option<Type>, Box<Expr>, Box<Expr>),
    /// A unary operation. Eg, `!0b101w3`.
    UnOp(Loc, UnOp, Box<Expr>),
    /// A binary operation. Eg, `1w8 + 1w8`.
    BinOp(Loc, BinOp, Box<Expr>, Box<Expr>),
    /// An `if` expression.
    If(Loc, Box<Expr>, Box<Expr>, Box<Expr>),
    /// A `match` expression.
    Match(Loc, Box<Expr>, Vec<MatchArm>),
    /// A field index. Eg, `foo->bar`.
    IdxField(Loc, Box<Expr>, Ident),
    /// A static index. Eg, `foo[0]`.
    Idx(Loc, Box<Expr>, u64),
    /// A static index over a range. Eg, `foo[8..4]`.
    IdxRange(Loc, Box<Expr>, u64, u64),
    /// A hole. Eg, `?foo`.
    Hole(Loc, Option<Ident>),
}

/// A reference to a hardware component, either in this module, or in a child module.
#[derive(Debug, Clone)]
pub enum Target {
    Local(Ident),
    Nonlocal(Ident, Ident),
}

impl Ident {
    pub fn as_str(&self) -> &str {
        &self.name
    }
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
    TypeRef(Ident),
}

/// A [`When`] statement drives procedural logic.
#[derive(Debug, Clone)]
pub struct When(pub Box<Expr>, pub Vec<Wire>);

/// An identifier in the grammar.
#[derive(Debug, Clone)]
pub struct Ident {
    pub loc: Loc,
    pub name: Name,
}

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl HasLoc for Ident {
    fn loc(&self) -> Loc {
        self.loc.clone()
    }
}

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
