#![allow(dead_code)]
use super::loc::Loc;
use super::loc::SourceInfo;

use super::{Width, UnOp, BinOp, Name, Length};

// use lalrpop_util::ParseError;
use lalrpop_util::lalrpop_mod;
lalrpop_mod!(ast_grammar);

#[derive(Debug, Clone)]
pub struct Package {
    decls: Vec<Decl>,
}

#[derive(Debug)]
enum ModDecl {
    Component(Component),
    Wire(Wire),
    When(When),
}

/// An expression.
#[derive(Clone, Debug)]
pub enum Expr {
    /// A referenec to a port, reg, or node.
    Id(Loc, String),
    /// A literal Word.
    Word(Loc, Option<Width>, u64),
    /// A literal enum value.
    Enum(Loc, String, String),
    Call(Loc, String, Vec<Expr>),
    Struct(Loc, Vec<(String, Box<Expr>)>),
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
    /// A multiplexer. Eg, `mux(cond, a, b)`.
    Vec(Loc, Vec<Box<Expr>>),
    Dot(Loc, Box<Expr>, String),
    /// A static index. Eg, `foo[0]`.
    Idx(Loc, Box<Expr>, u64),
    IdxRange(Loc, Box<Expr>, u64, u64),
    /// A hole. Eg, `?foo`.
    Hole(Loc, Option<String>),
}

/// A top-level declaration in a [`Package`].
#[derive(Debug, Clone)]
pub enum Decl {
    ModDef(Component),
    ExtDef(Component),
    EnumTypeDef(EnumTypeDef),
    StructTypeDef(StructTypeDef),
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

#[derive(Debug, Clone)]
pub enum Target {
    Local(Name),
    Nonlocal(Name, Name),
}

/// [`Wire`]s drive the value of port, node, or register.
#[derive(Debug, Clone)]
pub struct Wire(pub Loc, pub Target, pub Box<Expr>, pub WireType);

#[derive(Clone, Debug)]
pub struct MatchArm(pub Pat, pub Box<Expr>);

#[derive(Clone, Debug)]
pub enum Pat {
    At(String, Vec<Pat>),
    Bind(String),
    Otherwise,
}

/// A [`Component`] is a declaration that lives inside of a `mod` or `ext` definiton.
#[derive(Debug, Clone)]
pub enum Component {
    Mod(Loc, Name, Vec<Component>, Vec<Wire>, Vec<When>),
    Ext(Loc, Name, Vec<Component>),
    ModInst(Loc, Name, Name),
    Incoming(Loc, Name, Type),
    Outgoing(Loc, Name, Type),
    Node(Loc, Name, Type),
    Reg(Loc, Name, Type, Option<Box<Expr>>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WordLit(pub Option<Width>, pub u64);

/// A user-defined `enum` type.
#[derive(Debug, Clone, PartialEq)]
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

/// A type classifier for [`Value`]s.
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

#[derive(Debug, Clone)]
pub struct When(pub Box<Expr>, pub Vec<Wire>);
