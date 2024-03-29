use super::*;
use crate::sim::NetId;
use std::collections::BTreeSet;
use std::sync::Arc;
use once_cell::sync::OnceCell;

/// An expression.
#[derive(Clone, Debug)]
pub enum Expr {
    /// A referenec to a port, reg, or node.
    Reference(Span, OnceCell<Type>, Path),
    /// A referenec to a net. Used only in [`crate::sim::Sim`].
    Net(Span, OnceCell<Type>, NetId),
    /// A literal Word.
    Word(Span, OnceCell<Type>, Option<Width>, u64),
    /// A literal enum value.
    Enum(Span, OnceCell<Type>, Type, String),
    /// Constructor (for `Valid<T>`)
    Ctor(Span, OnceCell<Type>, String, Vec<Arc<Expr>>),
    /// Constructor for structs. Eg, `{ x = 0, y = 0}`.
    Struct(Span, OnceCell<Type>, Vec<(String, Arc<Expr>)>),
    /// Let binding. Eg, `let x = a + b in x + x`.
    Let(Span, OnceCell<Type>, String, Option<Type>, Arc<Expr>, Arc<Expr>),
    /// A unary operation. Eg, `!0b101w3`.
    UnOp(Span, OnceCell<Type>, UnOp, Arc<Expr>),
    /// A binary operation. Eg, `1w8 + 1w8`.
    BinOp(Span, OnceCell<Type>, BinOp, Arc<Expr>, Arc<Expr>),
    /// An `if` expression.
    If(Span, OnceCell<Type>, Arc<Expr>, Arc<Expr>, Arc<Expr>),
    /// A `match` expression.
    Match(Span, OnceCell<Type>, Arc<Expr>, Vec<MatchArm>),
    /// A multiplexer. Eg, `mux(cond, a, b)`.
    Mux(Span, OnceCell<Type>, Arc<Expr>, Arc<Expr>, Arc<Expr>),
    /// A concatenate expression. Eg, `cat(foo, 0w1)`.
    Cat(Span, OnceCell<Type>, Vec<Arc<Expr>>),
    /// A sign extension expression.
    Sext(Span, OnceCell<Type>, Arc<Expr>),
    /// A zero extension expression.
    Zext(Span, OnceCell<Type>, Arc<Expr>),
    /// Try to cast a `Word` to an `enum` type.
    TryCast(Span, OnceCell<Type>, Arc<Expr>),
    /// A word expression. Used to cast user-defined `enum` types to their bit values.
    ToWord(Span, OnceCell<Type>, Arc<Expr>),
    /// A vector constructor expression. Eg, `[0w2, 1w2, 2w2]`.
    Vec(Span, OnceCell<Type>, Vec<Arc<Expr>>),
    IdxField(Span, OnceCell<Type>, Arc<Expr>, String),
    /// A static index. Eg, `foo[0]`.
    Idx(Span, OnceCell<Type>, Arc<Expr>, u64),
    /// A static index range. Eg, `foo[8..4]`.
    IdxRange(Span, OnceCell<Type>, Arc<Expr>, u64, u64),
    /// A function call. Eg, `foo(x, y)`.
    Call(Span, OnceCell<Type>, Arc<FnDef>, Vec<Arc<Expr>>),
    /// A hole. Eg, `?foo`.
    Hole(Span, OnceCell<Type>, Option<String>),
}

/// A [`MatchArm`] is a case in a `match` expression.
///
/// In the expression:
///
/// ``` bitsy
/// match v {
///     @Valid(n) => n;
///     @Invalid => 0;
/// }
/// ```
///
/// The two match arms are `@Valid(n) => n;` and `@Invalid => 0;`.
#[derive(Clone, Debug)]
pub struct MatchArm(pub Pat, pub Arc<Expr>);

/// A [`Pat`] is a pattern for a `match` expression.
///
/// Notably different about Bitsy from other languages with a `match` or `case` statement,
/// constructors and constant patterns are prefixed by an `@` symbol to distinguish them
/// from variable bindings. For instance:
///
/// * `@Valid(n)`
/// * `@Invalid`
/// * `@Opcode::OP`
#[derive(Clone, Debug)]
pub enum Pat {
    At(String, Vec<Pat>),
    Bind(String),
    Otherwise,
}

impl Pat {
    pub fn bound_vars(&self) -> Vec<String> {
        let mut results = vec![];
        match self {
            Pat::At(_s, pats) => {
                for pat in pats {
                    results.extend(pat.bound_vars());
                }
            },
            Pat::Bind(x) => results.push(x.clone()),
            Pat::Otherwise => (),
        }
        results.sort();
        results.dedup();
        results
    }
}

impl MatchArm {
    fn free_vars(&self) -> Vec<Path> {
        let mut results = vec![];
        let MatchArm(pat, e) = self;
        let bound_vars = pat.bound_vars();
        for x in e.free_vars().iter() {
            if !bound_vars.contains(&x.to_string().into()) {
                results.push(x.clone());
            }
        }
        results.sort();
        results.dedup();
        results

    }
}

impl HasSpan for Expr {
    fn span(&self) -> Span {
        match self {
            Expr::Net(span, _typ, _netid) => span.clone(),
            Expr::Reference(span, _typ, _path) => span.clone(),
            Expr::Word(span, _typ, _width, _val) => span.clone(),
            Expr::Enum(span, _typ, _typedef, _name) => span.clone(),
            Expr::Ctor(span, _typ, _name, _e) => span.clone(),
            Expr::Struct(span, _typ, _fields) => span.clone(),
            Expr::Let(span, _typ, _name, _typascription, _e, _b) => span.clone(),
            Expr::UnOp(span, _typ, _op, _e) => span.clone(),
            Expr::BinOp(span, _typ, _op, _e1, _e2) => span.clone(),
            Expr::If(span, _typ, _cond, _e1, _e2) => span.clone(),
            Expr::Match(span, _typ, _e, _arms) => span.clone(),
            Expr::Mux(span, _typ, _cond, _e1, _e2) => span.clone(),
            Expr::Cat(span, _typ, _es) => span.clone(),
            Expr::Sext(span, _typ, _e) => span.clone(),
            Expr::Zext(span, _typ, _e) => span.clone(),
            Expr::TryCast(span, _typ, _e) => span.clone(),
            Expr::ToWord(span, _typ, _e) => span.clone(),
            Expr::Vec(span, _typ, _es) => span.clone(),
            Expr::IdxField(span, _typ, _e, _field) => span.clone(),
            Expr::Idx(span, _typ, _e, _i) => span.clone(),
            Expr::IdxRange(span, _typ, _e, _j, _i) => span.clone(),
            Expr::Call(span, _typ, _fndef, _es) => span.clone(),
            Expr::Hole(span, _typ, _opt_name) => span.clone(),
        }
    }
}

impl HasSpan for Arc<Expr> {
    fn span(&self) -> Span {
        let e: &Expr = &*self;
        e.span()
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum UnOp {
    Not,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum BinOp {
    Add,
    AddCarry,
    Sub,
//    SubBorrow,
    And,
    Or,
    Xor,
    Eq,
    Neq,
    Lt,
}

impl Expr {
    pub fn assert_has_types(&self) {
        let mut func = |e: &Expr| {
            if let Expr::Word(_loc, typ, _width, _n) = e {
                typ.get().expect(&format!("Expression was not typechecked: {e:?}"));
            }
        };
        self.with_subexprs(&mut func);
    }

    /// Walk the expression tree in-order, calling `callback` for each subexpression.
    pub fn with_subexprs(&self, callback: &mut dyn FnMut(&Expr)) {
        match self {
            Expr::Reference(_loc, _typ, _path) => callback(self),
            Expr::Net(_loc, _typ, _netid) => callback(self),
            Expr::Word(_loc, _typ, _width, _value) => callback(self),
            Expr::Enum(_loc, _typ, _typedef, _name) => callback(self),
            Expr::Ctor(_loc, _typ, _name, es) => {
                callback(self);
                for e in es {
                    e.with_subexprs(callback);
                }
            },
            Expr::Struct(_loc, _typ, fields) => {
                callback(self);
                for (_name, e) in fields {
                    e.with_subexprs(callback);
                }
            },
            Expr::Let(_loc, _typ, _name, _typascription, e, b) => {
                callback(self);
                e.with_subexprs(callback);
                b.with_subexprs(callback);
            },
            Expr::UnOp(_loc, _typ, _op, e) => {
                callback(self);
                e.with_subexprs(callback);
            }
            Expr::BinOp(_loc, _typ, _op, e1, e2) => {
                callback(self);
                e1.with_subexprs(callback);
                e2.with_subexprs(callback);
            },
            Expr::If(_loc, _typ, cond, e1, e2) => {
                callback(self);
                cond.with_subexprs(callback);
                e1.with_subexprs(callback);
                e2.with_subexprs(callback);
            },
            Expr::Match(_loc, _typ, e, arms) => {
                callback(self);
                callback(e);
                for MatchArm(_pat, arm_e) in arms {
                    arm_e.with_subexprs(callback);
                }
            }
            Expr::Mux(_loc, _typ, cond, e1, e2) => {
                callback(self);
                cond.with_subexprs(callback);
                e1.with_subexprs(callback);
                e2.with_subexprs(callback);
            },
            Expr::Cat(_loc, _typ, es) => {
                callback(self);
                for e in es {
                    e.with_subexprs(callback);
                }
            },
            Expr::Sext(_loc, _typ, e) => {
                callback(self);
                e.with_subexprs(callback);
            },
            Expr::Zext(_loc, _typ, e) => {
                callback(self);
                e.with_subexprs(callback);
            },
            Expr::TryCast(_loc, _typ, e) => {
                callback(self);
                e.with_subexprs(callback);
            },
            Expr::ToWord(_loc, _typ, e) => {
                callback(self);
                e.with_subexprs(callback);
            },
            Expr::Vec(_loc, _typ, es) => {
                callback(self);
                for e in es {
                    e.with_subexprs(callback);
                }
            },
            Expr::IdxField(_loc, _typ, e, _field) => {
                callback(self);
                e.with_subexprs(callback);
            },
            Expr::Idx(_loc, _typ, e, _i) => {
                callback(self);
                e.with_subexprs(callback);
            },
            Expr::IdxRange(_loc, _typ, e, _j, _i) => {
                callback(self);
                e.with_subexprs(callback);
            },
            Expr::Call(_loc, _typ, _fndef, es) => {
                callback(self);
                for e in es {
                    e.with_subexprs(callback);
                }
            },
            Expr::Hole(_loc, _typ, _name) => {
                callback(self);
            },
        }
    }

    pub fn paths(&self) -> Vec<Path> {
        let paths = std::cell::RefCell::new(vec![]);
        let mut func = |e: &Expr| {
            if let Expr::Reference(_loc, _typ, path) = e {
                paths.borrow_mut().push(path.clone());
            } else if let Expr::Net(_loc, _typ, _netid) = e {
                panic!("paths() only works on symbolic expressions.");
            }
        };
        self.with_subexprs(&mut func);

        let mut results = paths.into_inner();
        results.sort();
        results.dedup();
        results
    }

    pub fn is_constant(&self) -> bool {
        self.free_vars().is_empty()
    }

    pub fn free_vars(&self) -> BTreeSet<Path> {
        match self {
            Expr::Reference(_loc, _typ, path) => vec![path.clone()].iter().cloned().collect(),
            Expr::Net(_loc, _typ, _netid) => BTreeSet::new(),
            Expr::Word(_loc, _typ, _width, _value) => BTreeSet::new(),
            Expr::Enum(_loc, _typ, _typedef, _name) => BTreeSet::new(),
            Expr::Ctor(_loc, _typ, _name, es) => {
                let mut result = BTreeSet::new();
                for e in es {
                    result.extend(e.free_vars())
                }
                result
            },
            Expr::Struct(_loc, _typ, fields) => {
                let mut result = BTreeSet::new();
                for (_name, e) in fields {
                    result.extend(e.free_vars())
                }
                result
            },
            Expr::Let(_loc, _typ, x, _type_ascription, e, b) => {
                let mut result = b.free_vars();
                result.remove(&x.to_string().into());
                result.union(&e.free_vars()).cloned().collect()
            },
            Expr::UnOp(_loc, _typ, _op, e) => e.free_vars(),
            Expr::BinOp(_loc, _typ, _op, e1, e2) => e1.free_vars().union(&e2.free_vars()).cloned().collect(),
            Expr::If(_loc, _typ, cond, e1, e2) => {
                cond.free_vars()
                    .union(&e1.free_vars())
                    .cloned()
                    .collect::<BTreeSet<_>>()
                    .union(&e2.free_vars())
                    .cloned()
                    .collect()
            },
            Expr::Match(_loc, _typ, e, arms) => {
                let mut free_vars: Vec<_> = e.free_vars().into_iter().collect();
                for arm in arms {
                    free_vars.extend(arm.free_vars());
                }
                free_vars.into_iter().collect()
            },
            Expr::Mux(_loc, _typ, cond, e1, e2) => {
                cond.free_vars()
                    .union(&e1.free_vars())
                    .cloned()
                    .collect::<BTreeSet<_>>()
                    .union(&e2.free_vars())
                    .cloned()
                    .collect()
            },
            Expr::Cat(_loc, _typ, es) => {
                let mut result = BTreeSet::new();
                for e in es {
                    result.extend(e.free_vars())
                }
                result
            },
            Expr::TryCast(_loc, _typ, e) => e.free_vars(),
            Expr::ToWord(_loc, _typ, e) => e.free_vars(),
            Expr::Vec(_loc, _typ, es) => {
                let mut result = BTreeSet::new();
                for e in es {
                    result.extend(e.free_vars())
                }
                result
            },
            Expr::Sext(_loc, _typ, e) => e.free_vars(),
            Expr::Zext(_loc, _typ, e) => e.free_vars(),
            Expr::IdxField(_loc, _typ, e, _field) => e.free_vars(),
            Expr::Idx(_loc, _typ, e, _i) => e.free_vars(),
            Expr::IdxRange(_loc, _typ, e, _j, _i) => e.free_vars(),
            Expr::Call(_loc, _typ, _fndef, es) => {
                let mut result = BTreeSet::new();
                for e in es {
                    result.extend(e.free_vars())
                }
                result
            },
            Expr::Hole(_loc, _typ, _name) => BTreeSet::new(),
        }
    }

    pub fn depends_on(&self, path: Path) -> bool {
        self.paths().contains(&path)
    }

    pub fn type_of(&self) -> Type {
        self.type_of_cell().unwrap().get().unwrap().clone()
    }

    pub(crate) fn type_of_cell(&self) -> Option<&OnceCell<Type>> {
        match self {
            Expr::Net(_loc, typ, _netid) => Some(typ),
            Expr::Reference(_loc, typ, _path) => Some(typ),
            Expr::Word(_loc, typ, _width, _val) => Some(typ),
            Expr::Enum(_loc, typ, _typedef, _name) => Some(typ),
            Expr::Ctor(_loc, typ, _name, _e) => Some(typ),
            Expr::Struct(_loc, typ, _fields) => Some(typ),
            Expr::Let(_loc, typ, _name, _typascription, _e, _b) => Some(typ),
            Expr::UnOp(_loc, typ, _op, _e) => Some(typ),
            Expr::BinOp(_loc, typ, _op, _e1, _e2) => Some(typ),
            Expr::If(_loc, typ, _cond, _e1, _e2) => Some(typ),
            Expr::Match(_loc, typ, _e, _arms) => Some(typ),
            Expr::Mux(_loc, typ, _cond, _e1, _e2) => Some(typ),
            Expr::Cat(_loc, typ, _es) => Some(typ),
            Expr::Sext(_loc, typ, _e) => Some(typ),
            Expr::Zext(_loc, typ, _e) => Some(typ),
            Expr::TryCast(_loc, typ, _e) => Some(typ),
            Expr::ToWord(_loc, typ, _e) => Some(typ),
            Expr::Vec(_loc, typ, _es) => Some(typ),
            Expr::Idx(_loc, typ, _e, _i) => Some(typ),
            Expr::IdxField(_loc, typ, _e, _field) => Some(typ),
            Expr::IdxRange(_loc, typ, _e, _j, _i) => Some(typ),
            Expr::Call(_loc, typ, _fndef, _es) => Some(typ),
            Expr::Hole(_loc, typ, _opt_name) => Some(typ),
        }
    }
}
