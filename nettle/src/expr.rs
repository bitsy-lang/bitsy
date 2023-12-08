mod typecheck;
mod eval;

use super::*;
use crate::sim::NetId;
use std::collections::BTreeMap;

pub use typecheck::TypeError;

/// An expression.
#[derive(Clone)]
pub enum Expr {
    /// A referenec to a port, reg, or node.
    Reference(Loc, Path),
    /// A referenec to a net. Used only in [`crate::sim::Sim`]. See [`Expr::references_to_nets`].
    Net(Loc, NetId),
    /// A literal value.
    Lit(Loc, Value),
    /// A unary operation. Eg, `!0b101w3`.
    UnOp(Loc, UnOp, Box<Expr>),
    /// A binary operation. Eg, `1w8 + 1w8`.
    BinOp(Loc, BinOp, Box<Expr>, Box<Expr>),
    /// An `if` expression.
    If(Loc, Box<Expr>, Box<Expr>, Box<Expr>),
    /// A multiplexer. Eg, `mux(cond, a, b)`.
    Mux(Loc, Box<Expr>, Box<Expr>, Box<Expr>),
    /// A concatenate expression. Eg, `cat(foo, 0w1)`.
    Cat(Loc, Vec<Expr>),
    /// A sign extension expression.
    Sext(Loc, Box<Expr>, u64),
    /// A word expression. Used to cast user-defined `enum` types to their bit values.
    ToWord(Loc, Box<Expr>),
    /// A vector constructor expression. Eg, `[0w2, 1w2, 2w2]`.
    Vec(Loc, Vec<Expr>),
    /// A static index. Eg, `foo[0]`.
    Idx(Loc, Box<Expr>, u64),
    IdxRange(Loc, Box<Expr>, u64, u64),
    /// A static index range. Eg, `foo[8..4]`.
    IdxDyn(Loc, Box<Expr>, Box<Expr>),
    /// A hole. Eg, `?foo`.
    Hole(Loc, Option<String>),
}

impl HasLoc for Expr {
    fn loc(&self) -> Loc {
        match self {
            Expr::Net(loc, _netid) => loc.clone(),
            Expr::Reference(loc, _path) => loc.clone(),
            Expr::Lit(loc, _val) => loc.clone(),
            Expr::UnOp(loc, _op, _e) => loc.clone(),
            Expr::BinOp(loc, _op, _e1, _e2) => loc.clone(),
            Expr::If(loc, _cond, _e1, _e2) => loc.clone(),
            Expr::Mux(loc, _cond, _e1, _e2) => loc.clone(),
            Expr::Cat(loc, _es) => loc.clone(),
            Expr::Sext(loc, _e, _n) => loc.clone(),
            Expr::ToWord(loc, _e) => loc.clone(),
            Expr::Vec(loc, _es) => loc.clone(),
            Expr::Idx(loc, _e, _i) => loc.clone(),
            Expr::IdxRange(loc, _e, _j, _i) => loc.clone(),
            Expr::IdxDyn(loc, _e, _i) => loc.clone(),
            Expr::Hole(loc, _opt_name) => loc.clone(),
        }
    }
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Expr::Net(_loc, netid) => write!(f, "#{netid:?}"),
            Expr::Reference(_loc, path) => write!(f, "{path}"),
            Expr::Lit(_loc, val) => write!(f, "{val:?}"),
            Expr::UnOp(_loc, op, e) => {
                let op_symbol = match op {
                    UnOp::Not => "!",
                };
                write!(f, "({op_symbol}{e:?})")
            },
            Expr::BinOp(_loc, op, e1, e2) => {
                let op_symbol = match op {
                    BinOp::Add => "+",
                    BinOp::AddCarry => "+%",
                    BinOp::Sub => "-",
//                    BinOp::SubBorrow => "-%",
                    BinOp::And => "&&",
                    BinOp::Or => "||",
                    BinOp::Xor => "^",
                    BinOp::Eq => "==",
                    BinOp::Neq => "!=",
                    BinOp::Lt => "<",
                };
                write!(f, "({e1:?} {op_symbol} {e2:?})")
            },
            Expr::If(_loc, cond, e1, e2) => {
                write!(f, "if {cond:?} {{ {e1:?} }} else {{ {e2:?} }}")
            },
            Expr::Mux(_loc, cond, e1, e2) => write!(f, "mux({cond:?}, {e1:?}, {e2:?})"),
            Expr::Cat(_loc, es) => write!(f, "cat({})", es.iter().map(|e| format!("{e:?}")).collect::<Vec<_>>().join(", ")),
            Expr::Sext(_loc, e, n) => write!(f, "sext({e:?}, {n})"),
            Expr::ToWord(_loc, e) => write!(f, "word({e:?})"),
            Expr::Vec(_loc, es) => {
                write!(f, "[")?;
                for (i, e) in es.iter().enumerate() {
                    if i + 1 < es.len() {
                        write!(f, "{e:?}, ")?;
                    } else {
                        write!(f, "{e:?}")?;
                    }
                }
                write!(f, "]")
            },
            Expr::Idx(_loc, e, i) => write!(f, "{e:?}[{i}]"),
            Expr::IdxRange(_loc, e, j, i) => write!(f, "{e:?}[{j}..{i}]"),
            Expr::IdxDyn(_loc, e, i) => write!(f, "{e:?}[{i:?}]"),
            Expr::Hole(_loc, opt_name) => {
                if let Some(_name) = opt_name {
                    todo!()
                } else {
                    write!(f, "?")
                }
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum UnOp {
    Not,
}

#[derive(Debug, Eq, PartialEq, Clone)]
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
    /// Walk the expression tree in-order, calling `callback` for each subexpression.
    pub fn with_subexprs(&self, callback: &dyn Fn(&Expr)) {
        match self {
            Expr::Reference(_loc, _path) => callback(self),
            Expr::Net(_loc, _netid) => callback(self),
            Expr::Lit(_loc, _value) => callback(self),
            Expr::UnOp(_loc, _op, e) => {
                e.with_subexprs(callback);
                callback(self);
            }
            Expr::BinOp(_loc, _op, e1, e2) => {
                e1.with_subexprs(callback);
                e2.with_subexprs(callback);
                callback(self);
            },
            Expr::If(_loc, cond, e1, e2) => {
                cond.with_subexprs(callback);
                e1.with_subexprs(callback);
                e2.with_subexprs(callback);
                callback(self);
            },
            Expr::Mux(_loc, cond, e1, e2) => {
                cond.with_subexprs(callback);
                e1.with_subexprs(callback);
                e2.with_subexprs(callback);
                callback(self);
            },
            Expr::Cat(_loc, es) => {
                for e in es {
                    e.with_subexprs(callback);
                }
                callback(self);
            },
            Expr::Sext(_loc, e, _n) => {
                e.with_subexprs(callback);
                callback(self);
            },
            Expr::ToWord(_loc, e) => {
                e.with_subexprs(callback);
                callback(self);
            },
            Expr::Vec(_loc, es) => {
                for e in es {
                    e.with_subexprs(callback);
                }
                callback(self);
            },
            Expr::Idx(_loc, e, _i) => {
                e.with_subexprs(callback);
                callback(self);
            },
            Expr::IdxRange(_loc, e, _j, _i) => {
                e.with_subexprs(callback);
                callback(self);
            },
            Expr::IdxDyn(_loc, e, _i) => {
                e.with_subexprs(callback);
                callback(self);
            },
            Expr::Hole(_loc, _name) => {
                callback(self);
            },
        }
    }

    /// Walk the expression tree in-order, calling `callback` for each subexpression.
    pub fn with_subexprs_mut(&mut self, callback: &dyn Fn(&mut Expr)) {
        match self {
            Expr::Reference(_loc, _path) => callback(self),
            Expr::Net(_loc, _netid) => callback(self),
            Expr::Lit(_loc, _value) => callback(self),
            Expr::UnOp(_loc, _op, e) => {
                e.with_subexprs_mut(callback);
                callback(&mut *self);
            }
            Expr::BinOp(_loc, _op, e1, e2) => {
                e1.with_subexprs_mut(callback);
                e2.with_subexprs_mut(callback);
                callback(self);
            },
            Expr::If(_loc, cond, e1, e2) => {
                cond.with_subexprs_mut(callback);
                e1.with_subexprs_mut(callback);
                e2.with_subexprs_mut(callback);
                callback(self);
            },
            Expr::Mux(_loc, cond, e1, e2) => {
                cond.with_subexprs_mut(callback);
                e1.with_subexprs_mut(callback);
                e2.with_subexprs_mut(callback);
                callback(self);
            },
            Expr::Cat(_loc, es) => {
                for e in es {
                    e.with_subexprs_mut(callback);
                }
                callback(self);
            },
            Expr::Sext(_loc, e, _n) => {
                e.with_subexprs_mut(callback);
                callback(self);
            },
            Expr::ToWord(_loc, e) => {
                e.with_subexprs_mut(callback);
                callback(self);
            },
            Expr::Vec(_loc, es) => {
                for e in es {
                    e.with_subexprs_mut(callback);
                }
                callback(self);
            },
            Expr::Idx(_loc, e, _i) => {
                e.with_subexprs_mut(callback);
                callback(self);
            },
            Expr::IdxRange(_loc, e, _j, _i) => {
                e.with_subexprs_mut(callback);
                callback(self);
            },
            Expr::IdxDyn(_loc, e, _i) => {
                e.with_subexprs_mut(callback);
                callback(self);
            },
            Expr::Hole(_loc, _name) => {
                callback(self);
            },
        }
    }

    pub fn paths(&self) -> Vec<Path> {
        let paths = std::cell::RefCell::new(vec![]);
        let func = |e: &Expr| {
            if let Expr::Reference(_loc, path) = e {
                paths.borrow_mut().push(path.clone());
            } else if let Expr::Net(_loc, _netid) = e {
                panic!("paths() only works on symbolic expressions.");
            }
        };
        self.with_subexprs(&func);

        let mut results = paths.into_inner();
        results.sort();
        results.dedup();
        results
    }

    pub fn is_constant(&self) -> bool {
        match self {
            Expr::Reference(_loc, _path) => false,
            Expr::Net(_loc, _netid) => false,
            Expr::Lit(_loc, _value) => true,
            Expr::UnOp(_loc, _op, e) => e.is_constant(),
            Expr::BinOp(_loc, _op, e1, e2) => e1.is_constant() && e2.is_constant(),
            Expr::If(_loc, cond, e1, e2) => cond.is_constant() && e1.is_constant() && e2.is_constant(),
            Expr::Mux(_loc, cond, e1, e2) => cond.is_constant() && e1.is_constant() && e2.is_constant(),
            Expr::Cat(_loc, es) => es.iter().all(|e| e.is_constant()),
            Expr::ToWord(_loc, e) => e.is_constant(),
            Expr::Vec(_loc, es) => es.iter().all(|e| e.is_constant()),
            Expr::Sext(_loc, e, _n) => e.is_constant(),
            Expr::Idx(_loc, e, _i) => e.is_constant(),
            Expr::IdxRange(_loc, e, _j, _i) => e.is_constant(),
            Expr::IdxDyn(_loc, e, i) => e.is_constant() && i.is_constant(),
            Expr::Hole(_loc, _name) => false,
        }
    }

    /// Replace all references (see [`Expr::Reference`]) with nets (see [`Expr::Net`])
    /// to get it ready for simulation.
    pub fn references_to_nets(mut self, net_id_by_path: &BTreeMap<Path, NetId>) -> Expr {
        // TODO shouldn't this just take self and not &mut self?
        let func = |e: &mut Expr| {
            match &e {
                Expr::Reference(loc, path) => {
                    if let Some(net_id) = net_id_by_path.get(&path) {
                        *e = Expr::Net(loc.clone(), *net_id);
                    } else {
                        panic!("No net for {path}");
                    }
                },
                _ => (),
            }
        };
        self.with_subexprs_mut(&func);
        self
    }

    pub fn depends_on(&self, path: Path) -> bool {
        self.paths().contains(&path)
    }

    pub fn depends_on_net(&self, net_id: NetId) -> bool {
        match self {
            Expr::Reference(_loc, _path) => panic!("depends_on_net() only works on net expressions."),
            Expr::Net(_loc, other_netid) => net_id == *other_netid,
            Expr::Lit(_loc, _value) => false,
            Expr::UnOp(_loc, _op, e) => e.depends_on_net(net_id),
            Expr::BinOp(_loc, _op, e1, e2) => e1.depends_on_net(net_id) || e2.depends_on_net(net_id),
            Expr::If(_loc, cond, e1, e2) => cond.depends_on_net(net_id) || e1.depends_on_net(net_id) || e2.depends_on_net(net_id),
            Expr::Mux(_loc, cond, e1, e2) => cond.depends_on_net(net_id) || e1.depends_on_net(net_id) || e2.depends_on_net(net_id),
            Expr::Cat(_loc, es) => es.iter().any(|e| e.depends_on_net(net_id)),
            Expr::Sext(_loc, e, _n) => e.depends_on_net(net_id),
            Expr::ToWord(_loc, e) => e.depends_on_net(net_id),
            Expr::Vec(_loc, es) => es.iter().any(|e| e.depends_on_net(net_id)),
            Expr::Idx(_loc, e, _i) => e.depends_on_net(net_id),
            Expr::IdxRange(_loc, e, _j, _i) => e.depends_on_net(net_id),
            Expr::IdxDyn(_loc, e, i) => e.depends_on_net(net_id) || i.depends_on_net(net_id),
            Expr::Hole(_loc, _name) => false,
        }
    }

    pub fn rebase(mut self, current_path: Path) -> Expr {
        // TODO shouldn't this just take self and not &mut self?
        let func = |e: &mut Expr| {
            match &e {
                Expr::Reference(loc, path) => *e = Expr::Reference(loc.clone(), current_path.join(path.clone())),
                Expr::Net(_loc, _netid) => panic!("rebase() only works on symbolic expressions."),
                _ => (),
            }
        };
        self.with_subexprs_mut(&func);
        self
    }
}
