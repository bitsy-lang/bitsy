use super::*;
use std::collections::BTreeMap;

use anyhow::anyhow;

#[derive(Clone)]
pub enum Expr {
    Reference(Loc, Path),
    Net(Loc, NetId),
    Lit(Loc, Value),
    UnOp(Loc, UnOp, Box<Expr>),
    BinOp(Loc, BinOp, Box<Expr>, Box<Expr>),
    If(Loc, Box<Expr>, Box<Expr>, Box<Expr>),
    Cat(Loc, Vec<Expr>),
    Sext(Loc, Box<Expr>, u64),
    ToWord(Loc, Box<Expr>),
    Vec(Loc, Vec<Expr>),
    Idx(Loc, Box<Expr>, u64),
    IdxRange(Loc, Box<Expr>, u64, u64),
    IdxDyn(Loc, Box<Expr>, Box<Expr>),
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
                    BinOp::Sub => "-",
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
    Sub,
    And,
    Or,
    Xor,
    Eq,
    Neq,
    Lt,
}

impl Expr {
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

    pub fn references_to_nets(mut self, net_id_by_path: &BTreeMap<Path, NetId>) -> Expr {
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

    pub fn eval(&self, nettle: &Sim) -> Value {
        match self {
            Expr::Reference(_loc, path) => nettle.peek(path.clone()),
            Expr::Net(_loc, netid) => nettle.peek_net(*netid),
            Expr::Lit(_loc, value) => value.clone(),
            Expr::UnOp(_loc, op, e) => {
                match (op, e.eval(nettle)) {
                    (UnOp::Not, Value::Word(n, v)) => Value::Word(n, (!v) & ((1 << n) - 1)),
                    _ => Value::X,
                }
            },
            Expr::BinOp(_loc, op, e1, e2) => {
                match (op, e1.eval(nettle), e2.eval(nettle)) {
                    (BinOp::Add, Value::X, _other) => Value::X,
                    (BinOp::Add, _other, Value::X) => Value::X,
                    (BinOp::Add, Value::Word(w, a),  Value::Word(_w, b)) => Value::Word(w, a.wrapping_add(b) % (1 << w)),
                    (BinOp::Sub, Value::Word(w, a),  Value::Word(_w, b)) => Value::Word(w, a.wrapping_sub(b) % (1 << w)),
                    (BinOp::And, Value::Word(w, a),  Value::Word(_w, b)) => Value::Word(w, a & b),
                    (BinOp::Or,  Value::Word(w, a),  Value::Word(_w, b)) => Value::Word(w, a | b),
                    (BinOp::Eq,  Value::Word(_w, a), Value::Word(_v, b)) => (a == b).into(),
                    (BinOp::Eq,  Value::Enum(_typedef, a), Value::Enum(_typedef2, b)) => (a == b).into(),
                    (BinOp::Lt,  Value::Word(_w, a), Value::Word(_v, b)) => (a < b).into(),
                    (BinOp::Neq, Value::Word(_w, a), Value::Word(_v, b)) => (a != b).into(),
                    (BinOp::Xor, Value::Word(n, a),  Value::Word(_m, b)) => Value::Word(n, a ^ b),
                    _ => Value::X,
                }
            },
            Expr::If(_loc, cond, e1, e2) => {
                let cond_v = cond.eval(nettle);
                let v1 = e1.eval(nettle);
                let v2 = e2.eval(nettle);
                if cond_v.is_x() || v1.is_x() || v2.is_x() {
                    return Value::X;
                }
                match cond_v {
                    Value::Word(1, 1) => v1,
                    Value::Word(1, 0) => v2,
                    _ => Value::X,
                }
            },
            Expr::Cat(_loc, es) => {
                let mut cat_width: u64 = 0;
                let mut cat_val: u64 = 0;
                let mut wss: Vec<Value> = vec![];
                for v in es.iter().map(|e| e.eval(nettle)).rev() {
                    if let Value::X = v {
                        return Value::X;
                    } else if let Value::Word(width, val) = v {
                        cat_val |= val << cat_width;
                        cat_width += width;
                    } else if let Value::Vec(ws) = v {
                        wss.extend(ws.into_iter().rev());
                    } else {
                        panic!("Can't cat on a non-Word");
                    }
                }
                if wss.len() == 0 {
                    Value::Word(cat_width, cat_val)
                } else {
                    Value::Vec(wss.into_iter().rev().collect())
                }
            },
            Expr::Sext(_loc, e, n) => {
                match e.eval(nettle) {
                    Value::X => Value::X,
                    Value::Word(0, _x) => panic!("Can't sext a Word<0>"),
                    Value::Word(w, x) => {
                        if w <= *n {
                            let is_negative = x & (1 << (w - 1)) > 0;
                            if is_negative {
                                let flips = ((1 << (n - w)) - 1) << w;
                                Value::Word(*n, flips | x)
                            } else {
                                Value::Word(*n, x)
                            }
                        } else {
                            panic!("Can't sext a Word<{w}> to Word<{n}> because {w} > {n}.")
                        }
                    },
                    Value::Vec(_vs) => panic!("Can't sext a Vec"),
                    Value::Enum(typedef, _name) => panic!("Can't sext a {}", typedef.name()),
                }
            }
            Expr::ToWord(_loc, e) => {
                let v = e.eval(nettle);
                match v {
                    Value::X => Value::X,
                    Value::Enum(typedef, name) => typedef.get().unwrap().value_of(&name).unwrap(),
                    _ => panic!("Can only call word() on enum values, but found {v:?}"),
                }
            },
            Expr::Vec(_loc, es) => {
                let mut vs = vec![];
                for v in es.iter().map(|e| e.eval(nettle)) {
                    if let Value::X = v {
                        return Value::X;
                    } else {
                        vs.push(v.clone());
                    }
                }
                Value::Vec(vs)
            },
            Expr::Idx(_loc, e, i) => {
                let value = e.eval(nettle);
                if let Value::X = value {
                    Value::X
                } else if let Value::Word(width, val) = value {
                    if *i < width {
                        Value::Word(1, (val >> i) & 1)
                    } else {
                        panic!("Index at {i} out of range (width {width})")
                    }
                } else if let Value::Vec(vs) = value {
                    if *i < vs.len().try_into().unwrap() {
                        vs[*i as usize].clone()
                    } else {
                        panic!("Index at {i} out of range (length {})", vs.len())
                    }
                } else {
                        panic!("Index with invalid value: {value:?}")
                }
            },
            Expr::IdxRange(_loc, e, j, i) => {
                let value = e.eval(nettle);
                if let Value::X = value {
                    Value::X
                } else if let Value::Word(width, val) = value {
                    // TODO make errors better
                    if width >= *j && *j >= *i {
                        let new_width = j - i;
                        // eg, if new_width = 3, shift over 3 to get 0b1000
                        // then subtract 1 to get 0b01111
                        let mask = (1 << new_width) - 1;
                        Value::Word(new_width, (val >> i) & mask)
                    } else {
                        panic!("Index {j}..{i} out of range (width {width})")
                    }
                } else if let Value::Vec(vs) = value {
                    let width = vs.len();
                    if j <= i && *i <= width as u64 {
                        Value::Vec(vs[*j as usize..*i as usize].to_vec())
                    } else {
                        panic!("Index {j}..{i} out of range (length {width})")
                    }
                } else {
                    panic!("Can't index into value: {value:?}")
                }
            },
            Expr::IdxDyn(_loc, e, i) => {
                let index = if let Value::Word(_width, val) = i.eval(nettle) {
                    val
                } else {
                    panic!("Invalid index: {i:?}");
                };

                let value = e.eval(nettle);
                if let Value::Word(width, val) = value {
                    if index < val {
                        Value::Word(1, (val >> index) & 1)
                    } else {
                        panic!("Index at {index} out of range (width {width})")
                    }
                } else if let Value::Vec(vs) = value {
                    if index < vs.len().try_into().unwrap() {
                        vs[index as usize].clone()
                    } else {
                        panic!("Index at {index} out of range (length {})", vs.len())
                    }
                } else {
                    panic!("Index with invalid value: {value:?}")
                }
            },
            Expr::Hole(_loc, opt_name) => {
                match opt_name {
                    Some(name) => panic!("EVALUATED A HOLE: ?{name}"),
                    None => panic!("EVALUATED A HOLE"),
                }
            },
        }
    }

    #[allow(unused_variables)] // TODO remove this
    pub fn typecheck(&self, type_expected: &Type, ctx: Context<Path, Type>) -> anyhow::Result<()> {
        if let Some(type_actual) = self.typeinfer(ctx.clone()) {
            if type_actual == *type_expected {
                return Ok(());
            } else {
                //return Err(anyhow!("{self:?} has type {type_actual:?} but expected {type_expected:?}"));
                return Err(anyhow!(TypeError::NotExpectedType(type_expected.clone(), self.clone())));
            }
        }

        match self {
            Expr::Reference(_loc, path) => Err(anyhow!(TypeError::UndefinedReference(self.clone()))),
            Expr::Net(_loc, netid) => panic!("Can't typecheck a net"),
            Expr::Lit(_loc, Value::Word(w, n)) if n >> w != 0 =>
                Err(anyhow!(TypeError::InvalidLit(self.clone()))),
            Expr::Lit(_loc, _) => unreachable!(),
            Expr::UnOp(_loc, _op, e) => e.typecheck(type_expected, ctx.clone()),
            Expr::BinOp(_loc, _op, e1, e2) => {
                e1.typecheck(type_expected, ctx.clone())?;
                e2.typecheck(type_expected, ctx.clone())?;
                Ok(())
            },
            Expr::If(_loc, cond, e1, e2) => {
                cond.typecheck(&Type::Word(1), ctx.clone())?;
                e1.typecheck(type_expected, ctx.clone())?;
                e2.typecheck(type_expected, ctx.clone())?;
                Ok(())
            },
            Expr::Cat(_loc, _es) => unreachable!(),
            Expr::Sext(_loc, e, n) => {
                if let Some(Type::Word(m)) = e.typeinfer(ctx.clone()) {
                    if *n >= m {
                        Ok(())
                    } else {
                        Err(anyhow!(TypeError::Other(self.clone(), format!(" Can't sext a Word<{m}> to a a Word<{n}>"))))
                    }
                } else {
                    Err(anyhow!(TypeError::CantInferType(self.clone())))
                }
            },
            Expr::ToWord(_loc, e) => todo!(),
            Expr::Vec(_loc, es) => {
                if let Type::Vec(typ, n) = type_expected {
                    for e in es {
                        e.typecheck(typ, ctx.clone())?;
                    }
                    if es.len() != *n as usize {
                        Err(anyhow!(TypeError::NotExpectedType(type_expected.clone(), self.clone())))
                    } else {
                        Ok(())
                    }
                } else {
                    Err(anyhow!(TypeError::NotExpectedType(type_expected.clone(), self.clone())))
                }
            },
            Expr::Idx(_loc, e, i) => {
                match e.typeinfer(ctx.clone()) {
                    Some(Type::Word(n)) if *i < n => Ok(()),
                    Some(Type::Word(n)) => Err(anyhow!(TypeError::Other(self.clone(), format!("Index out of bounds")))),
                    Some(typ) => Err(anyhow!(TypeError::Other(self.clone(), format!("Can't index into type {typ:?}")))),
                    None => Err(anyhow!(TypeError::Other(self.clone(), format!("Can't infer the type of {e:?}")))),
                }
            },
            Expr::IdxRange(_loc, e, j, i) => {
                match e.typeinfer(ctx.clone()) {
                    Some(Type::Word(n)) if n >= *j && j >= i => Ok(()),
                    Some(Type::Word(_n)) => Err(anyhow!(TypeError::Other(self.clone(), format!("Index out of bounds")))),
                    Some(typ) => Err(anyhow!(TypeError::Other(self.clone(), format!("Can't index into type {typ:?}")))),
                    None => Err(anyhow!(TypeError::Other(self.clone(), format!("Can't infer the type of {e:?}")))),
                }
            },
            Expr::IdxDyn(_loc, e, i) => todo!(),
            Expr::Hole(_loc, opt_name) => Ok(()),
        }
    }

    #[allow(unused_variables)] // TODO remove this
    pub fn typeinfer(&self, ctx: Context<Path, Type>) -> Option<Type> {
        match self {
            Expr::Reference(_loc, path) => ctx.lookup(path),
            Expr::Net(_loc, netid) => panic!("Can't typecheck a net"),
            Expr::Lit(_loc, value) => match value {
                Value::Word(w, n) if n >> w == 0 => Some(Type::Word(*w)),
                Value::Word(_w, _n) => None,
                Value::Vec(_es) => None,
                Value::Enum(typedef, _name) => Some(Type::TypeDef(typedef.clone())),
                Value::X => None,
            },
            Expr::UnOp(_loc, _op, e) => e.typeinfer(ctx.clone()),
            Expr::BinOp(_loc, op, e1, e2) => {
                let typ1 = e1.typeinfer(ctx.clone())?;
                let typ2 = e2.typeinfer(ctx.clone())?;
                if typ1 == typ2 {
                    if *op == BinOp::Eq || *op == BinOp::Neq || *op == BinOp::Lt {
                        Some(Type::Word(1))
                    } else {
                        Some(typ1)
                    }
                } else {
                    None
                }
            },
            Expr::If(_loc, cond, e1, e2) => {
                cond.typecheck(&Type::Word(1), ctx.clone()).ok()?;
                let typ1 = e1.typeinfer(ctx.clone())?;
                let typ2 = e2.typeinfer(ctx.clone())?;
                if typ1 == typ2 {
                    Some(typ1)
                } else {
                    None
                }
            },
            Expr::Cat(_loc, es) => {
                let mut w = 0u64;
                for e in es {
                    if let Some(Type::Word(m)) = e.typeinfer(ctx.clone()) {
                        w += m;
                    } else {
                        return None;
                    }
                }
                Some(Type::Word(w))
            },
            Expr::Sext(_loc, e, n) => None,
            Expr::ToWord(_loc, e) => {
                match e.typeinfer(ctx.clone()) {
                    Some(Type::TypeDef(typedef)) => {
                        if let Some(typedef) = typedef.get() {
                            Some(Type::Word(typedef.width()))
                        } else {
                            panic!("Unresolved typedef: {typedef:?}")
                        }
                    }
                    _ => None,
                }
            },
            Expr::Vec(_loc, es) => None,
            Expr::Idx(_loc, e, i) => {
                match e.typeinfer(ctx.clone()) {
                    Some(Type::Word(n)) if *i < n => Some(Type::Word(1)),
                    _ => None,
                }
            },
            Expr::IdxRange(_loc, e, j, i) => {
                match e.typeinfer(ctx.clone()) {
                    Some(Type::Word(n)) if n >= *j && j >= i => Some(Type::Word(j - i)),
                    Some(Type::Word(n)) => None,
                    Some(typ) => None,
                    None => None,
                }
            },
            Expr::IdxDyn(_loc, e, i) => None,
            Expr::Hole(_loc, opt_name) => None,
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypeError {
    UndefinedReference(Expr),
    NotExpectedType(Type, Expr),
    InvalidLit(Expr),
    CantInferType(Expr),
    Other(Expr, String),
}

impl HasLoc for TypeError {
    fn loc(&self) -> Loc {
        match self {
            TypeError::UndefinedReference(e) => e.loc(),
            TypeError::NotExpectedType(_typ, e) => e.loc(),
            TypeError::InvalidLit(e) => e.loc(),
            TypeError::CantInferType(e) => e.loc(),
            TypeError::Other(e, _msg) => e.loc(),
        }
    }
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "[{}] {self:?}", self.loc().start())
    }
}
