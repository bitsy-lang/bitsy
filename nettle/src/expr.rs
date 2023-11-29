use super::*;

#[derive(Eq, PartialEq, Clone)]
pub enum Expr {
    Reference(Path),
    Net(NetId),
    Lit(Value),
    UnOp(UnOp, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Cat(Vec<Expr>),
    Sext(Box<Expr>, u64),
    ToWord(Box<Expr>),
    Idx(Box<Expr>, u64),
    IdxRange(Box<Expr>, u64, u64),
    IdxDyn(Box<Expr>, Box<Expr>),
    Hole(Option<String>),
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Expr::Net(netid) => write!(f, "#{netid:?}"),
            Expr::Reference(path) => write!(f, "{path}"),
            Expr::Lit(val) => write!(f, "{val:?}"),
            Expr::UnOp(op, e) => {
                let op_symbol = match op {
                    UnOp::Not => "!",
                };
                write!(f, "({op_symbol}{e:?})")
            },
            Expr::BinOp(op, e1, e2) => {
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
            Expr::If(cond, e1, e2) => {
                write!(f, "if {cond:?} {{ {e1:?} }} else {{ {e2:?} }}")
            },
            Expr::Cat(es) => write!(f, "cat({})", es.iter().map(|e| format!("{e:?}")).collect::<Vec<_>>().join(", ")),
            Expr::Sext(e, n) => write!(f, "sext({e:?}, {n})"),
            Expr::ToWord(e) => write!(f, "word({e:?})"),
            Expr::Idx(e, i) => write!(f, "{e:?}[{i}]"),
            Expr::IdxRange(e, j, i) => write!(f, "{e:?}[{j}..{i}]"),
            Expr::IdxDyn(e, i) => write!(f, "{e:?}[{i:?}]"),
            Expr::Hole(opt_name) => {
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
    pub fn with_subexprs_mut(&mut self, callback: &dyn Fn(&mut Expr)) {
        match self {
            Expr::Reference(_path) => callback(self),
            Expr::Net(_netid) => callback(self),
            Expr::Lit(_value) => callback(self),
            Expr::UnOp(_op, e) => {
                e.with_subexprs_mut(callback);
                callback(&mut *self);
            }
            Expr::BinOp(_op, e1, e2) => {
                e1.with_subexprs_mut(callback);
                e2.with_subexprs_mut(callback);
                callback(self);
            },
            Expr::If(cond, e1, e2) => {
                cond.with_subexprs_mut(callback);
                e1.with_subexprs_mut(callback);
                e2.with_subexprs_mut(callback);
                callback(self);
            },
            Expr::Cat(es) => {
                for e in es {
                    e.with_subexprs_mut(callback);
                }
                callback(self);
            },
            Expr::Sext(e, _n) => {
                e.with_subexprs_mut(callback);
                callback(self);
            },
            Expr::ToWord(e) => {
                e.with_subexprs_mut(callback);
                callback(self);
            },
            Expr::Idx(e, _i) => {
                e.with_subexprs_mut(callback);
                callback(self);
            },
            Expr::IdxRange(e, _j, _i) => {
                e.with_subexprs_mut(callback);
                callback(self);
            },
            Expr::IdxDyn(e, _i) => {
                e.with_subexprs_mut(callback);
                callback(self);
            },
            Expr::Hole(_name) => {
                callback(self);
            },
        }
    }

    pub fn paths(&self) -> Vec<Path> {
        match self {
            Expr::Reference(path) => vec![path.clone()],
            Expr::Net(_netid) => panic!("paths() only works on symbolic expressions."),
            Expr::Lit(_value) => vec![],
            Expr::UnOp(_op, e) => {
                let mut result = e.paths();
                result.sort();
                result.dedup();
                result
            }
            Expr::BinOp(_op, e1, e2) => {
                let mut result = e1.paths();
                result.extend(e2.paths());
                result.sort();
                result.dedup();
                result
            },
            Expr::If(cond, e1, e2) => {
                let mut result = cond.paths();
                result.extend(e1.paths());
                result.extend(e2.paths());
                result.sort();
                result.dedup();
                result
            },
            Expr::Cat(es) => {
                let mut result = vec![];
                for e in es {
                    result.extend(e.paths());
                }
                result.sort();
                result.dedup();
                result
            },
            Expr::Sext(e, _n) => e.paths(),
            Expr::ToWord(e) => e.paths(),
            Expr::Idx(e, _i) => e.paths(),
            Expr::IdxRange(e, _j, _i) => e.paths(),
            Expr::IdxDyn(e, i) => {
                let mut result = e.paths();
                result.extend(i.paths());
                result.sort();
                result.dedup();
                result
            },
            Expr::Hole(_name) => vec![],
        }
    }

    pub fn is_constant(&self) -> bool {
        match self {
            Expr::Reference(_path) => false,
            Expr::Net(_netid) => false,
            Expr::Lit(_value) => true,
            Expr::UnOp(_op, e) => e.is_constant(),
            Expr::BinOp(_op, e1, e2) => e1.is_constant() && e2.is_constant(),
            Expr::If(cond, e1, e2) => cond.is_constant() && e1.is_constant() && e2.is_constant(),
            Expr::Cat(es) => es.iter().all(|e| e.is_constant()),
            Expr::Sext(e, _n) => e.is_constant(),
            Expr::ToWord(e) => e.is_constant(),
            Expr::Idx(e, _i) => e.is_constant(),
            Expr::IdxRange(e, _j, _i) => e.is_constant(),
            Expr::IdxDyn(e, i) => e.is_constant() && i.is_constant(),
            Expr::Hole(_name) => false,
        }
    }

    pub fn depends_on(&self, path: Path) -> bool {
        self.paths().contains(&path)
    }

    pub fn depends_on_net(&self, net_id: NetId) -> bool {
        match self {
            Expr::Reference(_path) => panic!("rebase() only works on net expressions."),
            Expr::Net(other_netid) => net_id == *other_netid,
            Expr::Lit(_value) => false,
            Expr::UnOp(_op, e) => e.depends_on_net(net_id),
            Expr::BinOp(_op, e1, e2) => e1.depends_on_net(net_id) || e2.depends_on_net(net_id),
            Expr::If(cond, e1, e2) => cond.depends_on_net(net_id) || e1.depends_on_net(net_id) || e2.depends_on_net(net_id),
            Expr::Cat(es) => es.iter().any(|e| e.depends_on_net(net_id)),
            Expr::Sext(e, _n) => e.depends_on_net(net_id),
            Expr::ToWord(e) => e.depends_on_net(net_id),
            Expr::Idx(e, _i) => e.depends_on_net(net_id),
            Expr::IdxRange(e, _j, _i) => e.depends_on_net(net_id),
            Expr::IdxDyn(e, i) => e.depends_on_net(net_id) || i.depends_on_net(net_id),
            Expr::Hole(_name) => false,
        }
    }

    pub fn rebase(self, current_path: Path) -> Expr {
        match self {
            Expr::Reference(path) => Expr::Reference(current_path.join(path)),
            Expr::Net(_netid) => panic!("rebase() only works on symbolic expressions."),
            Expr::Lit(ref _value) => self,
            Expr::UnOp(op, e) => Expr::UnOp(op, Box::new(e.rebase(current_path))),
            Expr::BinOp(op, e1, e2) => Expr::BinOp(op, Box::new(e1.rebase(current_path.clone())), Box::new(e2.rebase(current_path))),
            Expr::If(cond, e1, e2) => Expr::If(Box::new(cond.rebase(current_path.clone())), Box::new(e1.rebase(current_path.clone())), Box::new(e2.rebase(current_path))),
            Expr::Cat(es) => Expr::Cat(es.into_iter().map(|e| e.rebase(current_path.clone())).collect()),
            Expr::Sext(e, n) => Expr::Sext(Box::new(e.rebase(current_path.clone())), n),
            Expr::ToWord(e) => Expr::ToWord(Box::new(e.rebase(current_path.clone()))),
            Expr::Idx(e, i) => Expr::Idx(Box::new(e.rebase(current_path)), i),
            Expr::IdxRange(e, j, i) => Expr::IdxRange(Box::new(e.rebase(current_path)), j, i),
            Expr::IdxDyn(e, i) => Expr::IdxDyn(Box::new(e.rebase(current_path.clone())), Box::new(i.rebase(current_path))),
            Expr::Hole(name) => Expr::Hole(name),
        }
    }

    pub fn eval(&self, nettle: &Sim) -> Value {
        match self {
            Expr::Reference(path) => nettle.peek(path.clone()),
            Expr::Net(netid) => nettle.peek_net(*netid),
            Expr::Lit(value) => value.clone(),
            Expr::UnOp(op, e) => {
                match (op, e.eval(nettle)) {
                    (UnOp::Not, Value::Word(n, v)) => Value::Word(n, (!v) & ((1 << n) - 1)),
                    _ => Value::X,
                }
            },
            Expr::BinOp(op, e1, e2) => {
                match (op, e1.eval(nettle), e2.eval(nettle)) {
                    (BinOp::Add, Value::Word(w, a),  Value::Word(_w, b)) => Value::Word(w, a.wrapping_add(b) % (1 << w)),
                    (BinOp::Sub, Value::Word(w, a),  Value::Word(_w, b)) => Value::Word(w, a.wrapping_sub(b) % (1 << w)),
                    (BinOp::And, Value::Word(w, a),  Value::Word(_w, b)) => Value::Word(w, a & b),
                    (BinOp::Or,  Value::Word(w, a),  Value::Word(_w, b)) => Value::Word(w, a | b),
                    (BinOp::Eq,  Value::Word(_w, a), Value::Word(_v, b)) => (a == b).into(),
                    (BinOp::Lt,  Value::Word(_w, a), Value::Word(_v, b)) => (a < b).into(),
                    (BinOp::Neq, Value::Word(_w, a), Value::Word(_v, b)) => (a != b).into(),
                    (BinOp::Xor, Value::Word(n, a),  Value::Word(_m, b)) => Value::Word(n, a ^ b),
                    _ => Value::X,
                }
            },
            Expr::If(cond, e1, e2) => {
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
            Expr::Cat(es) => {
                let mut cat_width: u64 = 0;
                let mut cat_val: u64 = 0;
                for v in es.iter().map(|e| e.eval(nettle)).rev() {
                    if let Value::X = v {
                        return Value::X;
                    } else if let Value::Word(width, val) = v {
                        cat_val |= val << cat_width;
                        cat_width += width;
                    } else {
                        panic!("Can't cat on a non-Word");
                    }
                }
                Value::Word(cat_width, cat_val)
            },
            Expr::Sext(e, n) => {
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
                    Value::Enum(typedef, _name) => panic!("Can't sext a {}", typedef.name()),
                }
            }
            Expr::ToWord(e) => {
                let v = e.eval(nettle);
                match v {
                    Value::X => Value::X,
                    Value::Enum(typedef, name) => typedef.get().unwrap().value_of(&name),
                    _ => panic!("Can only call word() on enum values, but found {v:?}"),
                }
            },
            Expr::Idx(e, i) => {
                let value = e.eval(nettle);
                if let Value::X = value {
                    Value::X
                } else if let Value::Word(width, val) = value {
                    if *i < width {
                        Value::Word(1, (val >> i) & 1)
                    } else {
                        panic!("Index at {i} out of range (width {width})")
                    }
                } else {
                        panic!("Index with invalid value: {value:?}")
                }
            },
            Expr::IdxRange(e, j, i) => {
                let value = e.eval(nettle);
                if let Value::X = value {
                    Value::X
                } else if let Value::Word(width, val) = value {
                    // TODO make errors better
                    if *i < width && *j >= *i {
                        let new_width = j - i;
                        // eg, if new_width = 3, shift over 3 to get 0b1000
                        // then subtract 1 to get 0b01111
                        let mask = (1 << new_width) - 1;
                        Value::Word(new_width, (val >> i) & mask)
                    } else {
                        panic!("Index {j}..{i} out of range (width {width})")
                    }
                } else {
                    panic!("Can't index into value: {value:?}")
                }
            },
            Expr::IdxDyn(e, i) => {
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
                } else {
                    panic!("Index with invalid value: {value:?}")
                }
            },
            Expr::Hole(opt_name) => {
                match opt_name {
                    Some(name) => panic!("EVALUATED A HOLE: ?{name}"),
                    None => panic!("EVALUATED A HOLE"),
                }
            },
        }
    }

    pub fn references_to_nets(self, net_id_by_path: &BTreeMap<Path, NetId>) -> Expr {
        match self {
            Expr::Reference(path) => {
                if let Some(net_id) = net_id_by_path.get(&path) {
                    Expr::Net(*net_id)
                } else {
                    panic!("No net for {path}")
                }
            },
            Expr::Net(_netid) => panic!("references_to_nets() only works on symbolic expressions."),
            Expr::Lit(ref _value) => self,
            Expr::UnOp(op, e) => Expr::UnOp(op, Box::new(e.references_to_nets(net_id_by_path))),
            Expr::BinOp(op, e1, e2) => Expr::BinOp(op, Box::new(e1.references_to_nets(net_id_by_path)), Box::new(e2.references_to_nets(net_id_by_path))),
            Expr::If(cond, e1, e2) => Expr::If(Box::new(cond.references_to_nets(net_id_by_path)), Box::new(e1.references_to_nets(net_id_by_path)), Box::new(e2.references_to_nets(net_id_by_path))),
            Expr::Cat(es) => Expr::Cat(es.into_iter().map(|e| e.references_to_nets(net_id_by_path)).collect()),
            Expr::Sext(e, n) => Expr::Sext(Box::new(e.references_to_nets(net_id_by_path)), n),
            Expr::ToWord(e) => Expr::ToWord(Box::new(e.references_to_nets(net_id_by_path))),
            Expr::Idx(e, i) => Expr::Idx(Box::new(e.references_to_nets(net_id_by_path)), i),
            Expr::IdxRange(e, j, i) => Expr::IdxRange(Box::new(e.references_to_nets(net_id_by_path)), j, i),
            Expr::IdxDyn(e, i) => Expr::IdxDyn(Box::new(e.references_to_nets(net_id_by_path)), Box::new(i.references_to_nets(net_id_by_path))),
            Expr::Hole(name) => Expr::Hole(name),
        }
    }
}
