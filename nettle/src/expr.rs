use super::*;

#[derive(Eq, PartialEq, Clone)]
pub enum Expr {
    Reference(Path),
    Lit(Value),
    UnOp(UnOp, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Cat(Vec<Expr>),
    Idx(Box<Expr>, u64),
    IdxRange(Box<Expr>, u64, u64),
    IdxDyn(Box<Expr>, Box<Expr>),
    Hole(Option<String>),
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
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
                    BinOp::Eq => "==",
                    BinOp::Neq => "!=",
                    BinOp::Lt => "<",
                };
                write!(f, "({e1:?} {op_symbol} {e2:?})")
            },
            Expr::If(cond, e1, e2) => {
                write!(f, "if {cond:?} {{ {e1:?} }} else {{ {e2:?} }}")
            },
            Expr::Cat(es) => {
                write!(f, "cat(")?;
                for e in es {
                    write!(f, "{e:?}")?;
                }
                write!(f, ")")
            },
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
    Eq,
    Neq,
    Lt,
}

impl Expr {
    pub fn paths(&self) -> Vec<Path> {
        match self {
            Expr::Reference(path) => vec![path.clone()],
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
        self.paths().is_empty()
    }

    pub fn depends_on(&self, path: Path) -> bool {
        self.paths().contains(&path)
    }

    pub fn to_absolute(self, current_path: &Path) -> Expr {
        match self {
            Expr::Reference(path) => Expr::Reference(current_path.join(path)),
            Expr::Lit(_value) => self,
            Expr::UnOp(op, e) => Expr::UnOp(op, Box::new(e.to_absolute(current_path))),
            Expr::BinOp(op, e1, e2) => Expr::BinOp(op, Box::new(e1.to_absolute(current_path)), Box::new(e2.to_absolute(current_path))),
            Expr::If(cond, e1, e2) => Expr::If(Box::new(cond.to_absolute(current_path)), Box::new(e1.to_absolute(current_path)), Box::new(e2.to_absolute(current_path))),
            Expr::Cat(es) => Expr::Cat(es.into_iter().map(|e| e.to_absolute(current_path)).collect()),
            Expr::Idx(e, i) => Expr::Idx(Box::new(e.to_absolute(current_path)), i),
            Expr::IdxRange(e, j, i) => Expr::IdxRange(Box::new(e.to_absolute(current_path)), j, i),
            Expr::IdxDyn(e, i) => Expr::IdxDyn(Box::new(e.to_absolute(current_path)), Box::new(i.to_absolute(current_path))),
            Expr::Hole(name) => Expr::Hole(name),
        }
    }

    pub fn is_absolute(self) -> bool {
        self.paths().iter().all(|path| path.is_absolute())
    }

    pub fn eval(&self, nettle: &Sim) -> Value {
        match self {
            Expr::Reference(path) => nettle.peek(path.clone()),
            Expr::Lit(value) => *value,
            Expr::UnOp(op, e) => {
                match (op, e.eval(nettle)) {
                    (UnOp::Not, Value::Bit(b)) => Value::Bit(!b),
                    _ => Value::X,
                }
            },
            Expr::BinOp(op, e1, e2) => {
                match (op, e1.eval(nettle), e2.eval(nettle)) {
                    (BinOp::Add, Value::Word(w, a),  Value::Word(_w, b)) => Value::Word(w, a.wrapping_add(b) % (1 << w)),
                    (BinOp::Sub, Value::Word(w, a),  Value::Word(_w, b)) => Value::Word(w, a.wrapping_sub(b) % (1 << w)),
                    (BinOp::And, Value::Word(w, a),  Value::Word(_w, b)) => Value::Word(w, a & b),
                    (BinOp::Or,  Value::Word(w, a),  Value::Word(_w, b)) => Value::Word(w, a | b),
                    (BinOp::Eq,  Value::Word(_w, a), Value::Word(_v, b)) => Value::Bit(a == b),
                    (BinOp::Lt,  Value::Word(_w, a), Value::Word(_v, b)) => Value::Bit(a < b),
                    (BinOp::Neq, Value::Word(_w, a), Value::Word(_v, b)) => Value::Bit(a != b),
                    (BinOp::Eq,  Value::Bit(b1),     Value::Bit(b2)) => Value::Bit(b1 == b2),
                    (BinOp::Neq, Value::Bit(b1),     Value::Bit(b2)) => Value::Bit(b1 == b2),
                    (BinOp::And, Value::Bit(b1),     Value::Bit(b2)) => Value::Bit(b1 && b2),
                    (BinOp::Or,  Value::Bit(b1),     Value::Bit(b2)) => Value::Bit(b1 || b2),
                    _ => Value::X,
                }
            },
            Expr::If(cond, e1, e2) => {
                match cond.eval(nettle) {
                    Value::Bit(true) => e1.eval(nettle),
                    Value::Bit(false) => e2.eval(nettle),
                    _ => Value::X,
                }
            },
            Expr::Cat(es) => {
                let mut cat_width: u64 = 0;
                let mut cat_val: u64 = 0;
                for v in es.iter().map(|e| e.eval(nettle)).rev() {
                    if let Value::Word(width, val) = v {
                        cat_val |= val << cat_width;
                        cat_width += width;
                    } else {
                        panic!("Can't cat on a non-Word");
                    }
                }
                Value::Word(cat_width, cat_val)
            },
            Expr::Idx(e, i) => {
                let value = e.eval(nettle);
                if let Value::Word(width, val) = value {
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
}
