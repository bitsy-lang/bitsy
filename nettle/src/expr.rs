use super::*;

#[derive(Eq, PartialEq, Clone)]
pub enum Expr {
    Reference(Path),
    Lit(Value),
    UnOp(UnOp, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
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
            Expr::Hole(_name) => vec![],
        }
    }

    pub fn is_constant(&self) -> bool {
        self.paths().is_empty()
    }

    pub fn depends_on(&self, path: Path) -> bool {
        self.paths().contains(&path)
    }

    pub fn relative_to(self, top: &Path) -> Expr {
        match self {
            Expr::Reference(path) => Expr::Reference(relative_to(top, &path)),
            Expr::Lit(_value) => self,
            Expr::UnOp(op, e) => Expr::UnOp(op, Box::new(e.relative_to(top))),
            Expr::BinOp(op, e1, e2) => Expr::BinOp(op, Box::new(e1.relative_to(top)), Box::new(e2.relative_to(top))),
            Expr::If(cond, e1, e2) => Expr::If(Box::new(cond.relative_to(top)), Box::new(e1.relative_to(top)), Box::new(e2.relative_to(top))),
            Expr::Hole(name) => Expr::Hole(name),
        }
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
            Expr::Hole(opt_name) => {
                match opt_name {
                    Some(name) => panic!("EVALUATED A HOLE: ?{name}"),
                    None => panic!("EVALUATED A HOLE"),
                }
            },
        }
    }

    pub fn expand_regs_as_val(self, regs: &[Path]) -> Expr {
        match self {
            Expr::Reference(path) => {
                if regs.contains(&path) {
                    Expr::Reference(format!("{path}.val").into())
                } else {
                    Expr::Reference(path)
                }
            },
            Expr::Lit(_value) => self,
            Expr::UnOp(op, e) => Expr::UnOp(op, Box::new(e.expand_regs_as_val(regs))),
            Expr::BinOp(op, e1, e2) => Expr::BinOp(op, Box::new(e1.expand_regs_as_val(regs)), Box::new(e2.expand_regs_as_val(regs))),
            Expr::If(cond, e1, e2) => Expr::If(Box::new(cond.expand_regs_as_val(regs)), Box::new(e1.expand_regs_as_val(regs)), Box::new(e2.expand_regs_as_val(regs))),
            Expr::Hole(name) => Expr::Hole(name),
        }
    }
}
