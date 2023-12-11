use super::*;
use crate::sim::Sim;
use crate::sim::Value;

impl Expr {
    pub fn eval(&self, bitsy: &Sim) -> Value {
        self.eval_with_ctx(bitsy, Context::empty())
    }

    fn eval_with_ctx(&self, bitsy: &Sim, ctx: Context<Path, Value>) -> Value {
        match self {
            Expr::Reference(_loc, _typ, path) => {
                if let Some(value) = ctx.lookup(path) {
                    value.clone()
                } else {
                    bitsy.peek(path.clone())
                }
            }
            Expr::Net(_loc, _typ, netid) => bitsy.peek_net(*netid),
            Expr::Word(_loc, _typ, width, value) => Value::Word(*width, *value),
            Expr::Enum(_loc, typedef, name) => Value::Enum(typedef.clone(), name.clone()),
            Expr::Ctor(_loc, name, es) => {
                let values: Vec<Value> = es.iter().map(|e| e.eval_with_ctx(bitsy, ctx.clone())).collect();
                Value::Ctor(name.to_string(), values)
            },
            Expr::Let(_loc, name, e, b) => {
                let v = e.eval_with_ctx(bitsy, ctx.clone());
                dbg!(&v);
                b.eval_with_ctx(bitsy, ctx.extend(name.clone().into(), v))
            },
            Expr::UnOp(_loc, op, e) => {
                match (op, e.eval_with_ctx(bitsy, ctx.clone())) {
                    (UnOp::Not, Value::Word(n, v)) => Value::Word(n, (!v) & ((1 << n) - 1)),
                    _ => Value::X,
                }
            },
            Expr::BinOp(_loc, op, e1, e2) => {
                match (op, e1.eval_with_ctx(bitsy, ctx.clone()), e2.eval_with_ctx(bitsy, ctx.clone())) {
                    (BinOp::Add, Value::X, _other) => Value::X,
                    (BinOp::Add, _other, Value::X) => Value::X,
                    (BinOp::Add, Value::Word(w, a),  Value::Word(_w, b)) => Value::Word(w, a.wrapping_add(b) % (1 << w)),
                    (BinOp::AddCarry, Value::Word(w, a),  Value::Word(_w, b)) => {
                        let new_w = w + 1;
                        Value::Word(new_w, a.wrapping_add(b) % (1 << new_w))
                    },
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
                let cond_v = cond.eval_with_ctx(bitsy, ctx.clone());
                let v1 = e1.eval_with_ctx(bitsy, ctx.clone());
                let v2 = e2.eval_with_ctx(bitsy, ctx.clone());
                if cond_v.is_x() || v1.is_x() || v2.is_x() {
                    return Value::X;
                }
                match cond_v {
                    Value::Word(1, 1) => v1,
                    Value::Word(1, 0) => v2,
                    _ => Value::X,
                }
            },
            Expr::Match(_loc, _e, arms) => todo!(),
            Expr::Mux(_loc, cond, e1, e2) => {
                let cond_v = cond.eval_with_ctx(bitsy, ctx.clone());
                let v1 = e1.eval_with_ctx(bitsy, ctx.clone());
                let v2 = e2.eval_with_ctx(bitsy, ctx.clone());
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
                for v in es.iter().map(|e| e.eval_with_ctx(bitsy, ctx.clone())).rev() {
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
                match e.eval_with_ctx(bitsy, ctx.clone()) {
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
                    _ => panic!("Can't sext {self:?}"),
                }
            }
            Expr::ToWord(_loc, e) => {
                let v = e.eval_with_ctx(bitsy, ctx.clone());
                match v {
                    Value::X => Value::X,
                    Value::Enum(typedef, name) => {
                        let typedef: &TypeDef = &*typedef.get().unwrap();
                        Value::Word(typedef.width(), typedef.value_of(&name).unwrap())
                    },
                    _ => panic!("Can only call word() on enum values, but found {v:?}"),
                }
            },
            Expr::Vec(_loc, es) => {
                let mut vs = vec![];
                for v in es.iter().map(|e| e.eval_with_ctx(bitsy, ctx.clone())) {
                    if let Value::X = v {
                        return Value::X;
                    } else {
                        vs.push(v.clone());
                    }
                }
                Value::Vec(vs)
            },
            Expr::Idx(_loc, e, i) => {
                let value = e.eval_with_ctx(bitsy, ctx.clone());
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
                let value = e.eval_with_ctx(bitsy, ctx.clone());
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
                let index = if let Value::Word(_width, val) = i.eval_with_ctx(bitsy, ctx.clone()) {
                    val
                } else {
                    panic!("Invalid index: {i:?}");
                };

                let value = e.eval_with_ctx(bitsy, ctx.clone());
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
}