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
            Expr::Word(_loc, typ, _width, value) => {
                if let Type::Word(width) = typ.get().unwrap() {
                    Value::Word(*width, *value)
                } else {
                    unreachable!()
                }
            },
            Expr::Enum(_loc, _typ, typedef, name) => Value::Enum(typedef.clone(), name.clone()),
            Expr::Ctor(_loc, _typ, name, es) => {
                let values: Vec<Value> = es.iter().map(|e| e.eval_with_ctx(bitsy, ctx.clone())).collect();
                Value::Ctor(name.to_string(), values)
            },
            Expr::Struct(_loc, typ, fields) => {
                // TODO canonicalize ordering
                let mut field_values = vec![];
                for (name, e) in fields {
                    let value = e.eval_with_ctx(bitsy, ctx.clone());
                    field_values.push((name.to_string(), value));
                }
                Value::Struct(typ.get().unwrap().clone(), field_values)
            },
            Expr::Let(_loc, _typ, name, _ascription, e, b) => {
                let v = e.eval_with_ctx(bitsy, ctx.clone());
                b.eval_with_ctx(bitsy, ctx.extend(name.clone().into(), v))
            },
            Expr::UnOp(_loc, _typ, op, e) => {
                match (op, e.eval_with_ctx(bitsy, ctx.clone())) {
                    (UnOp::Not, Value::Word(n, v)) => Value::Word(n, (!v) & ((1 << n) - 1)),
                    _ => Value::X,
                }
            },
            Expr::BinOp(_loc, _typ, op, e1, e2) => {
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
            Expr::If(_loc, _typ, cond, e1, e2) => {
                let cond_v = cond.eval_with_ctx(bitsy, ctx.clone());
                if cond_v.is_x() {
                    return Value::X;
                }
                match cond_v {
                    Value::Word(1, 1) => e1.eval_with_ctx(bitsy, ctx.clone()),
                    Value::Word(1, 0) => e2.eval_with_ctx(bitsy, ctx.clone()),
                    _ => Value::X,
                }
            },
            Expr::Match(loc, _typ, subject, arms) => {
                let subject_value = subject.eval_with_ctx(bitsy, ctx.clone());
                if subject_value.is_x() {
                    return Value::X;
                }

                for MatchArm(pat, e) in arms {
                    if let Some(new_ctx) = pat.bind(&subject_value, ctx.clone()) {
                        let e_value = e.eval_with_ctx(bitsy, new_ctx);
                        return e_value;
                    }
                }
                panic!("No match arm matched: {loc:?}")
            },
            Expr::Mux(_loc, _typ, cond, e1, e2) => {
                let cond_v = cond.eval_with_ctx(bitsy, ctx.clone());
                if cond_v.is_x() {
                    return Value::X;
                }
                match cond_v {
                    Value::Word(1, 1) => e1.eval_with_ctx(bitsy, ctx.clone()),
                    Value::Word(1, 0) => e2.eval_with_ctx(bitsy, ctx.clone()),
                    _ => Value::X,
                }
            },
            Expr::Cat(loc, _typ, es) => {
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
                        panic!("Can't cat on a non-Word {loc:?}");
                    }
                }
                if wss.len() == 0 {
                    Value::Word(cat_width, cat_val)
                } else {
                    Value::Vec(wss.into_iter().rev().collect())
                }
            },
            Expr::Sext(loc, typ, e) => {
                let n = if let Type::Word(n) = typ.get().unwrap() {
                    n
                } else {
                    unreachable!()
                };
                match e.eval_with_ctx(bitsy, ctx.clone()) {
                    Value::X => Value::X,
                    Value::Word(0, _x) => panic!("Can't sext a Word[0] {loc:?}"),
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
                            panic!("Can't sext a Word[{w}] to Word[{n}] because {w} > {n}. {loc:?}")
                        }
                    },
                    Value::Vec(_vs) => panic!("Can't sext a Vec {loc:?}"),
                    Value::Enum(typedef, _name) => panic!("Can't sext a {} {loc:?}", typedef.name()),
                    _ => panic!("Can't sext {self:?} {loc:?}"),
                }
            },
            Expr::Zext(loc, typ, e) => {
                let n = if let Type::Word(n) = typ.get().unwrap() {
                    n
                } else {
                    unreachable!()
                };
                match e.eval_with_ctx(bitsy, ctx.clone()) {
                    Value::X => Value::X,
                    Value::Word(w, x) => {
                        if w <= *n {
                            Value::Word(*n, x)
                        } else {
                            panic!("Can't sext a Word[{w}] to Word[{n}] because {w} > {n}. {loc:?}")
                        }
                    },
                    Value::Vec(_vs) => panic!("Can't sext a Vec {loc:?}"),
                    Value::Enum(typedef, _name) => panic!("Can't sext a {} {loc:?}", typedef.name()),
                    _ => panic!("Can't sext {self:?} {loc:?}"),
                }
            },
            Expr::TryCast(_loc, typ, e) => {
                let typ = typ.get().unwrap();
                let typedef = if let Type::Valid(inner_type) = typ {
                    if let Type::Enum(typedef) = &**inner_type {
                        typedef
                    } else {
                        unreachable!()
                    }
                } else {
                    unreachable!()
                };
                let value = e.eval_with_ctx(bitsy, ctx.clone());
                if let Value::X = value {
                    return Value::X;
                }
                let value = value.to_u64().unwrap();
                for (name, WordLit(_w, v)) in &typedef.values {
                    if value == *v {
                        return Value::Ctor("Valid".to_string(), vec![Value::Enum(Type::Enum(typedef.clone()), name.clone())]);
                    }
                }
                return Value::Ctor("Invalid".to_string(), vec![]);
            },
            Expr::ToWord(loc, _typ, e) => {
                let v = e.eval_with_ctx(bitsy, ctx.clone());
                match &v {
                    Value::X => Value::X,
                    Value::Enum(typ, name) => {
                        if let Type::Enum(typ) = typ {
                            Value::Word(typ.bitwidth(), typ.value_of(&name).expect(&format!("{loc:?}")))
                        } else {
                            panic!("{loc:?}: Can't call word() on {v:?}");
                        }
                    },
                    _ => panic!("Can only call word() on enum values, but found {v:?} {loc:?}"),
                }
            },
            Expr::Vec(_loc, _typ, es) => {
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
            Expr::IdxField(_loc, _typ, e, field) => {
                let value = e.eval_with_ctx(bitsy, ctx.clone());
                if let Value::X = value {
                    Value::X
                } else if let Value::Struct(_typ, fields) = e.eval_with_ctx(bitsy, ctx.clone()) {
                    for (fieldname, fieldval) in fields {
                        if *field == fieldname {
                            return fieldval;
                        }
                    }
                    panic!();
                } else {
                    panic!();
                }
            },
            Expr::Idx(_loc, _typ, e, i) => {
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
            Expr::IdxRange(_loc, _typ, e, j, i) => {
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
            Expr::Call(_loc, _typ, fndef, es) => {
                assert_eq!(fndef.args.len(), es.len());
                let mut new_ctx = ctx.clone();

                for ((arg_name, _arg_typ), e) in fndef.args.iter().zip(es.iter()) {
                    let v = e.eval_with_ctx(bitsy, ctx.clone());
                    new_ctx = new_ctx.extend(arg_name.clone().into(), v);
                }

                fndef.body.eval_with_ctx(bitsy, new_ctx)
            },
            Expr::Hole(loc, _typ, opt_name) => {
                let name = opt_name.clone().unwrap_or_default();
                eprintln!("{loc} EVALUATED A HOLE: ?{name}: {ctx}");
                Value::X
            },
        }
    }
}

impl Pat {
    fn bind(&self, v: &Value, ctx: Context<Path, Value>) -> Option<Context<Path, Value>> {
        match self {
            Pat::At(ctor, pats) => {
                match v {
                    Value::X => None,
                    Value::Word(_w, _n) => None,
                    Value::Vec(_vs) => None,
                    Value::Ctor(v_ctor, vs) => {
                        if ctor == v_ctor {
                            assert_eq!(pats.len(), vs.len());
                            let mut ctx_result = ctx.clone();
                            for (pat, v) in pats.iter().zip(vs.iter()) {
                                if let Some(ctx) = pat.bind(v, ctx_result.clone()) {
                                    ctx_result = ctx;
                                } else {
                                    return None;
                                }
                            }
                            Some(ctx_result)
                        } else {
                            None
                        }
                    },
                    Value::Enum(_typ, val) => {
                        assert_eq!(pats.len(), 0);
                        if ctor == val {
                            Some(ctx)
                        } else {
                            None
                        }
                    },
                    Value::Struct(_typ, _fields) => None,
                }
            },
            Pat::Bind(x) => Some(ctx.extend(x.clone().into(), v.clone())),
            Pat::Otherwise => Some(ctx),
        }
    }
}
