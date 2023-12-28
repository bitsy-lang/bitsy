use super::*;
use crate::types::*;

impl Expr {
    pub fn typecheck(self: &Arc<Self>, type_expected: Type, ctx: Context<Path, Type>) -> Result<(), TypeError> {
        if let Some(type_actual) = self.typeinfer(ctx.clone()) {
            if type_actual == type_expected {
                return Ok(());
            } else {
                return Err(TypeError::NotExpectedType(type_expected.clone(), type_actual.clone(), self.clone()));
            }
        }

        let result = match (type_expected.clone(), &**self) {
            (_type_expected, Expr::Reference(_loc, _typ, _path)) => Err(TypeError::UndefinedReference(self.clone())),
            (Type::Word(width_expected), Expr::Word(_loc, _typ, width_actual, n)) => {
                if let Some(width_actual) = width_actual {
                    if *width_actual == width_expected {
                        Err(TypeError::Other(self.clone(), format!("Not the expected width")))
                    } else if n >> *width_actual != 0 {
                        Err(TypeError::Other(self.clone(), format!("Doesn't fit")))
                    } else {
                        Ok(())
                    }
                } else {
                    if n >> width_expected != 0 {
                        Err(TypeError::Other(self.clone(), format!("Doesn't fit")))
                    } else {
                        Ok(())
                    }
                }
            },
            (_type_expected, Expr::Enum(_loc, _typ, typedef, _name)) => {
                if type_expected == *typedef {
                    Ok(())
                } else {
                    Err(TypeError::Other(self.clone(), format!("Type Error")))
                }
            },
            (Type::Valid(_typ2), Expr::Ctor(_loc, _typ, _name, es)) => {
                // TODO
                if let Type::Valid(ref typ) = type_expected {
                    if es.len() == 1 {
                        es[0].typecheck(*typ.clone(), ctx.clone())
                    } else if es.len() > 1 {
                        Err(TypeError::Other(self.clone(), format!("Error")))
                    } else {
                        Ok(())
                    }
                } else {
                    Err(TypeError::Other(self.clone(), format!("Not a Valid<T>: {self:?} is not {type_expected:?}")))
                }
            },
            (Type::Alt(typedef), Expr::Ctor(_loc, _typ, name, es)) => {
                if let Some(typs) = typedef.alt(name) {
                    if es.len() == typs.len() {
                        for (e, typ) in es.iter().zip(typs.iter()) {
                            e.typecheck(typ.clone(), ctx.clone())?;
                        }
                        Ok(())
                    } else {
                        Err(TypeError::Other(self.clone(), format!("Invalid alt type")))
                    }
                } else {
                    Err(TypeError::Other(self.clone(), format!("Invalid alt type")))
                }
            },
            (Type::Struct(typedef), Expr::Struct(_loc, _typ, fields)) => {
                // TODO Ensure all fields exist.
                for (name, e) in fields {
                    if let Some(typ) = typedef.type_of_field(name) {
                        e.typecheck(typ, ctx.clone())?;
                    } else {
                        let typename = &typedef.name;
                        return Err(TypeError::Other(self.clone(), format!("struct type {typename} has no field {name}")))
                    }
                }
                Ok(())
            },
            (_type_expected, Expr::Let(_loc, _typ, name, e, b)) => {
                if let Some(typ) = e.typeinfer(ctx.clone()) {
                    b.typecheck(type_expected.clone(), ctx.extend(name.clone().into(), typ))
                } else {
                    Err(TypeError::Other(self.clone(), format!("Can infer type of {e:?} in let expression.")))
                }
            },
            (_type_expected, Expr::Match(loc, _typ, subject, arms)) => {
                if let Some(subject_typ) = subject.typeinfer(ctx.clone()) {
                    let invalid_arms: Vec<&MatchArm> = arms.into_iter().filter(|MatchArm(pat, _e)| !subject_typ.valid_pat(pat)).collect();
                    if invalid_arms.len() > 0 {
                        return Err(TypeError::Other(self.clone(), format!("Invalid patterns for {subject_typ:?}")));
                    }
                    // TODO check pattern linearity

                    for MatchArm(pat, e) in arms {
                        let new_ctx = subject_typ.extend_context_for_pat(ctx.clone(), pat);
                        e.typecheck(type_expected.clone(), new_ctx)?;
                    }
                } else {
                    return Err(TypeError::Other(self.clone(), format!("Match: Can't infer subject type")));
                }
                Ok(())
            },
            (_type_expected, Expr::UnOp(_loc, _typ, UnOp::Not, e)) => e.typecheck(type_expected.clone(), ctx.clone()),
            (Type::Word(1), Expr::BinOp(_loc, _typ, BinOp::Eq | BinOp::Neq | BinOp::Lt, e1, e2)) => {
                if let Some(typ1) = e1.typeinfer(ctx.clone()) {
                    e2.typecheck(typ1, ctx.clone())?;
                    Ok(())
                } else {
                    Err(TypeError::Other(self.clone(), format!("Can't infer type.")))
                }
            },
            (Type::Word(_n), Expr::BinOp(_loc, _typ, BinOp::Add | BinOp::Sub | BinOp::And | BinOp::Or | BinOp::Xor, e1, e2)) => {
                e1.typecheck(type_expected.clone(), ctx.clone())?;
                e2.typecheck(type_expected.clone(), ctx.clone())?;
                Ok(())
            },
            (Type::Word(n), Expr::BinOp(_loc, _typ, BinOp::AddCarry, e1, e2)) => {
                if let (Some(typ1), Some(typ2)) = (e1.typeinfer(ctx.clone()), e2.typeinfer(ctx.clone())) {
                    if n > 0 && typ1 == typ2 && typ1 == Type::Word(n - 1) {
                        Ok(())
                    } else {
                        Err(TypeError::Other(self.clone(), format!("Types don't match")))
                    }
                } else {
                    Err(TypeError::Other(self.clone(), format!("Can't infer type.")))
                }
            },
            (_type_expected, Expr::If(_loc, _typ, cond, e1, e2)) => {
                cond.typecheck(Type::word(1), ctx.clone())?;
                e1.typecheck(type_expected.clone(), ctx.clone())?;
                e2.typecheck(type_expected.clone(), ctx.clone())?;
                Ok(())
            },
            (_type_expected, Expr::Mux(_loc, _typ, cond, e1, e2)) => {
                cond.typecheck(Type::word(1), ctx.clone())?;
                e1.typecheck(type_expected.clone(), ctx.clone())?;
                e2.typecheck(type_expected.clone(), ctx.clone())?;
                Ok(())
            },
            (Type::Word(width_expected), Expr::Sext(_loc, _typ, e)) => {
                if let Some(type_actual) = e.typeinfer(ctx.clone()) {
                    if let Type::Word(m) = type_actual {
                        if m == 0 {
                            Err(TypeError::Other(self.clone(), format!("Can't sext a Word<0>")))
                        } else if width_expected >= m {
                            Ok(())
                        } else {
                            Err(TypeError::Other(self.clone(), format!("Can't sext a Word<{m}> to a a Word<{width_expected}>")))
                        }
                    } else {
                        Err(TypeError::Other(self.clone(), format!("Unknown?")))
                    }
                } else {
                    Err(TypeError::CantInferType(self.clone()))
                }
            },
            (Type::Word(width_expected), Expr::Zext(_loc, _typ, e)) => {
                if let Some(type_actual) = e.typeinfer(ctx.clone()) {
                    if let Type::Word(m) = type_actual {
                        if width_expected >= m {
                            Ok(())
                        } else {
                            Err(TypeError::Other(self.clone(), format!("Can't zext a Word<{m}> to a a Word<{width_expected}>")))
                        }
                    } else {
                        Err(TypeError::Other(self.clone(), format!("Unknown?")))
                    }
                } else {
                    Err(TypeError::CantInferType(self.clone()))
                }
            },
            (Type::Valid(inner_type), Expr::TryCast(_loc, _typ, e)) => {
                if let Type::Enum(typedef) = &*inner_type {
                    let w = typedef.bitwidth();
                    e.typecheck(Type::Word(w), ctx.clone())?;
                    Ok(())
                } else {
                    Err(TypeError::Other(self.clone(), format!("trycast(e) has type Valid<T> for an enum type T")))
                }
            },
            (Type::Word(n), Expr::ToWord(_loc, _typ, e)) => {
                let typ = e.typeinfer(ctx.clone()).unwrap();
                if let Type::Enum(typedef) = typ {
                    let width = typedef.bitwidth();
                    if n == width {
                        Ok(())
                    } else {
                        let name = &typedef.name;
                        Err(TypeError::Other(self.clone(), format!("enum type {name} has bitwidth {width} which cannot be cast to Word<{n}>")))
                    }
                } else {
                    unreachable!()
                }
            },
            (Type::Vec(typ, n), Expr::Vec(_loc, _typ, es)) => {
                for e in es {
                    e.typecheck(*typ.clone(), ctx.clone())?;
                }
                if es.len() != n as usize {
                    let type_actual = Type::vec(*typ.clone(), es.len().try_into().unwrap());
                    Err(TypeError::NotExpectedType(type_expected.clone(), type_actual.clone(), self.clone()))
                } else {
                    Ok(())
                }
            },
            (_type_expected, Expr::IdxField(_loc, _typ, e, field)) => {
                // TODO probably want to infer idx exprs rather than check them.
                match e.typeinfer(ctx.clone()) {
                    Some(Type::Struct(typedef)) => {
                        if let Some(type_actual) = typedef.type_of_field(field) {
                            if type_expected == type_actual {
                                Ok(())
                            } else {
                                return Err(TypeError::NotExpectedType(type_expected.clone(), type_actual.clone(), self.clone()));
                            }
                        } else {
                            Err(TypeError::Other(self.clone(), format!("No such field: {field}")))
                        }
                    },
                    Some(typ) => Err(TypeError::Other(self.clone(), format!("Expected struct type, not {typ:?}"))),
                    None => Err(TypeError::Other(self.clone(), format!("Can't infer the type of {e:?}"))),
                }
            },
            (_type_expected, Expr::Idx(_loc, _typ, e, i)) => {
                match e.typeinfer(ctx.clone()) {
                    Some(Type::Word(n)) if *i < n => Ok(()),
                    Some(Type::Word(_n)) => Err(TypeError::Other(self.clone(), format!("Index out of bounds"))),
                    Some(typ) => Err(TypeError::Other(self.clone(), format!("Can't index into type {typ:?}"))),
                    None => Err(TypeError::Other(self.clone(), format!("Can't infer the type of {e:?}"))),
                }
            },
            (_type_expected, Expr::IdxRange(_loc, _typ, e, j, i)) => {
                match e.typeinfer(ctx.clone()) {
                    Some(Type::Word(n)) if n >= *j && j >= i => Ok(()),
                    Some(Type::Word(_n)) => Err(TypeError::Other(self.clone(), format!("Index out of bounds"))),
                    Some(typ) => Err(TypeError::Other(self.clone(), format!("Can't index into type {typ:?}"))),
                    None => Err(TypeError::Other(self.clone(), format!("Can't infer the type of {e:?}"))),
                }
            },
            (_type_expected, Expr::Call(_loc, _typ, fndef, es)) => {
                // TODO
                if fndef.args.len() != es.len() {
                    let fn_name = &fndef.name;
                    let m = fndef.args.len();
                    let n = es.len();
                    Err(TypeError::Other(self.clone(), format!("{fn_name} takes {n} args, but found {m} instead")))
                } else {
                    for ((_arg_name, arg_typ), e) in fndef.args.iter().zip(es.iter()) {
                        e.typecheck(Type::clone(&arg_typ), ctx.clone())?;
                    }

                    Ok(())
                }
            },
            (_type_expected, Expr::Hole(_loc, _typ, _opt_name)) => Ok(()),
            _ => Err(TypeError::Other(self.clone(), format!("{self:?} is not the expected type {type_expected:?}"))),
        };

        if let Ok(()) = &result {
            let typ = self.type_of_cell();
            let _ = typ.set(type_expected);
        }
        result
    }

    pub fn typeinfer(self: &Arc<Self>, ctx: Context<Path, Type>) -> Option<Type> {
        let result = match &**self {
            Expr::Reference(_loc, _typ, path) => {
                let type_actual = ctx.lookup(path)?;
                Some(type_actual)
            },
            Expr::Net(_loc, _typ, _netid) => panic!("Can't typecheck a net"),
            Expr::Word(_loc, _typ, None, _n) => None,
            Expr::Word(_loc, _typ, Some(w), n) => if n >> w == 0 {
                Some(Type::word(*w))
            } else {
                None
            },
            Expr::Enum(_loc, _typ, typedef, _name) => {
                Some(typedef.clone())
            },
            Expr::Cat(_loc, _typ, es) => {
                let mut w = 0u64;
                for e in es {
                    if let Some(Type::Word(m)) = e.typeinfer(ctx.clone()) {
                        w += m;
                    } else {
                        return None;
                    }
                }
                Some(Type::word(w))
            },
            Expr::ToWord(_loc, _typ, e) => {
                match e.typeinfer(ctx.clone()) {
                    Some(Type::Enum(typedef)) => {
                        Some(Type::word(typedef.bitwidth()))
                    }
                    _ => None,
                }
            },
            Expr::Vec(_loc, _typ, _es) => None,
            Expr::VecDynIdx(_loc, _typ, e, i) => {
                // TODO
                i.typeinfer(ctx.clone());
                match e.typeinfer(ctx.clone()) {
                    Some(Type::Vec(inner_type, _len)) => {
                        Some(*inner_type)
                    },
                    _ => None,
                }

            },
            Expr::VecUpdateDynIdx(_loc, _typ, e, i, ei) => {
                // TODO
                i.typeinfer(ctx.clone());
                ei.typeinfer(ctx.clone());
                match e.typeinfer(ctx.clone()) {
                    Some(Type::Vec(inner_type, len)) => {
                        Some(Type::Vec(inner_type, len))
                    },
                    _ => None,
                }

            },
            Expr::Idx(_loc, _typ, e, i) => {
                match e.typeinfer(ctx.clone()) {
                    Some(Type::Word(n)) if *i < n => Some(Type::word(1)),
                    _ => None,
                }
            },
            Expr::IdxRange(_loc, _typ, e, j, i) => {
                match e.typeinfer(ctx.clone()) {
                    Some(Type::Word(n)) if n >= *j && *j >= *i => Some(Type::word(*j - *i)),
                    Some(Type::Word(_n)) => None,
                    Some(_typ) => None,
                    None => None,
                }
            },
            Expr::Hole(_loc, _typ, _opt_name) => None,
            _ => None,
        };

        if let Some(type_actual) = &result {
            let typ = self.type_of_cell();
            typ.set(type_actual.clone()).unwrap();
        }
        result
    }
}

impl Type {
    fn valid_pat(&self, pat: &Pat) -> bool {
        match pat {
            Pat::At(ctor, subpats) => {
                match &*self {
                    Type::Valid(inner_type) => {
                        if ctor == "Invalid" && subpats.len() == 0 {
                            true
                        } else if ctor == "Valid" && subpats.len() == 1 {
                            inner_type.valid_pat(&subpats[0])
                        } else {
                            false
                        }
                    },
                    Type::Enum(typedef) => {
                        let alts: Vec<String> = typedef.values.iter().map(|(name, _val)| name.clone()).collect();
                        alts.contains(ctor)
                    },
                    Type::Alt(typedef) => {
                        let alts: Vec<String> = typedef.alts.iter().map(|(name, _val)| name.clone()).collect();
                        alts.contains(ctor)
                    },
                    _ => false,
                }
            },
            Pat::Bind(_x) => true,
            Pat::Otherwise => true,
        }
    }

    fn extend_context_for_pat(&self, ctx: Context<Path, Type>, pat: &Pat) -> Context<Path, Type> {
        match (self, pat) {
            (_, Pat::Bind(x)) => ctx.extend(x.clone().into(), self.clone()),
            (_, Pat::Otherwise) => ctx.clone(),
            (Type::Valid(inner_type), Pat::At(ctor, subpats)) => {
                if ctor == "Valid" && subpats.len() == 1 {
                    inner_type.extend_context_for_pat(ctx.clone(), &subpats[0])
                } else if ctor == "Invalid" {
                    ctx.clone()
                } else {
                    unreachable!()
                }
            },
            (Type::Enum(_typedef), Pat::At(_ctor, _subpats)) => {
                ctx.clone()
            },
            (Type::Alt(typedef), Pat::At(ctor, subpats)) => {
                if let Some(typs) = typedef.alt(ctor) {
                    let mut new_ctx = ctx.clone();
                    assert_eq!(subpats.len(), typs.len());
                    for (subpat, typ) in subpats.iter().zip(typs.iter()) {
                        new_ctx = typ.extend_context_for_pat(new_ctx.clone(), subpat)
                    }
                    new_ctx
                } else {
                    unreachable!()
                }
            },
            _ => unreachable!(),
        }
    }
}
