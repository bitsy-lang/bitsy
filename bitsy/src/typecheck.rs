use super::*;
use crate::types::*;
use std::sync::Arc;

impl Expr {
    pub fn typecheck(self: &Arc<Self>, type_expected: Type, ctx: Context<Path, Type>) -> Result<(), TypeError> {
        if let Some(type_actual) = self.typeinfer(ctx.clone()) {
            if type_actual.equals(&type_expected) {
                return Ok(());
            } else {
                return Err(TypeError::NotExpectedType(type_expected.clone(), type_actual.clone(), self.clone()));
            }
        }

        let result = match (type_expected.clone(), &**self) {
            (_type_expected, Expr::Reference(_span, _typ, _path)) => Err(TypeError::UndefinedReference(self.clone())),
            (Type::Word(width_expected), Expr::Word(_span, _typ, width_actual, n)) => {
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
            (_type_expected, Expr::Enum(_span, _typ, typedef, _name)) => {
                if type_expected.equals(typedef) {
                    Ok(())
                } else {
                    Err(TypeError::Other(self.clone(), format!("Type Error")))
                }
            },
            (Type::Valid(_typ2), Expr::Ctor(_span, _typ, _name, es)) => {
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
            (Type::Alt(typedef, params), Expr::Ctor(_span, _typ, name, es)) => {
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
            (Type::Struct(typedef), Expr::Struct(_span, _typ, fields)) => {
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
            (_type_expected, Expr::Let(_span, _typ, name, ascription, e, b)) => {
                if let Some(typ) = ascription {
                    e.typecheck(typ.clone(), ctx.clone())?;
                    b.typecheck(type_expected.clone(), ctx.extend(name.clone().into(), typ.clone()))
                } else if let Some(typ) = e.typeinfer(ctx.clone()) {
                    b.typecheck(type_expected.clone(), ctx.extend(name.clone().into(), typ))
                } else {
                    Err(TypeError::Other(self.clone(), format!("Can infer type of {e:?} in let expression.")))
                }
            },
            (_type_expected, Expr::Match(_span, _typ, subject, arms)) => {
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
            (_type_expected, Expr::UnOp(_span, _typ, UnOp::Not, e)) => e.typecheck(type_expected.clone(), ctx.clone()),
            (Type::Word(1), Expr::BinOp(_span, _typ, BinOp::Eq | BinOp::Neq | BinOp::Lt, e1, e2)) => {
                if let Some(typ1) = e1.typeinfer(ctx.clone()) {
                    e2.typecheck(typ1, ctx.clone())?;
                    Ok(())
                } else {
                    Err(TypeError::Other(self.clone(), format!("Can't infer type.")))
                }
            },
            (Type::Word(_n), Expr::BinOp(_span, _typ, BinOp::Add | BinOp::Sub | BinOp::And | BinOp::Or | BinOp::Xor, e1, e2)) => {
                e1.typecheck(type_expected.clone(), ctx.clone())?;
                e2.typecheck(type_expected.clone(), ctx.clone())?;
                Ok(())
            },
            (Type::Word(n), Expr::BinOp(_span, _typ, BinOp::AddCarry, e1, e2)) => {
                if let (Some(typ1), Some(typ2)) = (e1.typeinfer(ctx.clone()), e2.typeinfer(ctx.clone())) {
                    if n > 0 && typ1.equals(&typ2) && typ1.equals(&Type::Word(n - 1)) {
                        Ok(())
                    } else {
                        Err(TypeError::Other(self.clone(), format!("Types don't match")))
                    }
                } else {
                    Err(TypeError::Other(self.clone(), format!("Can't infer type.")))
                }
            },
            (_type_expected, Expr::If(_span, _typ, cond, e1, e2)) => {
                cond.typecheck(Type::word(1), ctx.clone())?;
                e1.typecheck(type_expected.clone(), ctx.clone())?;
                e2.typecheck(type_expected.clone(), ctx.clone())?;
                Ok(())
            },
            (_type_expected, Expr::Mux(_span, _typ, cond, e1, e2)) => {
                cond.typecheck(Type::word(1), ctx.clone())?;
                e1.typecheck(type_expected.clone(), ctx.clone())?;
                e2.typecheck(type_expected.clone(), ctx.clone())?;
                Ok(())
            },
            (Type::Word(width_expected), Expr::Sext(_span, _typ, e)) => {
                if let Some(type_actual) = e.typeinfer(ctx.clone()) {
                    if let Type::Word(m) = type_actual {
                        if m == 0 {
                            Err(TypeError::Other(self.clone(), format!("Can't sext a Word[0]")))
                        } else if width_expected >= m {
                            Ok(())
                        } else {
                            Err(TypeError::Other(self.clone(), format!("Can't sext a Word[{m}] to a a Word[{width_expected}]")))
                        }
                    } else {
                        Err(TypeError::Other(self.clone(), format!("Unknown?")))
                    }
                } else {
                    Err(TypeError::CantInferType(self.clone()))
                }
            },
            (Type::Word(width_expected), Expr::Zext(_span, _typ, e)) => {
                if let Some(type_actual) = e.typeinfer(ctx.clone()) {
                    if let Type::Word(m) = type_actual {
                        if width_expected >= m {
                            Ok(())
                        } else {
                            Err(TypeError::Other(self.clone(), format!("Can't zext a Word[{m}] to a a Word[{width_expected}]")))
                        }
                    } else {
                        Err(TypeError::Other(self.clone(), format!("Unknown?")))
                    }
                } else {
                    Err(TypeError::CantInferType(self.clone()))
                }
            },
            (Type::Valid(inner_type), Expr::TryCast(_span, _typ, e)) => {
                if let Type::Enum(typedef) = &*inner_type {
                    let w = typedef.bitwidth();
                    e.typecheck(Type::Word(w), ctx.clone())?;
                    Ok(())
                } else {
                    Err(TypeError::Other(self.clone(), format!("trycast(e) has type Valid<T> for an enum type T")))
                }
            },
            (Type::Word(n), Expr::ToWord(_span, _typ, e)) => {
                let typ = e.typeinfer(ctx.clone()).unwrap();
                if let Type::Enum(typedef) = typ {
                    let width = typedef.bitwidth();
                    if n == width {
                        Ok(())
                    } else {
                        let name = &typedef.name;
                        Err(TypeError::Other(self.clone(), format!("enum type {name} has bitwidth {width} which cannot be cast to Word[{n}]")))
                    }
                } else {
                    unreachable!()
                }
            },
            (Type::Vec(typ, n), Expr::Vec(_span, _typ, es)) => {
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
            (_type_expected, Expr::Call(_span, _typ, fndef, es)) => {
                // TODO
                if fndef.args.len() != es.len() {
                    let fn_name = &fndef.name;
                    let m = fndef.args.len();
                    let n = es.len();
                    Err(TypeError::Other(self.clone(), format!("{fn_name} takes {n} args, but found {m} instead")))
                } else {
                    let mut errors = vec![];
                    for ((arg_name, arg_typ), e) in fndef.args.iter().zip(es.iter()) {
                        if let Err(error) = e.typecheck(Type::clone(&arg_typ), ctx.clone()) {
                            errors.push(error);
                        }
                    }

                    if errors.len() > 0 {
                        return Err(errors[0].clone());
                    }

                    Ok(())
                }
            },
            (_type_expected, Expr::Hole(_span, _typ, _opt_name)) => Ok(()),
            _ => Err(TypeError::Other(self.clone(), format!("{self:?} is not the expected type {type_expected:?}"))),
        };

        if let Ok(()) = &result {
            self.annotate_type(type_expected.clone());
        }
        result
    }

    pub fn typeinfer(&self, ctx: Context<Path, Type>) -> Option<Type> {
        let result = match self {
            Expr::Reference(_span, _typ, path) => {
                let type_actual = ctx.lookup(path)?;
                Some(type_actual)
            },
            Expr::Net(_span, _typ, _netid) => panic!("Can't typecheck a net"),
            Expr::Word(_span, _typ, None, _n) => None,
            Expr::Word(_span, _typ, Some(w), n) => if n >> w == 0 {
                // TODO the n >> w condition should be a check, not a typecheck.
                Some(Type::word(*w))
            } else {
                None
            },
            Expr::Enum(_span, _typ, typedef, _name) => {
                Some(typedef.clone())
            },
            Expr::BinOp(_span, _typ, BinOp::Eq | BinOp::Neq | BinOp::Lt, e1, e2) => {
                if let Some(typ1) = e1.typeinfer(ctx.clone()) {
                    if let Err(_) = e2.typecheck(typ1, ctx.clone()) {
                        None
                    } else {
                        Some(Type::Word(1))
                    }
                } else {
                    None
                }
            },
            Expr::Cat(_span, _typ, es) => {
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
            Expr::Call(_span, _typ, fndef, es) => {
                // TODO
                if fndef.args.len() != es.len() {
                    None
                } else {
                    let mut errors = vec![];
                    for ((arg_name, arg_typ), e) in fndef.args.iter().zip(es.iter()) {
                        if let Err(error) = e.typecheck(Type::clone(&arg_typ), ctx.clone()) {
                            errors.push(error);
                        }
                    }

                    if errors.len() > 0 {
                        return None;
                    }

                    Some(fndef.ret.clone())
                }
            },
            Expr::IdxField(_span, _typ, e, field) => {
                match e.typeinfer(ctx.clone()) {
                    Some(Type::Struct(typedef)) => {
                        if let Some(type_actual) = typedef.type_of_field(field) {
                            Some(type_actual.clone())
                        } else {
                            None
                        }
                    },
                    _ => None,
                }
            },
            Expr::Idx(_span, _typ, e, i) => {
                match e.typeinfer(ctx.clone()) {
                    Some(Type::Word(n)) if *i < n => Some(Type::word(1)),
                    _ => None,
                }
            },
            Expr::IdxRange(_span, _typ, e, j, i) => {
                match e.typeinfer(ctx.clone()) {
                    Some(Type::Word(n)) if n >= *j && *j >= *i => Some(Type::word(*j - *i)),
                    Some(Type::Word(_n)) => None,
                    Some(_typ) => None,
                    None => None,
                }
            },
            _ => None,
        };

        if let Some(type_actual) = &result {
            self.annotate_type(type_actual.clone());
        }
        result
    }

    fn annotate_type(&self, typ: Type) {
        if let Some(type_cell) = self.type_of_cell() {
            let _ = type_cell.set(typ.clone());
        }
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
                    Type::Alt(typedef, _params) => {
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
            (Type::Alt(typedef, params), Pat::At(ctor, subpats)) => {
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
