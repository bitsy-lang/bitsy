use super::*;
use crate::types::*;

impl Expr {
    #[allow(unused_variables)] // TODO remove this
    pub fn typecheck(self: &Arc<Self>, type_expected: Arc<Type>, ctx: Context<Path, Arc<Type>>) -> Result<(), TypeError> {
        if let Some(type_actual) = self.typeinfer(ctx.clone()) {
            if type_actual == type_expected {
                return Ok(());
            } else {
                return Err(TypeError::NotExpectedType(type_expected.clone(), type_actual.clone(), self.clone()));
            }
        }

        let result = match (&*type_expected.clone(), &**self) {
            (_type_expected, Expr::Reference(_loc, _typ, path)) => Err(TypeError::UndefinedReference(self.clone())),
            (_type_expected, Expr::Net(_loc, _typ, netid)) => panic!("Can't typecheck a net"),
            (Type::Word(width_expected), Expr::Word(_loc, typ, width_actual, n)) => {
                if *width_actual == *width_expected {
                    Err(TypeError::Other(self.clone(), format!("Not the expected width")))
                } else if n >> *width_actual != 0 {
                    Err(TypeError::Other(self.clone(), format!("Doesn't fit")))
                } else {
                    Ok(())
                }
            },
            (_type_expected, Expr::Enum(_loc, typedef, _name)) => {
                if let Type::TypeDef(typedef_expected) = &*type_expected {
                    if typedef_expected == typedef {
                        Ok(())
                    } else {
                        Err(TypeError::Other(self.clone(), format!("Type Error")))
                    }
                } else {
                    Err(TypeError::Other(self.clone(), format!("Type Error")))
                }
            },
            (_type_expected, Expr::Ctor(loc, name, es)) => {
                // TODO
                if let Type::Valid(typ) = &*type_expected {
                    if es.len() == 1 {
                        es[0].typecheck(typ.clone(), ctx.clone())
                    } else if es.len() > 1 {
                        Err(TypeError::Other(self.clone(), format!("Error")))
                    } else {
                        Ok(())
                    }
                } else {
                    Err(TypeError::Other(self.clone(), format!("Not a Valid<T>: {self:?} is not {type_expected:?}")))
                }
            },
            (_type_expected, Expr::Let(_loc, name, e, b)) => {
                if let Some(typ) = e.typeinfer(ctx.clone()) {
                    b.typecheck(type_expected.clone(), ctx.extend(name.clone().into(), typ))
                } else {
                    Err(TypeError::Other(self.clone(), format!("Can infer type of {e:?} in let expression.")))
                }
            },
            (_type_expected, Expr::Match(_loc, _e, arms)) => Err(TypeError::Other(self.clone(), format!("match expressions are not yet implemented"))),
            (_type_expected, Expr::UnOp(_loc, _op, e)) => e.typecheck(type_expected.clone(), ctx.clone()),
            (_type_expected, Expr::BinOp(_loc, _op, e1, e2)) => {
                e1.typecheck(type_expected.clone(), ctx.clone())?;
                e2.typecheck(type_expected.clone(), ctx.clone())?;
                Ok(())
            },
            (_type_expected, Expr::If(_loc, cond, e1, e2)) => {
                cond.typecheck(Type::word(1), ctx.clone())?;
                e1.typecheck(type_expected.clone(), ctx.clone())?;
                e2.typecheck(type_expected.clone(), ctx.clone())?;
                Ok(())
            },
            (_type_expected, Expr::Mux(_loc, cond, e1, e2)) => {
                cond.typecheck(Type::word(1), ctx.clone())?;
                e1.typecheck(type_expected.clone(), ctx.clone())?;
                e2.typecheck(type_expected.clone(), ctx.clone())?;
                Ok(())
            },
            (_type_expected, Expr::Cat(_loc, _es)) => Err(TypeError::CantInferType(self.clone())),
            (_type_expected, Expr::Sext(_loc, e, n)) => {
                if let Some(type_actual) = e.typeinfer(ctx.clone()) {
                    if let Type::Word(m) = &*type_actual {
                        if n >= m {
                            Ok(())
                        } else {
                            Err(TypeError::Other(self.clone(), format!(" Can't sext a Word<{m}> to a a Word<{n}>")))
                        }
                    } else {
                        Err(TypeError::CantInferType(self.clone()))
                    }
                } else {
                    Err(TypeError::CantInferType(self.clone()))
                }
            },
            (_type_expected, Expr::ToWord(_loc, e)) => todo!(),
            (_type_expected, Expr::Vec(_loc, es)) => {
                if let Type::Vec(typ, n) = &*type_expected {
                    for e in es {
                        e.typecheck(typ.clone(), ctx.clone())?;
                    }
                    if es.len() != *n as usize {
                        let type_actual = Type::vec(typ.clone(), es.len().try_into().unwrap());
                        Err(TypeError::NotExpectedType(type_expected.clone(), type_actual.clone(), self.clone()))
                    } else {
                        Ok(())
                    }
                } else {
                    Err(TypeError::Other(self.clone(), format!("Expected {type_expected:?} but found a Vec.")))
                }
            },
            (_type_expected, Expr::Idx(_loc, e, i)) => {
                match e.typeinfer(ctx.clone()).as_ref().map(|arc| &**arc) {
                    Some(Type::Word(n)) if i < n => Ok(()),
                    Some(Type::Word(n)) => Err(TypeError::Other(self.clone(), format!("Index out of bounds"))),
                    Some(typ) => Err(TypeError::Other(self.clone(), format!("Can't index into type {typ:?}"))),
                    None => Err(TypeError::Other(self.clone(), format!("Can't infer the type of {e:?}"))),
                }
            },
            (_type_expected, Expr::IdxRange(_loc, e, j, i)) => {
                match e.typeinfer(ctx.clone()).as_ref().map(|arc| &**arc) {
                    Some(Type::Word(n)) if n >= j && j >= i => Ok(()),
                    Some(Type::Word(_n)) => Err(TypeError::Other(self.clone(), format!("Index out of bounds"))),
                    Some(typ) => Err(TypeError::Other(self.clone(), format!("Can't index into type {typ:?}"))),
                    None => Err(TypeError::Other(self.clone(), format!("Can't infer the type of {e:?}"))),
                }
            },
            (_type_expected, Expr::IdxDyn(_loc, e, i)) => todo!(),
            (_type_expected, Expr::Hole(_loc, opt_name)) => Ok(()),
            _ => Err(TypeError::Other(self.clone(), format!("{self:?} is not the expected type {type_expected:?}"))),
        };

        if let Some(typ) = self.type_of() {
            if let Ok(()) = &result {
                typ.set(type_expected).unwrap();
            }
        }
        result
    }

    #[allow(unused_variables)] // TODO remove this
    pub fn typeinfer(self: &Arc<Self>, ctx: Context<Path, Arc<Type>>) -> Option<Arc<Type>> {
        match &**self {
            Expr::Reference(_loc, typ, path) => {
                let type_actual = ctx.lookup(path)?;
                Some(type_actual)
            },
            Expr::Net(_loc, _typ, netid) => panic!("Can't typecheck a net"),
            Expr::Word(_loc, _typ, w, n) => if n >> w == 0 {
                Some(Type::word(*w))
            } else {
                None
            },
            Expr::Enum(_loc, typedef, _name) => Some(Arc::new(Type::TypeDef(typedef.clone()))),
            Expr::UnOp(_loc, _op, e) => e.typeinfer(ctx.clone()),
            Expr::BinOp(_loc, BinOp::AddCarry, e1, e2) => {
                let typ1 = e1.typeinfer(ctx.clone())?;
                let typ2 = e2.typeinfer(ctx.clone())?;
                if typ1 == typ2 {
                    match &*typ1 {
                        Type::Word(w) => Some(Type::word(w + 1)),
                        _ => None,
                    }
                } else {
                    None
                }
            },
            Expr::BinOp(_loc, op, e1, e2) => {
                let typ1 = e1.typeinfer(ctx.clone())?;
                let typ2 = e2.typeinfer(ctx.clone())?;
                if typ1 == typ2 {
                    if *op == BinOp::Eq || *op == BinOp::Neq || *op == BinOp::Lt {
                        Some(Type::word(1))
                    } else {
                        Some(typ1)
                    }
                } else {
                    None
                }
            },
            Expr::If(_loc, cond, e1, e2) => {
                cond.typecheck(Type::word(1), ctx.clone()).ok()?;
                let typ1 = e1.typeinfer(ctx.clone())?;
                let typ2 = e2.typeinfer(ctx.clone())?;
                if typ1 == typ2 {
                    Some(typ1)
                } else {
                    None
                }
            },
            Expr::Mux(_loc, cond, e1, e2) => {
                cond.typecheck(Type::word(1), ctx.clone()).ok()?;
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
                    if let Some(Type::Word(m)) = e.typeinfer(ctx.clone()).as_ref().map(|arc| &**arc) {
                        w += m;
                    } else {
                        return None;
                    }
                }
                Some(Type::word(w))
            },
            Expr::Sext(_loc, e, n) => None,
            Expr::ToWord(loc, e) => {
                match e.typeinfer(ctx.clone()).as_ref().map(|arc| &**arc) {
                    Some(Type::TypeDef(typedef)) => {
                        if let Some(typedef) = typedef.get() {
                            Some(Type::word(typedef.width()))
                        } else {
                            panic!("Unresolved typedef: {:?} location: {loc:?}", typedef.name())
                        }
                    }
                    _ => None,
                }
            },
            Expr::Vec(_loc, es) => None,
            Expr::Idx(_loc, e, i) => {
                match e.typeinfer(ctx.clone()).as_ref().map(|arc| &**arc) {
                    Some(Type::Word(n)) if i < n => Some(Type::word(1)),
                    _ => None,
                }
            },
            Expr::IdxRange(_loc, e, j, i) => {
                match e.typeinfer(ctx.clone()).as_ref().map(|arc| &**arc) {
                    Some(Type::Word(n)) if n >= j && j >= i => Some(Type::word(*j - *i)),
                    Some(Type::Word(n)) => None,
                    Some(typ) => None,
                    None => None,
                }
            },
            Expr::IdxDyn(_loc, e, i) => None,
            Expr::Hole(_loc, opt_name) => None,
            _ => None,
        }
    }
}
