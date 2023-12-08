use super::*;
use crate::types::*;

impl Expr {
    #[allow(unused_variables)] // TODO remove this
    pub fn typecheck(&self, type_expected: &Type, ctx: Context<Path, Type>) -> Result<(), TypeError> {
        if let Some(type_actual) = self.typeinfer(ctx.clone()) {
            if &type_actual == type_expected {
                return Ok(());
            } else {
                return Err(TypeError::NotExpectedType(type_expected.clone(), type_actual.clone(), self.clone()));
            }
        }

        match self {
            Expr::Reference(_loc, path) => Err(TypeError::UndefinedReference(self.clone())),
            Expr::Net(_loc, netid) => panic!("Can't typecheck a net"),
            Expr::Lit(_loc, Value::Word(w, n)) if n >> w != 0 =>
                Err(TypeError::InvalidLit(self.clone())),
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
            Expr::Mux(_loc, cond, e1, e2) => {
                cond.typecheck(&Type::Word(1), ctx.clone())?;
                e1.typecheck(type_expected, ctx.clone())?;
                e2.typecheck(type_expected, ctx.clone())?;
                Ok(())
            },
            Expr::Cat(_loc, _es) => Err(TypeError::CantInferType(self.clone())),
            Expr::Sext(_loc, e, n) => {
                if let Some(Type::Word(m)) = e.typeinfer(ctx.clone()) {
                    if *n >= m {
                        Ok(())
                    } else {
                        Err(TypeError::Other(self.clone(), format!(" Can't sext a Word<{m}> to a a Word<{n}>")))
                    }
                } else {
                    Err(TypeError::CantInferType(self.clone()))
                }
            },
            Expr::ToWord(_loc, e) => todo!(),
            Expr::Vec(_loc, es) => {
                if let Type::Vec(typ, n) = type_expected {
                    for e in es {
                        e.typecheck(typ, ctx.clone())?;
                    }
                    if es.len() != *n as usize {
                        let type_actual = Type::Vec(typ.clone(), es.len().try_into().unwrap());
                        Err(TypeError::NotExpectedType(type_expected.clone(), type_actual.clone(), self.clone()))
                    } else {
                        Ok(())
                    }
                } else {
                    Err(TypeError::Other(self.clone(), format!("Expected {type_expected:?} but found a Vec.")))
                }
            },
            Expr::Idx(_loc, e, i) => {
                match e.typeinfer(ctx.clone()) {
                    Some(Type::Word(n)) if *i < n => Ok(()),
                    Some(Type::Word(n)) => Err(TypeError::Other(self.clone(), format!("Index out of bounds"))),
                    Some(typ) => Err(TypeError::Other(self.clone(), format!("Can't index into type {typ:?}"))),
                    None => Err(TypeError::Other(self.clone(), format!("Can't infer the type of {e:?}"))),
                }
            },
            Expr::IdxRange(_loc, e, j, i) => {
                match e.typeinfer(ctx.clone()) {
                    Some(Type::Word(n)) if n >= *j && j >= i => Ok(()),
                    Some(Type::Word(_n)) => Err(TypeError::Other(self.clone(), format!("Index out of bounds"))),
                    Some(typ) => Err(TypeError::Other(self.clone(), format!("Can't index into type {typ:?}"))),
                    None => Err(TypeError::Other(self.clone(), format!("Can't infer the type of {e:?}"))),
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
            Expr::BinOp(_loc, BinOp::AddCarry, e1, e2) => {
                let typ1 = e1.typeinfer(ctx.clone())?;
                let typ2 = e2.typeinfer(ctx.clone())?;
                if typ1 == typ2 {
                    match typ1 {
                        Type::Word(w) => Some(Type::Word(w + 1)),
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
            Expr::Mux(_loc, cond, e1, e2) => {
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
            Expr::ToWord(loc, e) => {
                match e.typeinfer(ctx.clone()) {
                    Some(Type::TypeDef(typedef)) => {
                        if let Some(typedef) = typedef.get() {
                            Some(Type::Word(typedef.width()))
                        } else {
                            panic!("Unresolved typedef: {:?} location: {loc:?}", typedef.name())
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
    NotExpectedType(Type, Type, Expr),
    InvalidLit(Expr),
    CantInferType(Expr),
    Other(Expr, String),
}

impl HasLoc for TypeError {
    fn loc(&self) -> Loc {
        match self {
            TypeError::UndefinedReference(e) => e.loc(),
            TypeError::NotExpectedType(_type_expected, _type_actual , e) => e.loc(),
            TypeError::InvalidLit(e) => e.loc(),
            TypeError::CantInferType(e) => e.loc(),
            TypeError::Other(e, _msg) => e.loc(),
        }
    }
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            TypeError::UndefinedReference(expr) => write!(f, "Undefiend reference: {expr:?}"),
            TypeError::NotExpectedType(type_expected, type_actual, expr) => write!(f, "Not expected type: {expr:?} has type {type_actual:?} but expected {type_expected:?}."),
            TypeError::InvalidLit(expr) => write!(f, "Invalid literal: {expr:?}"),
            TypeError::CantInferType(expr) => write!(f, "Can't infer type: {expr:?}"),
            TypeError::Other(_expr, _string) => write!(f, "{self:?}"),
        }
    }
}
