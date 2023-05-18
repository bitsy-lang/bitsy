use std::sync::Arc;
use crate::context::Context;
use log::*;

use crate::common::{Direction, BitsyResult, BitsyError, Loc};
use crate::{Bitsy, Expr, Type, Value, MatchArm, MatchPattern, TypeNode, Component, Port, Pin};
use crate::defs::{EnumAlt, StructField, ExprNode};


impl Context<Type> {
    pub fn check_type(&self, expr: Expr, typ: Type) -> BitsyResult<()> {
        let loc = expr.loc();
        match expr.as_node() {
            ExprNode::Var(x) => self.check_type_var(loc, x, typ)?,
            ExprNode::Lit(value) => self.check_type_lit(loc, value.clone(), typ)?,
            ExprNode::Field(subject, field) => self.check_type_field(loc, subject.clone(), &field, typ)?,
            ExprNode::Let(x, def, ascription, body) => self.check_type_let(loc, x, def.clone(), ascription.clone(), body.clone(), typ)?,
            ExprNode::Add(op0, op1) => self.check_type_add(loc, op0.clone(), op1.clone(), typ)?,
            ExprNode::Mul(op0, op1) => todo!(),
            ExprNode::Eq(op0, op1) => self.check_type_is(loc, expr.clone(), Type::bit())?,
            ExprNode::Neq(op0, op1) => self.check_type_is(loc, expr.clone(), Type::bit())?,
            ExprNode::Match(subject, arms) => self.check_type_match(loc, subject.clone(), arms, typ)?,
            ExprNode::If(e, t, f) => self.check_type_if(loc, e.clone(), t.clone(), f.clone(), typ)?,
            ExprNode::Tuple(es) => self.check_type_tuple(loc, es, typ)?,
            ExprNode::Struct(fs) => self.check_type_struct(loc, fs, typ)?,
            ExprNode::Enum(ctor_name, payload) => self.check_type_enum(loc, ctor_name, payload.clone(), typ.clone())?,
            ExprNode::Slice(subject, index) => todo!(),
            ExprNode::SliceConst(subject, index) => todo!(),
        }
        Ok(())
    }

    fn infer_type(&self, expr: Expr) -> Option<Type> {
        match expr.as_node() {
            ExprNode::Var(x) => Some(self.infer_type_var(x)),
            ExprNode::Lit(value) => self.infer_type_lit(value),
            ExprNode::Field(subject, field) => self.infer_type_field(subject.clone(), &field),
            ExprNode::Let(x, def, def_shape, body) => self.infer_type_let(x, def.clone(), def_shape.clone(), body.clone()),
            ExprNode::Add(op0, op1) => None,
            ExprNode::Mul(op0, op1) => None,
            ExprNode::Eq(op0, op1) => {
                match (self.infer_type(op0.clone()), self.infer_type(op1.clone())) {
                    (Some(shape0), Some(shape1)) => if shape0 == shape1 { Some(Type::bit()) } else { None },
                    _ => None,
                }
            },
            ExprNode::Neq(op0, op1) => {
                match (self.infer_type(op0.clone()), self.infer_type(op1.clone())) {
                    (Some(shape0), Some(shape1)) => if shape0 == shape1 { Some(Type::bit()) } else { None },
                    _ => None,
                }
            },
            _ => None,
        }
    }

    fn check_type_is(&self, loc: &Loc, expr: Expr, typ: Type) -> BitsyResult<()> {
        let inferred_type = self.infer_type(expr.clone());
        if inferred_type.as_ref() == Some(&typ) {
            Ok(())
        } else {
            Err(BitsyError::Type(loc.clone(), format!("{expr:?} is not expected type {typ}")))
        }
    }

    fn infer_type_var(&self, x: &str) -> Type {
        if let Some(typ) = self.lookup(&x) {
            typ
        } else {
            error!("No such variable: {x}");
            panic!("No such variable: {x}")
        }
    }

    fn infer_type_lit(&self, value: &Value) -> Option<Type> {
        match value {
            Value::Bit(_b) => Some(Type::bit()),
            Value::Word(v) => None, // because you can't infer the bitwidth
            Value::Tuple(vs) => {
                // try to infer each v in vs, if you can then good.
                let mut shapes: Vec<Type> = vec![];
                for opt_shape in vs.iter().map(|v| self.infer_type_lit(v)) {
                    if let Some(typ) = opt_shape {
                        shapes.push(typ.clone());
                    } else {
                        return None
                    }
                }
                Some(Type::tuple(shapes))
            },
            _ => None,
        }
    }

    fn infer_type_field(&self, subject: Expr, field: &str) -> Option<Type> {
        let subject_type: Option<Type> = self.infer_type(subject);
        if subject_type.is_none() {
            return None;
        }
        let subject_type: Type = subject_type.unwrap();
        if let Some(component) = subject_type.as_reference() {
            match component {
                Component::Port(Port(_name, pins)) => {
                    for Pin(name, direction, pin_typ) in pins {
                        if name == field {
                            if direction == &Direction::Incoming {
                                return Some(pin_typ.clone());
                            } else {
                                // output ports can't drive a value
                                return None;
                            }
                        }
                    }
                    None
                },
                Component::Reg(_name, _vis, reg) => {
                    Some(reg.shape.clone())
                },
                Component::Mod(_name, _vis, mod_) => todo!(),
                Component::Gate(_name, _vis, gate) => todo!(),
                Component::Const(_name, _vis, val, const_type) => todo!(),
            }
        } else if let Some(fields) = subject_type.as_struct() {
            for StructField(name, typ) in &fields {
                if name == field {
                    return Some(typ.clone());
                }
            }
            None
        } else {
            None
        }
    }

    fn infer_type_let(&self, x: &str, def: Expr, def_type: Option<Type>, body: Expr) -> Option<Type> {
        match (self.infer_type(def.clone()), def_type) {
            (Some(def_type0), Some(def_type1)) => {
                if def_type0 == def_type1 {
                    let new_context = self.extend(x.to_string(), def_type0);
                    new_context.infer_type(body)
                } else {
                    None
                }
            },
            (Some(def_type0), None) => {
                let new_context = self.extend(x.to_string(), def_type0);
                new_context.infer_type(body)
            },
            (None, Some(def_type1)) => {
                if let Ok(()) = self.check_type(def.clone(), def_type1.clone()) {
                    let new_context = self.extend(x.to_string(), def_type1.clone());
                    new_context.infer_type(body)
                } else {
                    None
                }
            },
            (None, None) => None,
        }
    }

    fn check_type_var(&self, loc: &Loc, x: &str, typ: Type) -> BitsyResult<()> {
        if let Some(type0) = self.lookup(x) {
            if typ == type0 {
                Ok(())
            } else {
                Err(BitsyError::Type(loc.clone(), format!("Expected {typ} found {type0}")))
            }
        } else {
            Err(BitsyError::Type(loc.clone(), format!("No such variable: {x}")))
        }
    }

    fn check_type_let(&self, loc: &Loc, x: &str, def: Expr, ascription: Option<Type>, body: Expr, typ: Type) -> BitsyResult<()> {
        match ascription {
            None => {
                if let Some(def_type) = self.infer_type(def) {
                    let new_context = self.extend(x.to_string(), def_type);
                    new_context.check_type(body, typ)?;
                } else {
                    return Err(BitsyError::Type(loc.clone(), format!("Could not infer type of let subject. Consider using an ascription.")));
                }
            },
            Some(def_type) => {
                self.check_type(def, def_type.clone())?;
                let new_context = self.extend(x.to_string(), def_type.clone());
                new_context.check_type(body, typ)?;
            },
        }
        Ok(())
    }

    fn check_type_add(&self, loc: &Loc, op0: Expr, op1: Expr, typ: Type) -> BitsyResult<()> {
        if let Some(n) = typ.as_word(self) {
            if n > 0 {
                if let Ok(()) = self.check_type(op0.clone(), Type::word(n - 1)) {
                    for i in 0..n {
                        if let Ok(()) = self.check_type(op1.clone(), Type::word(i)) {
                            return Ok(())
                        }
                    }
                } else if let Ok(()) = self.check_type(op1.clone(), Type::word(n - 1)) {
                    for i in 0..n {
                        if let Ok(()) = self.check_type(op0.clone(), Type::word(i)) {
                            return Ok(())
                        }
                    }
                }
            }
            Err(BitsyError::Type(loc.clone(), format!("Addition didn't typecheck")))
        } else {
            Err(BitsyError::Type(loc.clone(), format!("Addition didn't typecheck because something wasn't a {typ}")))
        }
    }

    fn check_type_match(&self, loc: &Loc, subject: Expr, arms: &[MatchArm], typ: Type) -> BitsyResult<()> {
        if let Some(subject_type) = &self.infer_type(subject) {
            let enum_alts = if let Some(enum_alts) = subject_type.enum_alts() { enum_alts } else { return Err(BitsyError::Type(loc.clone(), format!("Not an enum."))) }; // todo!()

            for MatchArm(pat, expr0) in arms {
                let mut new_context = self.clone();
                match &*pat {
                    MatchPattern::Ctor(ctor_name, pats) => {
                        if let Some(alt) = subject_type.enum_alt(ctor_name) {
                            let new_context: Context<Type> = match alt.payload() {
                                Some(payload_type) => {
                                    if pats.len() == 0 {
                                        self.clone()
                                    } else {
                                        if pats.len() != 1 {
                                            return Err(BitsyError::Type(loc.clone(), format!("pats isn't the right length")));
                                        }
                                        if let MatchPattern::Var(x0) = &*pats[0] {
                                            let payload_type0 = payload_type.substs(&subject_type.params().unwrap_or_else(|| {
                                                error!("This shouldn't happen");
                                                panic!("This shouldn't happen")
                                            }));
                                            self.extend(x0.to_string(), payload_type0)
                                        } else {
                                            return Err(BitsyError::Type(loc.clone(), format!("Pats wasn't a var or something")));
                                        }
                                    }
                                },
                                None => self.clone(),
                            };


                            new_context.check_type(expr0.clone(), typ.clone())?;
                        } else {
                            return Err(BitsyError::Type(loc.clone(), format!("I think the type wasn't an enum or something.")));
                        }
                    },
                    MatchPattern::Var(x) => {
                        let new_context = self.extend(x.to_string(), subject_type.clone());
                        new_context.check_type(expr0.clone(), typ.clone())?;
                    },
                    MatchPattern::Lit(v) => {
                        self.check_type(Expr::lit(loc.clone(), v.clone()), subject_type.clone())?;
                        self.check_type(expr0.clone(), typ.clone())?;
                    },
                    MatchPattern::Otherwise => {
                        self.check_type(expr0.clone(), typ.clone())?;
                    },
                }
            }
            Ok(())
        } else {
            Err(BitsyError::Type(loc.clone(), format!("Couldn't infer type of the subject")))
        }
    }

    fn check_type_if(&self, loc: &Loc, subject: Expr, true_branch: Expr, false_branch: Expr, typ: Type) -> BitsyResult<()> {
        self.check_type_is(loc, subject.clone(), Type::bit())?;
        self.check_type(true_branch.clone(), typ.clone())?;
        self.check_type(false_branch.clone(), typ)?;
        Ok(())
    }

    fn check_type_tuple(&self, loc: &Loc, es: &[Expr], typ: Type) -> BitsyResult<()> {
        if let Some(params) = typ.as_tuple() {
            if es.len() != params.len() {
                return Err(BitsyError::Type(loc.clone(), format!("Wrong number of arguments: {typ}")));
            }

            for (e, s) in es.iter().zip(params) {
                self.check_type(e.clone(), s)?;
            }
            Ok(())
        } else {
            Err(BitsyError::Type(loc.clone(), format!("Thing isn't a tuple")))
        }
    }

    fn check_type_struct(&self, loc: &Loc, fs: &[(String, Expr)], typ: Type) -> BitsyResult<()> {
        if let TypeNode::Family(shape, args) = typ.as_node() {
            let params = shape.clone().params();
            if params.is_none() {
                error!("params is None");
                return Err(BitsyError::Type(loc.clone(), format!("Params is none?")));
            }
            let param_names: Vec<String> = params.unwrap().into_inner().iter().map(|(name, _kind)| name.to_string()).collect();
            if let Some(struct_fields) = typ.as_struct() {
                let typ_params = typ.params();
                if typ_params.is_none() {
                    error!("typ_params is None");
                    return Err(BitsyError::Type(loc.clone(), format!("type_params is none?")));
                }
                let mut new_context: Context<Type> = self.extend_from(&typ_params.unwrap());
                if fs.len() != struct_fields.len() {
                    return Err(BitsyError::Type(loc.clone(), format!("Number of fields is wrong {typ}")));
                }
                let mut fs_sorted = fs.to_vec();
                fs_sorted.sort_by_key(|(field_name, _val)| field_name.to_string());
                let mut struct_fields_sorted = struct_fields.clone();
                struct_fields_sorted.sort_by_key(|StructField(field_name, _type)| field_name.clone());

                for ((field_name0, e), StructField(field_name1, s)) in fs_sorted.iter().zip(&struct_fields_sorted) {
                    let field_typ = s.substs(&param_names.clone().into_iter().zip(args.iter().cloned()).collect::<Vec<_>>()).clone();
                    new_context.check_type(e.clone(), field_typ)?;
                    if field_name0 != field_name1 {
                        return Err(BitsyError::Type(loc.clone(), format!("Bad field: {field_name0}")));
                    }
                }
                return Ok(());
            } else {
                return Err(BitsyError::Type(loc.clone(), format!("Not a struct")));
            }
        } else {
            return Err(BitsyError::Type(loc.clone(), format!("Not a type family?")));
        }
    }

    fn check_type_enum(&self, loc: &Loc, ctor_name: &str, payload: Option<Expr>, typ: Type) -> BitsyResult<()> {
        debug!("check_type_enum");
        if let TypeNode::Family(shape, args) = typ.as_node() {
            let param_names: Vec<String> = shape.clone().params().unwrap().into_inner().iter().map(|(name, _kind)| name.to_string()).collect();
            debug!("shape = {shape:?} args = {args:?}");
            if let Some(alts) = typ.enum_alts() {
                debug!("alts = {alts:?}");
                if let Some(alt) = typ.enum_alt(ctor_name) {
                    debug!("alt = {alt:?}");
                    return match (payload, &alt.payload()) {
                        (None, None) => Ok(()),
                        (Some(payload_val), Some(payload_type)) => self.check_type(payload_val, payload_type.substs(&param_names.into_iter().zip(args.iter().cloned()).collect::<Vec<_>>()).clone()),
                        _ => Err(BitsyError::Type(loc.clone(), format!("Bad"))),
                    };
                } else {
                    return Err(BitsyError::Type(loc.clone(), format!("Bad")));
                }
            } else {
                return Err(BitsyError::Type(loc.clone(), format!("Bad")));
            }
            Err(BitsyError::Type(loc.clone(), format!("Bad")))
        } else {
            Err(BitsyError::Type(loc.clone(), format!("Bad")))
        }
    }

    fn check_type_lit(&self, loc: &Loc, value: Value, typ: Type) -> BitsyResult<()> {
        match value {
            Value::Bit(_b) => {
                if typ == Type::bit() {
                    Ok(())
                } else {
                    Err(BitsyError::Type(loc.clone(), format!("Type of {value} is Bit not {typ}")))
                }
            },
            Value::Word(v) => {
                if let Some(n) = typ.as_word(self) {
                    if v < (1 << n) {
                        Ok(())
                    } else {
                        Err(BitsyError::Type(loc.clone(), format!("Value {v} is too large to fit into a Word<{n}>")))
                    }
                } else {
                    Err(BitsyError::Type(loc.clone(), format!("Value {v} is not a {typ}")))
                }
            },
            Value::Tuple(vs) => {
                if let Some(ss) = typ.as_tuple() {
                    if vs.len() != ss.len() {
                        return Err(BitsyError::Type(loc.clone(), format!("Tuple does not have the right number of arguments.")));
                    } else {
                        for (v, s) in vs.iter().zip(ss) {
                            self.check_type_lit(loc, *v.clone(), s)?;
                        }
                        Ok(())
                    }
                } else {
                    Err(BitsyError::Type(loc.clone(), format!("Tuple is not a {typ}")))
                }
            }
            Value::Struct(fs) => {
                if let Some(ffs) = typ.as_struct() {
                    for ((field_name, field_val), StructField(type_field_name, type_field_type)) in fs.iter().zip(ffs.clone()) {
                        if field_name != &type_field_name {
                            return Err(BitsyError::Type(loc.clone(), format!("Struct has a bad field.")));
                        }
                        self.check_type_lit(loc, *field_val.clone(), type_field_type.clone())?;
                    }
                    Ok(())
                } else {
                    Err(BitsyError::Type(loc.clone(), format!("Struct is not a {typ}")))
                }
            },
            _ => Err(BitsyError::Type(loc.clone(), format!("Something bad happened"))),
        }
    }

    fn check_type_field(&self, loc: &Loc, subject: Expr, field: &str, typ: Type) -> BitsyResult<()> {
        if let Some(typ0) = self.infer_type_field(subject, field) {
             if typ0 == typ {
                 Ok(())
             } else {
                 Err(BitsyError::Type(loc.clone(), format!("Field type is not {typ}")))
             }
        } else {
            Err(BitsyError::Unknown(loc.clone(), "subject_type is None".to_string()))
        }
    }
}
/*
#[cfg(test)]
mod test {
    use super::*;
    use crate::common::ShapeRef;
    use crate::parser::*;

    #[test]
    fn typecheck_0() {
        let mut bitsy = Bitsy::new();
        let text = "
            pub mod Top
            end
        ";

        bitsy.add(&text);
        let context = Context::empty();
        let parser = ExprParser::new();
        let expr: Expr = bitsy.expr(&parser.parse("true").unwrap());
        let typ = context.infer_type(expr.clone()).unwrap().unwrap();

        assert!(context.check_type(expr, typ).unwrap());
    }

    #[test]
    fn typecheck_1() {
        let mut bitsy = Bitsy::new();
        let text = "
            pub mod Top
            end
        ";
        bitsy.add(&text);

        let context = Context::empty();
        let parser = ExprParser::new();
        let expr: Expr = bitsy.expr(&parser.parse("let x of Bit = true; x").unwrap());
        let typ = Context::empty().infer_type(expr.clone()).unwrap().unwrap();
        assert!(context.check_type(expr, typ).unwrap());

        let parser = ExprParser::new();
        let expr: Expr = bitsy.expr(&parser.parse("let x = true; x").unwrap());
        let typ = Context::empty().infer_type(expr.clone()).unwrap();
        let bit_shape: Type = Type::bit();
        assert!(context.check_type(expr.clone(), bit_shape).unwrap());

        let parser = ExprParser::new();
        let expr: Expr = bitsy.expr(&parser.parse("let x of Word<8> = 255; x").unwrap());
        let typ = context.infer_type(expr.clone()).unwrap().unwrap();
        assert!(context.check_type(expr, typ).unwrap());

        let parser = ExprParser::new();
        let expr: Expr = bitsy.expr(&parser.parse("let x of Word<0> = 0; x").unwrap());
        let typ = context.infer_type(expr.clone()).unwrap().unwrap();
        assert!(context.check_type(expr, typ).unwrap());
    }

    #[test]
    fn typecheck_2() {
        // check things fail when they are the wrong typ

        let mut bitsy = Bitsy::new();
        let text = "
            pub mod Top
            end
        ";
        bitsy.add(&text);

        let bit_shape: Type = Type::bit();
        let word_8_shape: Type = Type::word(8);

        let context = Context::empty();
        let parser = ExprParser::new();
        let expr: Expr = bitsy.expr(&parser.parse("let x of Bit = 0; x").unwrap());
        let typ = context.infer_type(expr.clone()).unwrap();
        assert_eq!(typ, None);
        assert!(!context.check_type(expr, bit_shape).unwrap());

        let parser = ExprParser::new();
        let expr: Expr = bitsy.expr(&parser.parse("let x = 0; x").unwrap());
        let typ = context.infer_type(expr.clone()).unwrap();
        assert_eq!(typ, None);

        let parser = ExprParser::new();
        let expr: Expr = bitsy.expr(&parser.parse("let x of Word<8> = 256; x").unwrap());
        assert_eq!(typ, None);
        assert!(!context.check_type(expr, word_8_shape).unwrap());
    }
}
*/
