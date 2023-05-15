use std::sync::Arc;
use crate::context::Context;

use crate::{Bitsy, Expr, Type, Value, MatchArm, MatchPattern, TypeNode, Component, Port, Pin};
use crate::defs::{EnumAlt, StructField, ExprNode};

impl Context<Type> {
    pub fn check_type(&self, expr: Expr, typ: Type) -> bool {
        match expr.as_node() {
            ExprNode::Var(x) => self.check_type_var(x, typ),
            ExprNode::Lit(value) => self.check_type_lit(value.clone(), typ),
            ExprNode::Field(subject, field) => self.check_type_field(subject.clone(), &field, typ),
            ExprNode::Let(x, def, ascription, body) => self.check_type_let(x, def.clone(), ascription.clone(), body.clone(), typ),
            ExprNode::Add(op0, op1) => self.check_type_add(op0.clone(), op1.clone(), typ),
            ExprNode::Mul(op0, op1) => todo!(),
            ExprNode::Eq(op0, op1) => self.infer_type(expr.clone()) == Some(Type::bit()),
            ExprNode::Neq(op0, op1) => self.infer_type(expr.clone()) == Some(Type::bit()),
            ExprNode::Match(subject, arms) => self.check_type_match(subject.clone(), arms, typ),
            ExprNode::Tuple(es) => self.check_type_tuple(es, typ),
            ExprNode::Struct(fs) => self.check_type_struct(fs, typ),
            ExprNode::Enum(ctor_name, payload) => self.check_type_enum(ctor_name, payload.clone(), typ.clone()),
            ExprNode::Slice(subject, index) => todo!(),
            ExprNode::SliceConst(subject, index) => todo!(),
        }
    }

    pub fn infer_type(&self, expr: Expr) -> Option<Type> {
        let result = match expr.as_node() {
            ExprNode::Var(x) => self.infer_type_var(x),
            ExprNode::Lit(value) => self.infer_type_lit(value),
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
        };
        result
    }

    fn infer_type_var(&self, x: &str) -> Option<Type> {
        Some(self.lookup(&x).expect(&format!("No such variable: {x}")))
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
                if self.check_type(def.clone(), def_type1.clone()) {
                    let new_context = self.extend(x.to_string(), def_type1.clone());
                    new_context.infer_type(body)
                } else {
                    None
                }
            },
            (None, None) => None,
        }
    }

    fn check_type_var(&self, x: &str, typ: Type) -> bool {
        if let Some(type0) = self.lookup(x) {
            typ == type0
        } else {
            panic!("No such variable: {x}")
        }
    }

    fn check_type_let(&self, x: &str, def: Expr, ascription: Option<Type>, body: Expr, typ: Type) -> bool {
        match ascription {
            None => {
                if let Some(def_type) = self.infer_type(def) {
                    let new_context = self.extend(x.to_string(), def_type);
                    new_context.check_type(body, typ)
                } else {
                    false
                }
            },
            Some(def_type) => {
                if !self.check_type(def, def_type.clone()) {
                    false
                } else {
                    let new_context = self.extend(x.to_string(), def_type.clone());
                    new_context.check_type(body, typ)
                }
            },
        }
    }

    fn check_type_add(&self, op0: Expr, op1: Expr, typ: Type) -> bool {
        if let Some(n) = typ.as_word(self) {
            if n > 0 {
                if self.check_type(op0.clone(), Type::word(n - 1)) {
                    for i in 0..n {
                        if self.check_type(op1.clone(), Type::word(i)) {
                            return true
                        }
                    }
                } else if self.check_type(op1.clone(), Type::word(n - 1)) {
                    for i in 0..n {
                        if self.check_type(op0.clone(), Type::word(i)) {
                            return true
                        }
                    }
                }
            }
            false
        } else {
            false
        }
    }

    fn check_type_match(&self, subject: Expr, arms: &[MatchArm], typ: Type) -> bool {
        if let Some(subject_type) = &self.infer_type(subject) {
            let enum_alts = if let Some(enum_alts) = subject_type.enum_alts() { enum_alts } else { return false };

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
                                            panic!("asdf");
                                            return false;
                                        }
                                        if let MatchPattern::Var(x0) = &*pats[0] {
                                            self.extend(x0.to_string(), payload_type.clone())
                                        } else {
                                            panic!("qwer");
                                            return false
                                        }
                                    }
                                },
                                None => self.clone(),
                            };


                            if !new_context.check_type(expr0.clone(), typ.clone()) {
                                return false;
                            }
                        } else {
                            return false;
                        }
                    },
                    MatchPattern::Var(x) => {
                        let new_context = self.extend(x.to_string(), subject_type.clone());
                        if !new_context.check_type(expr0.clone(), typ.clone()) {
                            return false;
                        }
                    },
                    MatchPattern::Lit(v) => {
                        if !self.check_type(Expr::lit(v.clone()), subject_type.clone()) {
                            return false;
                        }
                        if !self.check_type(expr0.clone(), typ.clone()) {
                            return false;
                        }
                    },
                    MatchPattern::Otherwise => {
                        if !self.check_type(expr0.clone(), typ.clone()) {
                            return false;
                        }
                    },
                }
            }
            true
        } else {
            false
        }
    }

    fn check_type_tuple(&self, es: &[Expr], typ: Type) -> bool {
        if let Some(params) = typ.as_tuple() {
            if es.len() != params.len() {
                return false;
            }

            for (e, s) in es.iter().zip(params) {
                if !self.check_type(e.clone(), s) {
                    return false;
                }
            }
            true
        } else {
            false
        }
    }

    fn check_type_struct(&self, fs: &[(String, Expr)], typ: Type) -> bool {
        if let Some(struct_fields) = typ.as_struct() {
            let mut new_context: Context<Type> = self.extend_from(&typ.params().unwrap());
            if fs.len() != struct_fields.len() {
                return false;
            }
            let mut fs_sorted = fs.to_vec();
            fs_sorted.sort_by_key(|(field_name, _val)| field_name.to_string());
            let mut struct_fields_sorted = struct_fields.clone();
            struct_fields_sorted.sort_by_key(|StructField(field_name, _type)| field_name.clone());

            for ((field_name0, e), StructField(field_name1, s)) in fs_sorted.iter().zip(&struct_fields_sorted) {
                if field_name0 != field_name1 || !new_context.check_type(e.clone(), s.clone()) {
                    return false;
                }
            }
            true
        } else {
            false
        }
    }

    fn check_type_enum(&self, ctor_name: &str, payload: Option<Expr>, typ: Type) -> bool {
        if let Some(alts) = typ.enum_alts() {
            if let Some(alt) = typ.enum_alt(ctor_name) {
                match (payload, &alt.payload()) {
                    (None, None) => true,
                    (Some(payload_val), Some(payload_type)) => self.check_type(payload_val, payload_type.clone()),
                    _ => false
                }
            } else {
                false
            }
        } else {
            false
        }
    }

    fn check_type_lit(&self, value: Value, typ: Type) -> bool {
        match value {
            Value::Bit(_b) => {
                typ == Type::bit()
            },
            Value::Word(v) => {
                if let Some(n) = typ.as_word(self) {
                    v < (1 << n)
                } else {
                    false
                }
            },
            Value::Tuple(vs) => {
                if let Some(ss) = typ.as_tuple() {
                    if vs.len() != ss.len() {
                        return false;
                    } else {
                        for (v, s) in vs.iter().zip(ss) {
                            if !self.check_type_lit(*v.clone(), s) {
                                return false;
                            }
                        }
                        true
                    }
                } else {
                    false
                }
            }
            Value::Struct(fs) => {
                if let Some(ffs) = typ.as_struct() {
                    for ((field_name, field_val), StructField(type_field_name, type_field_type)) in fs.iter().zip(ffs.clone()) {
                        if field_name != &type_field_name {
                            return false;
                        } else if !self.check_type_lit(*field_val.clone(), type_field_type.clone()) {
                            return false;
                        }

                    }
                    true
                } else {
                    false
                }
            },
            _ => false,
        }
    }

    fn check_type_field(&self, subject: Expr, field: &str, typ: Type) -> bool {
        let subject_type: Type = self.infer_type(subject).expect("Can't infer type");
        if let Some(component) = subject_type.as_ref() {
            match component {
                Component::Port(Port(_name, pins)) => {
                    for Pin(name, direction, pin_typ) in pins {
                        if name == field {
                            return pin_typ == &typ
                        }
                    }
                    false
                },
                _ => todo!(),
            }
        } else {
            false
        }
    }
}

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
        let typ = context.infer_type(expr.clone());

        assert!(context.check_type(expr, typ.unwrap()));
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
        let expr: Expr = bitsy.expr(&parser.parse("let x : Bit = true; x").unwrap());
        let typ = Context::empty().infer_type(expr.clone());
        assert!(context.check_type(expr, typ.unwrap()));

        let parser = ExprParser::new();
        let expr: Expr = bitsy.expr(&parser.parse("let x = true; x").unwrap());
        let typ = Context::empty().infer_type(expr.clone());
        let bit_shape: Type = Type::bit();
        assert!(context.check_type(expr.clone(), bit_shape));

        let parser = ExprParser::new();
        let expr: Expr = bitsy.expr(&parser.parse("let x : Word<8> = 255; x").unwrap());
        let typ = context.infer_type(expr.clone());
        assert!(context.check_type(expr, typ.unwrap()));

        let parser = ExprParser::new();
        let expr: Expr = bitsy.expr(&parser.parse("let x : Word<0> = 0; x").unwrap());
        let typ = context.infer_type(expr.clone());
        assert!(context.check_type(expr, typ.unwrap()));
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
        let expr: Expr = bitsy.expr(&parser.parse("let x : Bit = 0; x").unwrap());
        let typ = context.infer_type(expr.clone());
        assert_eq!(typ, None);
        assert!(!context.check_type(expr, bit_shape));

        let parser = ExprParser::new();
        let expr: Expr = bitsy.expr(&parser.parse("let x = 0; x").unwrap());
        let typ = context.infer_type(expr.clone());
        assert_eq!(typ, None);

        let parser = ExprParser::new();
        let expr: Expr = bitsy.expr(&parser.parse("let x : Word<8> = 256; x").unwrap());
        assert_eq!(typ, None);
        assert!(!context.check_type(expr, word_8_shape));
    }
}
