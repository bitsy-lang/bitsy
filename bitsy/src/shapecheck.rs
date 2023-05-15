use std::sync::Arc;
use crate::context::Context;

use crate::{Bitsy, Expr, Type, Value, MatchArm, MatchPattern, TypeNode};
use crate::defs::{EnumAlt, StructField, ExprNode};

impl Context<Type> {
    pub fn check_shape(&self, expr: Expr, shape: Type) -> bool {
        match expr.as_node() {
            ExprNode::Var(x) => self.check_shape_var(x, shape),
            ExprNode::Lit(value) => self.check_shape_lit(value.clone(), shape),
            ExprNode::Let(x, def, ascription, body) => self.check_shape_let(x, def.clone(), ascription.clone(), body.clone(), shape),
            ExprNode::Add(op0, op1) => self.check_shape_add(op0.clone(), op1.clone(), shape),
            ExprNode::Mul(op0, op1) => todo!(),
            ExprNode::Eq(op0, op1) => self.infer_shape(expr.clone()) == Some(Type::bit()),
            ExprNode::Neq(op0, op1) => self.infer_shape(expr.clone()) == Some(Type::bit()),
            ExprNode::Match(subject, arms) => self.check_shape_match(subject.clone(), arms, shape),
            ExprNode::Tuple(es) => self.check_shape_tuple(es, shape),
            ExprNode::Struct(fs) => self.check_shape_struct(fs, shape),
            ExprNode::Enum(ctor_name, payload) => self.check_shape_enum(ctor_name, payload.clone(), shape.clone()),
            ExprNode::Slice(subject, index) => todo!(),
            ExprNode::SliceConst(subject, index) => todo!(),
        }
    }

    pub fn infer_shape(&self, expr: Expr) -> Option<Type> {
        let result = match expr.as_node() {
            ExprNode::Var(x) => self.infer_shape_var(x),
            ExprNode::Lit(value) => self.infer_shape_lit(value),
            ExprNode::Let(x, def, def_shape, body) => self.infer_shape_let(x, def.clone(), def_shape.clone(), body.clone()),
            ExprNode::Add(op0, op1) => None,
            ExprNode::Mul(op0, op1) => None,
            ExprNode::Eq(op0, op1) => {
                match (self.infer_shape(op0.clone()), self.infer_shape(op1.clone())) {
                    (Some(shape0), Some(shape1)) => if shape0 == shape1 { Some(Type::bit()) } else { None },
                    _ => None,
                }
            },
            ExprNode::Neq(op0, op1) => {
                match (self.infer_shape(op0.clone()), self.infer_shape(op1.clone())) {
                    (Some(shape0), Some(shape1)) => if shape0 == shape1 { Some(Type::bit()) } else { None },
                    _ => None,
                }
            },
            _ => None,
        };
        result
    }

    fn infer_shape_var(&self, x: &str) -> Option<Type> {
        Some(self.lookup(&x).expect(&format!("No such variable: {x}")))
    }

    fn infer_shape_lit(&self, value: &Value) -> Option<Type> {
        match value {
            Value::Bit(_b) => Some(Type::bit()),
            Value::Word(v) => None, // because you can't infer the bitwidth
            Value::Tuple(vs) => {
                // try to infer each v in vs, if you can then good.
                let mut shapes: Vec<Type> = vec![];
                for opt_shape in vs.iter().map(|v| self.infer_shape_lit(v)) {
                    if let Some(shape) = opt_shape {
                        shapes.push(shape.clone());
                    } else {
                        return None
                    }
                }
                Some(Type::tuple(shapes))
            },
            _ => None,
        }
    }

    fn infer_shape_let(&self, x: &str, def: Expr, def_shape: Option<Type>, body: Expr) -> Option<Type> {
        match (self.infer_shape(def.clone()), def_shape) {
            (Some(def_shape0), Some(def_shape1)) => {
                if def_shape0 == def_shape1 {
                    let new_context = self.extend(x.to_string(), def_shape0);
                    new_context.infer_shape(body)
                } else {
                    None
                }
            },
            (Some(def_shape0), None) => {
                let new_context = self.extend(x.to_string(), def_shape0);
                new_context.infer_shape(body)
            },
            (None, Some(def_shape1)) => {
                if self.check_shape(def.clone(), def_shape1.clone()) {
                    let new_context = self.extend(x.to_string(), def_shape1.clone());
                    new_context.infer_shape(body)
                } else {
                    None
                }
            },
            (None, None) => None,
        }
    }

    fn check_shape_var(&self, x: &str, shape: Type) -> bool {
        if let Some(shape0) = self.lookup(x) {
            shape == shape0
        } else {
            panic!("No such variable: {x}")
        }
    }

    fn check_shape_let(&self, x: &str, def: Expr, ascription: Option<Type>, body: Expr, shape: Type) -> bool {
        match ascription {
            None => {
                if let Some(def_shape) = self.infer_shape(def) {
                    let new_context = self.extend(x.to_string(), def_shape);
                    new_context.check_shape(body, shape)
                } else {
                    false
                }
            },
            Some(def_shape) => {
                if !self.check_shape(def, def_shape.clone()) {
                    false
                } else {
                    let new_context = self.extend(x.to_string(), def_shape.clone());
                    new_context.check_shape(body, shape)
                }
            },
        }
    }

    fn check_shape_add(&self, op0: Expr, op1: Expr, shape: Type) -> bool {
        if let Some(n) = shape.as_word(self) {
            if n > 0 {
                if self.check_shape(op0.clone(), Type::word(n - 1)) {
                    for i in 0..n {
                        if self.check_shape(op1.clone(), Type::word(i)) {
                            return true
                        }
                    }
                } else if self.check_shape(op1.clone(), Type::word(n - 1)) {
                    for i in 0..n {
                        if self.check_shape(op0.clone(), Type::word(i)) {
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

    fn check_shape_match(&self, subject: Expr, arms: &[MatchArm], shape: Type) -> bool {
        if let Some(subject_shape) = &self.infer_shape(subject) {
            let enum_alts = if let Some(enum_alts) = subject_shape.enum_alts() { enum_alts } else { return false };

            for MatchArm(pat, expr0) in arms {
                let mut new_context = self.clone();
                match &*pat {
                    MatchPattern::Ctor(ctor_name, pats) => {
                        if let Some(alt) = subject_shape.enum_alt(ctor_name) {
                            let new_context: Context<Type> = match alt.payload() {
                                Some(payload_shape) => {
                                    if pats.len() == 0 {
                                        self.clone()
                                    } else {
                                        if pats.len() != 1 {
                                            panic!("asdf");
                                            return false;
                                        }
                                        if let MatchPattern::Var(x0) = &*pats[0] {
                                            self.extend(x0.to_string(), payload_shape.clone())
                                        } else {
                                            panic!("qwer");
                                            return false
                                        }
                                    }
                                },
                                None => self.clone(),
                            };


                            if !new_context.check_shape(expr0.clone(), shape.clone()) {
                                return false;
                            }
                        } else {
                            return false;
                        }
                    },
                    MatchPattern::Var(x) => {
                        let new_context = self.extend(x.to_string(), subject_shape.clone());
                        if !new_context.check_shape(expr0.clone(), shape.clone()) {
                            return false;
                        }
                    },
                    MatchPattern::Lit(v) => {
                        if !self.check_shape(Expr::lit(v.clone()), subject_shape.clone()) {
                            return false;
                        }
                        if !self.check_shape(expr0.clone(), shape.clone()) {
                            return false;
                        }
                    },
                    MatchPattern::Otherwise => {
                        if !self.check_shape(expr0.clone(), shape.clone()) {
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

    fn check_shape_tuple(&self, es: &[Expr], shape: Type) -> bool {
        if let Some(params) = shape.as_tuple() {
            if es.len() != params.len() {
                return false;
            }

            for (e, s) in es.iter().zip(params) {
                if !self.check_shape(e.clone(), s) {
                    return false;
                }
            }
            true
        } else {
            false
        }
    }

    fn check_shape_struct(&self, fs: &[(String, Expr)], shape: Type) -> bool {
        if let Some(struct_fields) = shape.as_struct() {
            let mut new_context: Context<Type> = self.extend_from(&shape.params().unwrap());
            if fs.len() != struct_fields.len() {
                return false;
            }
            let mut fs_sorted = fs.to_vec();
            fs_sorted.sort_by_key(|(field_name, _val)| field_name.to_string());
            let mut struct_fields_sorted = struct_fields.clone();
            struct_fields_sorted.sort_by_key(|StructField(field_name, _shape)| field_name.clone());

            for ((field_name0, e), StructField(field_name1, s)) in fs_sorted.iter().zip(&struct_fields_sorted) {
                if field_name0 != field_name1 || !new_context.check_shape(e.clone(), s.clone()) {
                    return false;
                }
            }
            true
        } else {
            false
        }
    }

    fn check_shape_enum(&self, ctor_name: &str, payload: Option<Expr>, shape: Type) -> bool {
        if let Some(alts) = shape.enum_alts() {
            if let Some(alt) = shape.enum_alt(ctor_name) {
                match (payload, &alt.payload()) {
                    (None, None) => true,
                    (Some(payload_val), Some(payload_shape)) => self.check_shape(payload_val, payload_shape.clone()),
                    _ => false
                }
            } else {
                false
            }
        } else {
            false
        }
    }

    fn check_shape_lit(&self, value: Value, shape: Type) -> bool {
        match value {
            Value::Bit(_b) => {
                shape == Type::bit()
            },
            Value::Word(v) => {
                if let Some(n) = shape.as_word(self) {
                    v < (1 << n)
                } else {
                    false
                }
            },
            Value::Tuple(vs) => {
                if let Some(ss) = shape.as_tuple() {
                    if vs.len() != ss.len() {
                        return false;
                    } else {
                        for (v, s) in vs.iter().zip(ss) {
                            if !self.check_shape_lit(*v.clone(), s) {
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
                if let Some(ffs) = shape.as_struct() {
                    for ((field_name, field_val), StructField(shape_field_name, shape_field_shape)) in fs.iter().zip(ffs.clone()) {
                        if field_name != &shape_field_name {
                            return false;
                        } else if !self.check_shape_lit(*field_val.clone(), shape_field_shape.clone()) {
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
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::common::ShapeRef;
    use crate::parser::*;

    #[test]
    fn shapecheck_0() {
        let mut bitsy = Bitsy::new();
        let text = "
            pub mod Top
            end
        ";

        bitsy.add(&text);
        let context = Context::empty();
        let parser = ExprParser::new();
        let expr: Expr = bitsy.expr(&parser.parse("true").unwrap());
        let shape = context.infer_shape(expr.clone());

        assert!(context.check_shape(expr, shape.unwrap()));
    }

    #[test]
    fn shapecheck_1() {
        let mut bitsy = Bitsy::new();
        let text = "
            pub mod Top
            end
        ";
        bitsy.add(&text);

        let context = Context::empty();
        let parser = ExprParser::new();
        let expr: Expr = bitsy.expr(&parser.parse("let x : Bit = true; x").unwrap());
        let shape = Context::empty().infer_shape(expr.clone());
        assert!(context.check_shape(expr, shape.unwrap()));

        let parser = ExprParser::new();
        let expr: Expr = bitsy.expr(&parser.parse("let x = true; x").unwrap());
        let shape = Context::empty().infer_shape(expr.clone());
        let bit_shape: Type = Type::bit();
        assert!(context.check_shape(expr.clone(), bit_shape));

        let parser = ExprParser::new();
        let expr: Expr = bitsy.expr(&parser.parse("let x : Word<8> = 255; x").unwrap());
        let shape = context.infer_shape(expr.clone());
        assert!(context.check_shape(expr, shape.unwrap()));

        let parser = ExprParser::new();
        let expr: Expr = bitsy.expr(&parser.parse("let x : Word<0> = 0; x").unwrap());
        let shape = context.infer_shape(expr.clone());
        assert!(context.check_shape(expr, shape.unwrap()));
    }

    #[test]
    fn shapecheck_2() {
        // check things fail when they are the wrong shape

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
        let shape = context.infer_shape(expr.clone());
        assert_eq!(shape, None);
        assert!(!context.check_shape(expr, bit_shape));

        let parser = ExprParser::new();
        let expr: Expr = bitsy.expr(&parser.parse("let x = 0; x").unwrap());
        let shape = context.infer_shape(expr.clone());
        assert_eq!(shape, None);

        let parser = ExprParser::new();
        let expr: Expr = bitsy.expr(&parser.parse("let x : Word<8> = 256; x").unwrap());
        assert_eq!(shape, None);
        assert!(!context.check_shape(expr, word_8_shape));
    }
}
