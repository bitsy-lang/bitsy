use std::sync::Arc;
use crate::{Bitsy, Expr, Shape, Value, StructField, EnumAlt, MatchArm, MatchPattern};

type VarName = String;

#[derive(Clone, Debug)]
pub struct ShapeContext(Vec<(VarName, Shape)>);

impl ShapeContext {
    pub fn empty() -> ShapeContext {
        ShapeContext(vec![])
    }

    pub fn lookup(&self, v: &VarName) -> Option<Shape> {
        for (v0, shape) in &self.0 {
            if v0 == v {
                return Some(shape.clone());
            }
        }
        None
    }

    pub fn extend(&self, v: VarName, shape: Shape) -> ShapeContext {
        let mut result = self.clone();
        result.0.push((v, shape));
        result
    }
}

pub fn infer_shape(context: &ShapeContext, expr: &Expr) -> Option<Shape> {
    let result = match expr {
        Expr::Var(x) => Some(context.lookup(&x).expect(&format!("No such variable: {x}"))),
        Expr::Lit(value) => infer_shape_lit(value),
        Expr::Let(x, def, def_shape, body) => {
            match (infer_shape(context, def), def_shape) {
                (Some(def_shape0), Some(def_shape1)) => {
                    if &def_shape0 == def_shape1 {
                        let new_context = context.extend(x.to_string(), def_shape0);
                        infer_shape(&new_context, body)
                    } else {
                        None
                    }
                },
                (Some(def_shape0), None) => {
                    let new_context = context.extend(x.to_string(), def_shape0);
                    infer_shape(&new_context, body)
                },
                (None, Some(def_shape1)) => {
                    if check_shape(context, def, def_shape1) {
                        let new_context = context.extend(x.to_string(), def_shape1.clone());
                        infer_shape(&new_context, body)
                    } else {
                        None
                    }
                },
                (None, None) => None,
            }
        },
        Expr::Add(op0, op1) => None,
        Expr::Mul(op0, op1) => None,
        // Expr::Match // todo!()
        _ => None,
    };
    result
}

pub fn check_shape(context: &ShapeContext, expr: &Expr, shape: &Shape) -> bool {
    let result = match expr {
        Expr::Var(x) => {
            if let Some(shape0) = context.lookup(&x) {
                *shape == shape0
            } else {
                panic!("No such variable: {x}")
            }
        },
        Expr::Lit(value) => check_shape_lit(value, shape),
        Expr::Let(x, def, None, body) => {
            if let Some(def_shape) = infer_shape(&context, def) {
                let new_context = context.extend(x.to_string(), def_shape);
                check_shape(&new_context, body, shape)
            } else {
                false
            }
        },
        Expr::Let(x, def, Some(def_shape), body) => {
            if !check_shape(&context, def, def_shape) {
                false
            } else {
                let new_context = context.extend(x.to_string(), def_shape.clone());
                check_shape(&new_context, body, shape)
            }
        },
        Expr::Add(op0, op1) => {
            match shape {
                Shape::Word(n) => {
                    if *n > 0 {
                        if check_shape(context, op0, &Shape::Word(*n - 1)) {
                            for i in 0..*n {
                                if check_shape(context, op1, &Shape::Word(i)) {
                                    return true
                                }
                            }
                        } else if check_shape(context, op1, &Shape::Word(*n - 1)) {
                            for i in 0..*n {
                                if check_shape(context, op0, &Shape::Word(i)) {
                                    return true
                                }
                            }
                        }
                    }
                    false
                },
                _ => false,
            }
        },
        Expr::Mul(op0, op1) => {
            match shape {
                Shape::Word(n) => {
                    for i in 0..*n {
                        if check_shape(context, op0, &Shape::Word(i)) &&
                           check_shape(context, op1, &Shape::Word(n - i)) {
                            return true;
                        }
                    }
                    false
                },
                _ => false,
            }
        },
        Expr::Match(subject, arms) => {
            if let Some(subject_shape) = &infer_shape(context, subject) {
                let enum_shape_family = if let Shape::Enum(enum_shape_family) = subject_shape {
                    Some(enum_shape_family)
                } else {
                    None
                };

                for MatchArm(pat, expr0) in arms {
                    let mut new_context = context.clone();
                    match &**pat {
                        MatchPattern::Ctor(ctor_name, pats) => {
                            if let Some(enum_shape_family) = enum_shape_family {
                                if let Some(alt) = enum_shape_family.alt_by_ctor(ctor_name) {
                                    let new_context: ShapeContext = match &alt.payload {
                                        Some(payload_shape) => {
                                            if pats.len() == 0 {
                                                context.clone()
                                            } else {
                                                if pats.len() != 1 {
                                                    return false;
                                                }
                                                if let MatchPattern::Var(x0) = &*pats[0] {
                                                    context.extend(x0.to_string(), Shape::clone(payload_shape))
                                                } else {
                                                    return false;
                                                    unreachable!()
                                                }
                                            }
                                        },
                                        None => context.clone(),
                                    };

                                    if !check_shape(&new_context, expr0, shape) {
                                        return false;
                                    }
                                } else {
                                    return false;
                                }
                            } else {
                                return false;
                            }
                        },
                        MatchPattern::Var(x) => {
                            let new_context = context.extend(x.to_string(), subject_shape.clone());
                            if !check_shape(&new_context, expr0, shape) {
                                return false;
                            }
                        },
                        MatchPattern::Lit(v) => {
                            if !check_shape(context, &Expr::Lit(v.clone()), subject_shape) {
                                return false;
                            }
                            if !check_shape(context, expr0, shape) {
                                return false;
                            }
                        },
                        MatchPattern::Otherwise => {
                            if !check_shape(context, expr0, shape) {
                                return false;
                            }
                        },
                    }
                }
                true
            } else {
                false
            }
        },
        Expr::Tuple(es) => {
            if let Shape::Tuple(ss) = shape {
                if ss.len() != es.len() {
                    return false;
                }

                for (e, s) in es.iter().zip(ss) {
                    if !check_shape(context, e, s) {
                        return false;
                    }
                }
                true
            } else {
                false
            }
        },
        Expr::Struct(fs) => {
            if let Shape::Struct(struct_shape_family) = shape {
                if fs.len() != struct_shape_family.fields.len() {
                    return false;
                }
                let mut fs_sorted = fs.clone();
                fs_sorted.sort_by_key(|(field_name, _val)| field_name.clone());
                let mut struct_fields_sorted = struct_shape_family.fields.clone();
                struct_fields_sorted.sort_by_key(|StructField(field_name, _shape)| field_name.clone());


                for ((field_name0, e), StructField(field_name1, s)) in fs_sorted.iter().zip(&struct_fields_sorted) {
                    if field_name0 != field_name1 || !check_shape(context, e, s) {
                        return false;
                    }
                }
                true
            } else {
                false
            }
        },
        Expr::Enum(ctor_name, payload) => {
            if let Shape::Enum(enum_shape_family) = shape {
                if let Some(alt) = enum_shape_family.alt_by_ctor(ctor_name) {
                    match (payload, &alt.payload) {
                        (None, None) => true,
                        (Some(payload_val), Some(payload_shape)) => check_shape(context, payload_val, payload_shape),
                        _ => false
                    }
                } else {
                    false
                }
            } else {
                false
            }
        },
    };
    result
}

fn infer_shape_lit(value: &Value) -> Option<Shape> {
    match value {
        Value::Bit(_b) => Some(Shape::Bit),
        Value::Word(v) => None, // because you can't infer the bitwidth
        Value::Tuple(vs) => {
            // try to infer each v in vs, if you can then good.
            let mut shapes: Vec<Arc<Shape>> = vec![];
            for opt_shape in vs.iter().map(|v| infer_shape_lit(v)) {
                if let Some(shape) = opt_shape {
                    shapes.push(Arc::new(shape.clone()));
                } else {
                    return None
                }
            }
            Some(Shape::Tuple(shapes))
        },
        _ => None,
    }
}

fn check_shape_lit(value: &Value, shape: &Shape) -> bool {
    match (value, shape) {
        (Value::Bit(_b), Shape::Bit) => true,
        (Value::Word(v), Shape::Word(n)) => *v < (1 << n),
        (Value::Tuple(vs), Shape::Tuple(ss)) => {
            if vs.len() != ss.len() {
                return false;
            } else {
                for (v, s) in vs.iter().zip(ss) {
                    if !check_shape_lit(v, s) {
                        return false;
                    }
                }
                true
            }
        }
        (Value::Struct(fs), Shape::Struct(struct_shape_family)) => {
            for ((field_name, field_val), StructField(shape_field_name, shape_field_shape)) in fs.iter().zip(struct_shape_family.fields.clone()) {
                if field_name != &shape_field_name {
                    return false;
                } else if !check_shape_lit(field_val, &shape_field_shape) {
                    return false;
                }

            }
            true
        },
        _ => false,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::*;

    #[test]
    fn shapecheck_0() {
        let mut bitsy = Bitsy::new();
        let text = "
            pub mod Top
            end
        ";

        bitsy.add(&text);
        let parser = ExprParser::new();
        let expr: Box<Expr> = bitsy.expr(&parser.parse("true").unwrap());
        let shape = infer_shape(&ShapeContext::empty(), &expr.clone());

        assert!(check_shape(&ShapeContext::empty(), &*expr, &shape.unwrap()));
    }

    #[test]
    fn shapecheck_1() {
        let mut bitsy = Bitsy::new();
        let text = "
            pub mod Top
            end
        ";
        bitsy.add(&text);

        let parser = ExprParser::new();
        let expr: Box<Expr> = bitsy.expr(&parser.parse("let x : Bit = true; x").unwrap());
        let shape = infer_shape(&ShapeContext::empty(), &expr.clone());
        assert!(check_shape(&ShapeContext::empty(), &*expr, &shape.unwrap()));

        let parser = ExprParser::new();
        let expr: Box<Expr> = bitsy.expr(&parser.parse("let x = true; x").unwrap());
        let shape = infer_shape(&ShapeContext::empty(), &expr.clone());
        assert!(check_shape(&ShapeContext::empty(), &*expr, &Shape::Bit));

        let parser = ExprParser::new();
        let expr: Box<Expr> = bitsy.expr(&parser.parse("let x : Word<8> = 255; x").unwrap());
        let shape = infer_shape(&ShapeContext::empty(), &expr.clone());
        assert!(check_shape(&ShapeContext::empty(), &*expr, &shape.unwrap()));

        let parser = ExprParser::new();
        let expr: Box<Expr> = bitsy.expr(&parser.parse("let x : Word<0> = 0; x").unwrap());
        let shape = infer_shape(&ShapeContext::empty(), &expr.clone());
        assert!(check_shape(&ShapeContext::empty(), &*expr, &shape.unwrap()));
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

        let parser = ExprParser::new();
        let expr: Box<Expr> = bitsy.expr(&parser.parse("let x : Bit = 0; x").unwrap());
        let shape = infer_shape(&ShapeContext::empty(), &expr.clone());
        assert_eq!(shape, None);
        assert!(!check_shape(&ShapeContext::empty(), &*expr, &Shape::Bit));

        let parser = ExprParser::new();
        let expr: Box<Expr> = bitsy.expr(&parser.parse("let x = 0; x").unwrap());
        let shape = infer_shape(&ShapeContext::empty(), &expr.clone());
        assert_eq!(shape, None);

        let parser = ExprParser::new();
        let expr: Box<Expr> = bitsy.expr(&parser.parse("let x : Word<8> = 256; x").unwrap());
        assert_eq!(shape, None);
        assert!(!check_shape(&ShapeContext::empty(), &*expr, &Shape::Word(8)));
    }
}
