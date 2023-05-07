use std::sync::Arc;
use crate::{Bitsy, Expr, Shape, Value};

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
        }
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
        let expr: Box<Expr> = Expr::from(&parser.parse("true").unwrap(), &bitsy);
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
        let expr: Box<Expr> = Expr::from(&parser.parse("let x : Bit = true; x").unwrap(), &bitsy);
        let shape = infer_shape(&ShapeContext::empty(), &expr.clone());
        assert!(check_shape(&ShapeContext::empty(), &*expr, &shape.unwrap()));

        let parser = ExprParser::new();
        let expr: Box<Expr> = Expr::from(&parser.parse("let x = true; x").unwrap(), &bitsy);
        let shape = infer_shape(&ShapeContext::empty(), &expr.clone());
        assert!(check_shape(&ShapeContext::empty(), &*expr, &Shape::Bit));

        let parser = ExprParser::new();
        let expr: Box<Expr> = Expr::from(&parser.parse("let x : Word<8> = 255; x").unwrap(), &bitsy);
        let shape = infer_shape(&ShapeContext::empty(), &expr.clone());
        assert!(check_shape(&ShapeContext::empty(), &*expr, &shape.unwrap()));

        let parser = ExprParser::new();
        let expr: Box<Expr> = Expr::from(&parser.parse("let x : Word<0> = 0; x").unwrap(), &bitsy);
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
        let expr: Box<Expr> = Expr::from(&parser.parse("let x : Bit = 0; x").unwrap(), &bitsy);
        let shape = infer_shape(&ShapeContext::empty(), &expr.clone());
        assert_eq!(shape, None);
        assert!(!check_shape(&ShapeContext::empty(), &*expr, &Shape::Bit));

        let parser = ExprParser::new();
        let expr: Box<Expr> = Expr::from(&parser.parse("let x = 0; x").unwrap(), &bitsy);
        let shape = infer_shape(&ShapeContext::empty(), &expr.clone());
        assert_eq!(shape, None);

        let parser = ExprParser::new();
        let expr: Box<Expr> = Expr::from(&parser.parse("let x : Word<8> = 256; x").unwrap(), &bitsy);
        assert_eq!(shape, None);
        assert!(!check_shape(&ShapeContext::empty(), &*expr, &Shape::Word(8)));
    }
}
