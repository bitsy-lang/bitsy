use std::sync::Arc;
use crate::{Circuit, Expr, Shape, Value};

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
    match expr {
        Expr::Term(term) => context.lookup(&format!("{}.{}", term.0, term.1)),
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
        Expr::Add(op0, op1) => {
            todo!()
            /*
            match (infer_shape(context, op0), infer_shape(context, op1)) {
                (Some(shape0), Some(shape1)) => {
                    if shape0 == shape1 {
                        Some(shape0)
                    } else {
                        None
                    }
                },
                (Some(shape0), None) => Some(shape0),
                (None, Some(shape1)) => Some(shape1),
                (None, None) => None,
            }
            */
        },
        Expr::Mul(op0, op1) => {
            todo!()
            /*
            match (infer_shape(context, op0), infer_shape(context, op1)) {
                (Some(shape0), Some(shape1)) => {
                    if shape0 == shape1 {
                        Some(shape0)
                    } else {
                        None
                    }
                },
                (Some(shape0), None) => Some(shape0),
                (None, Some(shape1)) => Some(shape1),
                (None, None) => None,
            }
            */
        },
        Expr::As(expr0, shape0) => {
            if !check_shape(context, expr0, shape0) {
                None
            } else {
                Some(Shape::clone(shape0))
            }
        },
    }
}

pub fn check_shape(context: &ShapeContext, expr: &Expr, shape: &Shape) -> bool {
    if let Some(shape0) = infer_shape(context, expr) {
        return &shape0 == shape;
    }

    match expr {
        Expr::Term(term) => {
            if let Some(shape0) = context.lookup(&format!("{}.{}", term.0, term.1)) {
                *shape == shape0
            } else {
                false
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
        Expr::Add(op0, op1) => todo!(),
        Expr::Mul(op0, op1) => todo!(),
        Expr::As(expr0, shape0) =>  check_shape(context, expr0, shape0) && *shape == **shape0,
    }
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
                    if !check_shape_lit(value, shape) {
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
        let parser = ExprParser::new();
//        let expr: Box<Expr> = Expr::from(&parser.parse("let x : Word<2> = 2; x").unwrap());
        let expr: Box<Expr> = Expr::from(&parser.parse("true").unwrap());
        let shape = infer_shape(&ShapeContext::empty(), &*expr);

        dbg!(shape);
    }
}
