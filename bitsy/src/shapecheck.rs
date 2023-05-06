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

pub fn infer_shape(context: &ShapeContext, circuit: &Circuit, expr: &Expr) -> Option<Shape> {
    match expr {
        Expr::Term(term) => context.lookup(&format!("{}.{}", term.0, term.1)),
        Expr::Lit(value) => infer_shape_lit(circuit, value),
        Expr::Let(x, def, body) => todo!(), // need to add a context after all
        Expr::Add(op0, op1) => todo!(), // infer either or then check the other
        Expr::Mul(op0, op1) => todo!(), // infer either or then check the other
        Expr::As(expr0, shape0) => {
            if !check_shape(context, circuit, expr0, shape0) {
                None
            } else {
                Some(Shape::clone(shape0))
            }
        },
    }
}

pub fn check_shape(context: &ShapeContext, circuit: &Circuit, expr: &Expr, shape: &Shape) -> bool {
    match expr {
        Expr::Term(term) => {
            if let Some(shape0) = context.lookup(&format!("{}.{}", term.0, term.1)) {
                *shape == shape0
            } else {
                false
            }
        },
        Expr::Lit(value) => check_shape_lit(value, shape),
        Expr::Let(x, def, body) => todo!(), // need to add a context after all
        Expr::Add(op0, op1) => todo!(),
        Expr::Mul(op0, op1) => todo!(),
        Expr::As(expr0, shape0) =>  check_shape(context, circuit, expr0, shape0) && *shape == **shape0,
    }
}

fn infer_shape_lit(circuit: &Circuit, value: &Value) -> Option<Shape> {
    match value {
        Value::Bit(_b) => Some(Shape::Bit),
        Value::Word(v) => None, // because you can't infer the bitwidth
        Value::Tuple(vs) => todo!(), // try to infer each v in vs, if you can then good.
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

