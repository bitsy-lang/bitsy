use crate::{Bitsy, Kind, StructField};
use crate::defs::{EnumAlt, Shape, ShapeNode};
use crate::context::Context;

impl Context<Kind> {
    pub fn check(&self, shape: Shape, kind: Kind) -> bool {
        match &*shape {
            ShapeNode::Var(x) => self.lookup(x).expect("Unknown variable") == kind,
            ShapeNode::Nat(_n) => kind == Kind::Nat,
            ShapeNode::Family(shape_family, args) => {
                match shape_family.params() {
                    None => {
                        // tuple
                        for arg in args {
                            if !self.check(arg.clone(), Kind::Shape) {
                                return false;
                            }
                        }
                        kind == Kind::Shape
                    },
                    Some(params) => {
                        if args.len() != params.len() {
                            return false;
                        }
                        for (arg, (_param_name, param_kind)) in args.iter().zip(params.iter()) {
                            if !self.check(arg.clone(), *param_kind) {
                                return false;
                            }
                        }
                        kind == Kind::Shape
                    },
                }
            },
        }
    }
}
