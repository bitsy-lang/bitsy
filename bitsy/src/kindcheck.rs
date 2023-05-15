use crate::{Bitsy, Kind, StructField};
use crate::defs::{EnumAlt, Type, TypeNode};
use crate::context::Context;

impl Context<Kind> {
    pub fn check(&self, typ: Type, kind: Kind) -> bool {
        match &*typ {
            TypeNode::Var(x) => self.lookup(x).expect("Unknown variable") == kind,
            TypeNode::Nat(_n) => kind == Kind::Nat,
            TypeNode::Family(shape, args) => {
                match shape.params() {
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
