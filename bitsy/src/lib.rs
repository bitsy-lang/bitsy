#![allow(unused, dead_code)]

use std::collections::HashSet;
use ast::Namespace;
use parser::NamespaceParser;

use std::sync::Arc;
use log::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Private,
}

#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub parser);

mod common;
pub mod shapecheck;
pub mod ast;
pub mod depends;
// pub mod sim;
// pub mod nettle;
//
pub use common::*;

#[derive(Debug)]
pub struct Bitsy {
    modules: Vec<Arc<Module>>,
    shape_families: Vec<Arc<ShapeFamily>>,
}

impl Bitsy {
    pub fn new() -> Bitsy {
        Bitsy {
            modules: vec![],
            shape_families: vec![],
        }
    }

    pub fn add(&mut self, text: &str)  {
        info!("Adding text: \"{}...\"", &text[..90].split("\n").collect::<Vec<_>>().join("\\n"));
        let parser = NamespaceParser::new();
        let namespace = parser.parse(text).unwrap();
        self.add_from(&namespace);
    }

    fn add_from(&mut self, namespace: &Namespace) {
        let mut decl_names = HashSet::new();
        for decl in &namespace.decls {
            assert!(!decl_names.contains(decl.name()), "Duplicate declaration: {}", decl.name());
            decl_names.insert(decl.name().clone());
        }

        let mut depends = depends::Depends::<String>::new();

        for shape_def in &namespace.shape_defs() {
            depends.add(shape_def.name().to_string());
            for dep_shape_def in &shape_def.shape_refs() {
                depends.add_dependency(dep_shape_def.0.to_string(), shape_def.name().to_string());
            }
        }

        info!("Adding shape families");
        self.add_builtin_shape_families();
        for shape_ref in depends.sort().expect("Cycle detected") {
            info!("    {shape_ref}");
            // if not defined, define it
            if self.shape_family(&shape_ref).is_none() {
                self.add_shape_family(&namespace.shape_def(&shape_ref));
            }
        }

        let mut depends = depends::Depends::<String>::new();

        for mod_def in &namespace.mod_defs() {
            depends.add(mod_def.name.clone());
            for dep_mod_def in &mod_def.depends_on() {
                depends.add_dependency(mod_def.name.clone(), dep_mod_def.to_string());
            }
        }

        info!("Adding modules");
        for mod_name in depends.sort().expect("Cycle detected") {
            info!("    {mod_name}");
            self.add_module(&namespace.mod_def(&mod_name));
        }
    }

    fn add_builtin_shape_families(&mut self) {
        use ShapeParamType::{Nat, Shape};
        let is_struct = false;
        let is_enum = false;

        let builtins = [
            ShapeFamily { name: "Tuple".to_string(), args: None, is_struct, is_enum },
            ShapeFamily { name: "Bit".to_string(), args: Some(vec![]), is_struct, is_enum },
            ShapeFamily { name: "Word".to_string(), args: Some(vec![Nat]), is_struct, is_enum },
        ];
        for builtin in builtins {
            self.shape_families.push(Arc::new(builtin));
        }
    }

    fn add_shape_family(&mut self, shape_def: &ast::ShapeDef) {
        let args: Option<Vec<ShapeParamType>> = Some(shape_def.params().iter().map(|param| {
            match param {
                ast::ShapeDefParam::Nat(_name) => ShapeParamType::Nat,
                ast::ShapeDefParam::Shape(_name) => ShapeParamType::Shape,
            }
        }).collect());

        let (is_enum, is_struct) = match shape_def {
            ast::ShapeDef::EnumDef(_def) => (true, false),
            ast::ShapeDef::StructDef(_def) => (false, true),
        };

        let shape_family = ShapeFamily {
            name: shape_def.name().to_string(),
            args,
            is_enum,
            is_struct,
        };
        self.shape_families.push(Arc::new(shape_family));
    }

    fn add_module(&mut self, mod_def: &ast::ModDef) {
 //    pub name: String,
 //    pub visibility: Visibility,
 //    pub ports: Vec<Port>,
 //    pub components: Vec<Component>,
 //    pub wires: Vec<Wire>,
 //
        let mut ports = vec![];
        for ast::Port(name, direction, shape_ref, domain_ref) in &mod_def.ports {
            ports.push(Port(name.to_string(), Arc::new(self.shape(shape_ref))));
        }

        let module = Module {
            ports,
        };
        self.modules.push(Arc::new(module));
    }

    fn shape_family(&self, name: &str) -> Option<Arc<ShapeFamily>> {
        for shape_family in &self.shape_families {
            if shape_family.name == name {
                return Some(shape_family.clone());
            }
        }
        None
    }

    fn modules(&self) -> Vec<Arc<Module>> {
        todo!()
    }

    fn module(&self, name: &str) -> Option<Arc<Module>> {
        todo!()
    }

    fn shape(&self, shape_ref: &ShapeRef) -> Shape {
        let ShapeRef(shape_familly_name, args) = shape_ref;
        if let Some(shape_family) = self.shape_family(shape_familly_name) {
            shape_family.to_shape(&self, args.as_slice())
        } else {
            panic!("Shape family not defined: {shape_familly_name}")
        }
    }
}

#[derive(Debug)]
pub struct Module {
    pub ports: Vec<Port>,
}

#[derive(Debug)]
pub struct ShapeFamily {
    pub name: String,
    pub args: Option<Vec<ShapeParamType>>, // Option for variadic
    pub is_enum: bool,
    pub is_struct: bool,
}

impl ShapeFamily {
    fn to_shape(&self, bitsy: &Bitsy, args: &[ShapeParam]) -> Shape {
        match self.name.as_str() {
            "Bit" => Shape::Bit,
            "Word" => {
                if let ShapeParam::Nat(n) = args[0] {
                    Shape::Word(n)
                } else {
                    panic!("Improper args for Nat")
                }
            },
            "Tuple" => {
                let mut params: Vec<Arc<Shape>> = vec![];
                for arg in args {
                    if let ShapeParam::Shape(shape_ref) = arg {
                        let shape = bitsy.shape(shape_ref);
                        params.push(Arc::new(shape));
                    } else {
                        panic!("Improper arg: {arg:?}")
                    }
                }
                Shape::Tuple(params)
            },
            _ => todo!(),
        }
    }
}

#[derive(Debug)]
pub enum ShapeParamType {
    Nat,
    Shape,
}


pub type PortName = String;
pub type FieldName = String;

#[derive(Debug)]
pub struct Port(PortName, Arc<Shape>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Shape {
    Bit,
    Word(u64),
    Tuple(Vec<Arc<Shape>>),
    Enum(Arc<EnumShape>),
    Struct(Arc<StructShape>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumShape {
    name: String,
    alts: Vec<EnumAlt>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumAlt {
    ctor_name: String,
    visibility: Visibility,
    payload: Option<Arc<Shape>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructShape {
    name: String,
    visibility: Visibility,
    fields: Vec<StructField>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructField(FieldName, Arc<Shape>);

impl Shape {
    pub fn bitwidth(&self) -> u64 {
        match self {
            Shape::Bit => 1,
            Shape::Word(n) => *n,
            Shape::Tuple(shapes) => shapes.iter().map(|shape| shape.bitwidth()).sum(),
            Shape::Enum(enum_shape) => todo!(), // enum_def.bitwidth(),
            Shape::Struct(struct_shape) => todo!(), // struct_def.bitwidth(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value {
    Unknown,
    Unobservable,
    Bit(bool),
    Word(u64),
    Tuple(Vec<Box<Value>>),
}

impl Value {
    pub fn from(value: ast::Value) -> Value {
        match value {
            ast::Value::Bit(b) => Value::Bit(b),
            ast::Value::Word(n) => Value::Word(n),
            ast::Value::Tuple(vs) => {
                let mut new_vs = vec![];
                for v in vs {
                    new_vs.push(Box::new(Value::from(*v.clone())));
                }

                Value::Tuple(new_vs)
            },
            _ => panic!("Uh oh"),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Value::Unknown => write!(f, "?")?,
            Value::Unobservable => write!(f, "X")?,
            Value::Bit(b) => write!(f,"{b}")?,
            Value::Word(n) => write!(f, "{n}")?,
            Value::Tuple(elts) => {
                write!(f, "tuple(")?;
                for (i, elt) in elts.iter().enumerate() {
                    write!(f, "{elt}")?;
                    if i + 1 < elts.len() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "tuple)")?;
            },
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Var(String),
    Lit(Value),
    Let(String, Box<Expr>, Option<Shape>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
}

impl Expr {
    pub fn from(expr: &ast::Expr, bitsy: &Bitsy) -> Box<Expr> {
        Box::new(match expr {
            ast::Expr::Var(x) => Expr::Var(x.to_string()),
            ast::Expr::Lit(v) => Expr::Lit(Value::from(v.clone())),
            ast::Expr::Let(x, def, def_shape, body) => {
                match def_shape {
                    Some(def_shape0) => {
                        Expr::Let(x.to_string(), Expr::from(def, bitsy), Some(bitsy.shape(def_shape0)), Expr::from(body, bitsy))
                    },
                    None => {
                        Expr::Let(x.to_string(), Expr::from(def, bitsy), None, Expr::from(body, bitsy))
                    },
                }
            }
            ast::Expr::Add(op0, op1) => Expr::Add(Expr::from(op0, bitsy), Expr::from(op1, bitsy)),
            ast::Expr::Mul(op0, op1) => Expr::Mul(Expr::from(op0, bitsy), Expr::from(op1, bitsy)),
            _ => panic!("No as"),
        })
    }
}
