#![allow(unused, dead_code)]

use std::collections::{HashSet, HashMap};
use ast::Namespace;
use parser::NamespaceParser;

use std::sync::Arc;
use log::*;

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

        let mut decl_names = HashSet::new();
        for decl in &namespace.decls {
            assert!(!decl_names.contains(decl.name()), "Duplicate declaration: {}", decl.name());
            decl_names.insert(decl.name().clone());
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
            self.add_module(&namespace.mod_def(&mod_name));
        }
    }

    fn add_builtin_shape_families(&mut self) {
        use ShapeParamType::{Nat, Shape};
        let enum_shape = None;
        let struct_shape = None;

        let builtins = [
            ShapeFamily { name: "Tuple".to_string(), args: None, struct_shape: struct_shape.clone(), enum_shape: enum_shape.clone() },
            ShapeFamily { name: "Bit".to_string(), args: Some(vec![]), struct_shape: struct_shape.clone(), enum_shape: enum_shape.clone() },
            ShapeFamily { name: "Word".to_string(), args: Some(vec![Nat]), struct_shape: struct_shape.clone(), enum_shape: enum_shape.clone() },
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

        let (enum_shape, struct_shape) = match shape_def {
            ast::ShapeDef::EnumDef(enum_def) => (Some(self.enum_shape_family(enum_def)), None),
            ast::ShapeDef::StructDef(struct_def) => (None, Some(self.struct_shape_family(struct_def))),
        };

        let shape_family = ShapeFamily {
            name: shape_def.name().to_string(),
            args,
            enum_shape,
            struct_shape,
        };
        self.shape_families.push(Arc::new(shape_family));
    }

    fn enum_shape_family(&self, enum_def: &ast::EnumDef) -> EnumShape {
        let mut alts: Vec<EnumAlt> = vec![];

        for ast::EnumAlt(ctor_name, payload_shape_ref) in &enum_def.alts {
            let payload = payload_shape_ref.as_ref().map(|shape_ref| Arc::new(self.shape(shape_ref)));
            let alt = EnumAlt {
                ctor_name: ctor_name.to_string(),
                payload,
            };
            alts.push(alt);
        }

        EnumShape {
            name: enum_def.name.to_string(), // String,
            alts,
        }
    }

    fn struct_shape_family(&self, struct_def: &ast::StructDef) -> StructShape {
        let mut fields = vec![];
        for ast::StructField(field_name, shape_ref) in &struct_def.fields {
            fields.push(StructField(field_name.to_string(), self.shape(&shape_ref).into()));
        }

        StructShape {
            name: struct_def.name.to_string(),
            visibility: struct_def.visibility,
            fields,
        }
    }

    fn add_module(&mut self, mod_def: &ast::ModDef) {
        info!("add_module: {}", &mod_def.name);

        let mut ports = vec![];
        info!("    Ports:");
        for ast::Port(name, direction, shape_ref, domain_ref) in &mod_def.ports {
            ports.push(Port(name.to_string(), *direction, Arc::new(self.shape(shape_ref))));
            info!("        port: {}", name);
        }

        let mut terminals_by_ref: HashMap<TerminalRef, Terminal> = HashMap::new();
        let mut terminals: Vec<Terminal> = vec![];
        let mut driven_terminals: Vec<TerminalRef> = vec![];
        // let mut floating_terminals = vec![]; // todo!()

        info!("    Terminals:");
        for port in &ports {
            let polarity = match port.direction() {
                Direction::Incoming => Polarity::Source,
                Direction::Outgoing => Polarity::Sink,
            };
            info!("        terminal: io.{} : {}{}", port.name(), polarity, port.shape());

            let terminal = Terminal(
                "io".to_string(),
                port.name().to_string(),
                polarity,
                port.shape(),
            );

            if port.direction() == Direction::Incoming {
                driven_terminals.push(terminal.to_ref());
            }

            terminals_by_ref.insert(terminal.to_ref(), terminal.clone());
            terminals.push(terminal);
        }

        for component in &mod_def.components {
            match component {
                ast::Component::Reg(name, visibility, reg_component) => {
                    let shape = self.shape(&reg_component.shape);
                    //let domain = self.domain(domain);

                    let set_terminal = Terminal(name.to_string(), "set".to_string(), Polarity::Sink, shape.clone().into());
                    let val_terminal = Terminal(name.to_string(), "val".to_string(), Polarity::Source, shape.clone().into());

                    info!("        terminal: {}.set : -{}", name, &shape);
                    info!("        terminal: {}.val : +{}", name, &shape);
                    terminals_by_ref.insert(set_terminal.to_ref(), set_terminal.clone());
                    terminals.push(set_terminal);
                    terminals_by_ref.insert(val_terminal.to_ref(), val_terminal.clone());
                    terminals.push(val_terminal);
                },
                _ => (),
            }
        }

        for ast::Wire(visibility, sink_terminal_ref, source) in &mod_def.wires {
            driven_terminals.push(sink_terminal_ref.clone());

            if let ast::WireSource::Expr(ast_expr) = source {
                let expr = self.expr(ast_expr);
                let sink_terminal: &Terminal = &terminals_by_ref.get(sink_terminal_ref).unwrap();
                let shape = sink_terminal.shape();
                let mut context = shapecheck::ShapeContext::empty();
                for terminal in &terminals {
                    context = context.extend(terminal.name(), Shape::clone(&terminal.shape()));

                }
                if !shapecheck::check_shape(&context, &expr, &shape) {
                    panic!("Shape check failed: {:?} is not {:?}", &expr, &shape);
                }
            }
        }

        let module = Module {
            ports,
            terminals,
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
            self.shape_from_family(&shape_family, args.as_slice())
        } else {
            panic!("Shape family not defined: {shape_familly_name}")
        }
    }

    pub fn expr(&self, expr: &ast::Expr) -> Box<Expr> {
        Box::new(match expr {
            ast::Expr::Var(x) => Expr::Var(x.to_string()),
            ast::Expr::Lit(v) => Expr::Lit(Value::from(v.clone())),
            ast::Expr::Let(x, def, def_shape, body) => {
                match def_shape {
                    Some(def_shape0) => {
                        Expr::Let(x.to_string(), self.expr(def), Some(self.shape(def_shape0)), self.expr(body))
                    },
                    None => {
                        Expr::Let(x.to_string(), self.expr(def), None, self.expr(body))
                    },
                }
            }
            ast::Expr::Add(op0, op1) => Expr::Add(self.expr(op0), self.expr(op1)),
            ast::Expr::Mul(op0, op1) => Expr::Mul(self.expr(op0), self.expr(op1)),
            ast::Expr::Tuple(es) => Expr::Tuple(es.iter().map(|e| self.expr(e)).collect()),
            ast::Expr::Struct(fs) => {
                Expr::Struct(fs.iter().map(|(field_name, e)| {
                    (field_name.clone(), self.expr(e))
                }).collect())
            },
            ast::Expr::Enum(ctor_name, payload) => Expr::Enum(ctor_name.to_string(), payload.as_ref().map(|e| self.expr(e))),
            ast::Expr::Match(e, arms) => {
                Expr::Match(self.expr(e), arms.iter().map(|ast::MatchArm(pat, e)| {
                    MatchArm(pat.clone(), self.expr(e))
                }).collect())
            },
        })
    }

    fn shape_from_family(&self, shape_family: &ShapeFamily, args: &[ShapeParam]) -> Shape {
        match shape_family.name.as_str() {
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
                        let shape = self.shape(shape_ref);
                        params.push(Arc::new(shape));
                    } else {
                        panic!("Improper arg: {arg:?}")
                    }
                }
                Shape::Tuple(params)
            },
            _ => {
                let shape_family = self.shape_family(&shape_family.name).expect("Shape Family not found");
                if let Some(enum_shape_family) = &shape_family.enum_shape {
                    Shape::Enum(Arc::new(enum_shape_family.clone()))
                } else if let Some(struct_shape_family) = &shape_family.struct_shape {
                    Shape::Struct(Arc::new(struct_shape_family.clone()))
                } else {
                    panic!("Expected shape family to either be an enum or struct")
                }
            },
        }
    }
}


#[derive(Debug, Clone)]
pub struct Terminal(ComponentName, PortName, Polarity, Arc<Shape>);

impl Terminal {
    pub fn name(&self) -> String {
        format!("{}.{}", &self.0, &self.1)
    }

    pub fn to_ref(&self) -> TerminalRef {
        TerminalRef(self.0.clone(), self.1.clone())
    }

    pub fn shape(&self) -> Arc<Shape> {
        self.3.clone()
    }
}

#[derive(Debug)]
pub struct Module {
    pub ports: Vec<Port>,
    pub terminals: Vec<Terminal>,
}

#[derive(Debug)]
pub struct ShapeFamily {
    pub name: String,
    pub args: Option<Vec<ShapeParamType>>, // Option for variadic
    pub enum_shape: Option<EnumShape>, // todo!() rename this EnumShapeFamily
    pub struct_shape: Option<StructShape>, // todo!() rename this StructShapeFamily
}

impl ShapeFamily {
    pub fn is_enum(&self) -> bool {
        self.enum_shape.is_some()
    }

    pub fn is_struct(&self) -> bool {
        self.struct_shape.is_some()
    }


}

#[derive(Debug)]
pub enum ShapeParamType {
    Nat,
    Shape,
}

impl std::fmt::Display for Shape {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Shape::Bit => write!(f, "Bit"),
            Shape::Word(n) => write!(f, "Word<{n}>"),
            Shape::Tuple(shapes) => {
                write!(f, "Tuple<")?;
                for (i, shape) in shapes.iter().enumerate() {
                    write!(f, "{shape}")?;
                    if i + 1 < shapes.len() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ">")?;
                Ok(())
            },
            Shape::Enum(enum_shape) => {
                // missing parameters
                write!(f, "{}", enum_shape.name)?;
                Ok(())
            }
            Shape::Struct(struct_shape) => {
                // missing parameters
                write!(f, "{}", struct_shape.name)?;
                Ok(())
            }
        }
    }
}

#[derive(Debug)]
pub struct Port(PortName, Direction, Arc<Shape>);

impl Port {
    pub fn name(&self) -> &str {
        &self.0
    }

    pub fn direction(&self) -> Direction {
        self.1
    }

    pub fn shape(&self) -> Arc<Shape> {
        self.2.clone()
    }
}

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

impl EnumShape {
    fn alt_by_ctor(&self, ctor_name: &str) -> Option<&EnumAlt> {
        for alt in &self.alts {
            if alt.ctor_name == ctor_name {
                return Some(alt)
            }
        }
        None
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumAlt {
    ctor_name: String,
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

#[derive(Debug, Clone)]
pub enum Expr {
    Var(String),
    Lit(Value),
    Let(String, Box<Expr>, Option<Shape>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Match(Box<Expr>, Vec<MatchArm>),
    Tuple(Vec<Box<Expr>>),
    Struct(Vec<(FieldName, Box<Expr>)>),
    Enum(CtorName, Option<Box<Expr>>),
}

#[derive(Debug, Clone)]
pub struct MatchArm(pub Box<MatchPattern>, pub Box<Expr>);

#[derive(Debug, Clone)]
pub struct Wire(pub Visibility, pub TerminalRef, pub WireSource);

#[derive(Debug, Clone)]
pub enum WireSource {
    Terminal(TerminalRef),
    Expr(Box<Expr>),
}
