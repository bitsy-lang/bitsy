#![allow(unused, dead_code)]

use std::sync::Arc;
use std::collections::{HashSet, HashMap};
use lalrpop_util::ParseError;

use log::*;
use parser::NamespaceParser;

#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub parser);

mod common;
pub mod typecheck;
pub mod kindcheck;
pub mod ast;
pub mod depends;
pub mod verilog;
pub mod defs;
pub mod flatten;
pub mod context;
// pub mod sim;
// pub mod nettle;

use defs::{Module, Gate, Type, Shape, StructField, TypeNode, Expr, EnumAlt};
pub use context::Context;

pub use common::*;

#[derive(Debug)]
pub struct Bitsy {
    modules: Vec<Module>,
    gates: Vec<Gate>,
    shape_families: Vec<Shape>,
}

pub use common::{BitsyError, BitsyResult};

impl Bitsy {
    pub fn new() -> Bitsy {
        Bitsy {
            modules: vec![],
            gates: Gate::builtins(),
            shape_families: Shape::builtins(),
        }
    }

    pub fn add(&mut self, text: &str) -> Result<(), BitsyError> {
        let parser = NamespaceParser::new();
        match parser.parse(text) {
            Ok(namespace) => self.add_namespace(&namespace)?,
            Err(e) => {
                match e {
                    ParseError::InvalidToken { location } => {
                        let loc = Loc::new("Top.bitsy".to_string(), location, location + 1);
                        return Err(BitsyError::Parse(loc, "Invalid token".to_string()));
                    },
                    ParseError::UnrecognizedEOF { location, expected } => return Err(BitsyError::Parse(Loc::new("Top.bitsy".to_string(), location, location + 1), "Unexpected end of file".to_string())),
                    ParseError::UnrecognizedToken { token, expected } => {
                        let (location_start, found, location_end) = token;
                        let loc = Loc::new("Top.bitsy".to_string(), location_start, location_end);
                        return Err(BitsyError::Parse(loc, "Unrecognized token".to_string()));
                    },
                    _ => return Err(BitsyError::Unknown(Loc::unknown(), format!("{e:?}"))),
                }
            }
        }
        Ok(())
    }

    fn add_namespace(&mut self, namespace: &ast::Namespace) -> Result<(), BitsyError> {
        self.add_shape_defs(namespace)?;
        self.add_module_defs(namespace)?;
        Ok(())
    }

    fn add_module_defs(&mut self, namespace: &ast::Namespace) -> Result<(), BitsyError> {
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
            self.add_module(namespace.mod_def(&mod_name)?)?;
        }
        Ok(())
    }

    fn add_shape_defs(&mut self, namespace: &ast::Namespace) -> Result<(), BitsyError> {
        let mut depends = depends::Depends::<String>::new();

        let mut shape_defs_by_name: HashMap<&str, &ast::ShapeDef> = HashMap::new();

        let shape_defs = namespace.shape_defs();
        for shape_def in &shape_defs {
            shape_defs_by_name.insert(shape_def.name(), shape_def);
            depends.add(shape_def.name().to_string());
            for dep_shape_def in &shape_def.shape_refs() {
                depends.add_dependency(shape_def.name().to_string(), dep_shape_def.0.to_string());
            }
        }

        info!("Adding shape families");
        for shape_ref in &depends.sort().expect("Cycle detected") {

            if self.shape_family(&shape_ref).is_none() {
            info!("    Adding {shape_ref}");
                self.add_shape_family(&namespace.shape_def(&shape_ref)?)?;
            }
        }

        Ok(())
    }

    fn add_shape_family(&mut self, shape_def: &ast::ShapeDef) -> Result<(), BitsyError> {
        match shape_def {
            ast::ShapeDef::EnumDef(enum_def) => self.add_enum_shape_family(enum_def),
            ast::ShapeDef::StructDef(struct_def) => self.add_struct_shape_family(struct_def),
        }
    }

    fn add_struct_shape_family(&mut self, struct_def: &ast::StructDef) -> Result<(), BitsyError> {
        let mut fields = vec![];
        let context = &struct_def.params;
        for ast::StructField(field_name, shape_ref) in &struct_def.fields {
            if let Some(shape) = self.shape(shape_ref, context) {
                if !context.check(shape.clone(), Kind::Shape) {
                    return Err(BitsyError::Unknown(Loc::unknown(), "Kind check failed".to_string()));
                }
                fields.push(StructField(field_name.to_string(), shape.clone()));
            } else {
                    return Err(BitsyError::Unknown(Loc::unknown(), "Uh oh".to_string()));
            }
        }
        let shape_family = Shape::new_struct(&struct_def.name, struct_def.params.clone(), fields);
        self.shape_families.push(shape_family);
        Ok(())
    }

    fn add_enum_shape_family(&mut self, enum_def: &ast::EnumDef) -> Result<(), BitsyError> {
        let mut context: Context<Kind> = enum_def.params.clone();

        let mut alts = vec![];
        for ast::EnumAlt(ctor_name, payload_shape_ref) in &enum_def.alts {
            alts.push(EnumAlt::new(ctor_name.to_string(), payload_shape_ref.as_ref().map(|shape_ref| self.shape(shape_ref, &context)).flatten()));
        }

        let shape_family = Shape::new_enum(&enum_def.name, enum_def.params.clone(), alts);
        self.shape_families.push(shape_family);
        Ok(())
    }

    fn shape(&self, shape_ref: &ShapeRef, context: &Context<Kind>) -> Option<Type> {
        info!("Looking up shape {shape_ref:?} in context {context:?}");
        let ShapeRef(shape_family_name, shape_args) = shape_ref;

        info!("Is it ({shape_family_name}) in the context?");
        if let Some(_kind) = context.lookup(shape_family_name) {
            info!("    ... YES");
            if shape_args.len() == 0 {
                return Some(Type::var(shape_family_name.to_string()));
            } else {
                return None;
            }
        }
        info!("    ... no");

        let shape_family = self.shape_family(shape_family_name).expect(&format!("Unknown shape family {shape_family_name}"));

        let mut shapes: Vec<Type> = vec![];
        for shape_arg in shape_args {
            let shape_arg: Type = match shape_arg {
                ShapeArg::Nat(n) => Type::nat(*n),
                ShapeArg::Shape(shape_ref) => self.shape(shape_ref, context).expect("Unknown shape"),
            };
            shapes.push(shape_arg);
        }
        Some(Type::family(shape_family, shapes))
    }

    fn gate(&self, gate_name: &str) -> Option<Gate> {
        for gate in &self.gates {
            if gate_name == gate.name() {
                return Some(gate.clone());
            }
        }
        None
    }

    fn add_module(&mut self, mod_def: &ast::ModDef) -> Result<(), BitsyError> {
        let context: Context<Kind> = Context::empty();
        let module = Module::new(&mod_def.name);
        self.modules.push(module);

        info!("add_module: {}", &mod_def.name);

        let mut ports = vec![];
        info!("    Ports:");
        for ast::Port(port_name, port_pins) in &mod_def.ports {
            let mut pins: Vec<Pin> = vec![];
            info!("        port: {}", port_name);
            for ast::Pin(name, direction, shape_ref, domain_ref) in port_pins {
                info!("            pin: {}", name);
                if let Some(shape) = self.shape(&shape_ref, &Context::empty()) {
                    let pin = Pin(name.to_string(), *direction, shape);
                    pins.push(pin);
                } else {
                    return Err(BitsyError::Type(Loc::unknown(), format!("Unknown shape: {shape_ref:?}")));
                }
            }
            let port = Port(port_name.clone(), pins);
            ports.push(port);
        }

        let mut terminals_by_ref: HashMap<TerminalRef, Terminal> = HashMap::new();
        let mut terminals: Vec<Terminal> = vec![];
        let mut driven_terminals: Vec<TerminalRef> = vec![];
        // let mut floating_terminals = vec![]; // todo!()

        info!("    Terminals:");
        for port in &ports {
            for pin in port.pins() {
                let polarity = match pin.direction() {
                    Direction::Incoming => Polarity::Source,
                    Direction::Outgoing => Polarity::Sink,
                };
                info!("        terminal: io.{} : {}{}", port.name(), polarity, pin.shape());

                let terminal = Terminal(
                    port.name().to_string(),
                    pin.name().to_string(),
                    polarity,
                    pin.shape(),
                );

                if pin.direction() == Direction::Incoming {
                    driven_terminals.push(terminal.to_ref());
                }

                terminals_by_ref.insert(terminal.to_ref(), terminal.clone());
                terminals.push(terminal);
            }
        }

        for component in &mod_def.components {
            match component {
                ast::Component::Reg(name, visibility, reg_component) => {
                    if let Some(shape) = self.shape(&reg_component.shape, &context) {
                        //let domain = self.domain(domain);

                        let set_terminal = Terminal(name.to_string(), "set".to_string(), Polarity::Sink, shape.clone());
                        let val_terminal = Terminal(name.to_string(), "val".to_string(), Polarity::Source, shape.clone());

                        terminals_by_ref.insert(set_terminal.to_ref(), set_terminal.clone());
                        terminals.push(set_terminal);
                        terminals_by_ref.insert(val_terminal.to_ref(), val_terminal.clone());
                        terminals.push(val_terminal);
                    } else {
                        return Err(BitsyError::Type(Loc::unknown(), format!("Unknown shape: {:?}", &reg_component.shape)));
                    }
                },
                _ => (),
            }
        }

        let mut components: Vec<Arc<Component>> = vec![];
        for component in &mod_def.components {
            let c = match component {
                ast::Component::Mod(name, visibility, module)  => {
                    Component::Mod(name.to_string(), *visibility, ModComponent {
                        module: self.module(&module.mod_def_ref).expect("Unknown module definition"),
                    })
                },
                ast::Component::Reg(name, visibility, reg)     => {
                    let shape = self.shape(&reg.shape, &context).expect("Unknown shape");
                    Component::Reg(name.to_string(), *visibility, RegComponent {
                        shape,
                        init: reg.init.clone().map(|e| self.expr(&e)),
                    })
                },
                ast::Component::Const(name, visibility, value, shape_ref) => {
                    let shape = self.shape(shape_ref, &context).expect("Unknown shape");
                    Component::Const(name.to_string(), *visibility, Box::new(value.clone()), shape)
                },
                ast::Component::Gate(name, visibility, gate)   => {
                    Component::Gate(name.to_string(), *visibility, GateComponent {
                        gate: self.gate(gate.gate_ref.name()).expect("Unknown gate"),
                    })
                },
            };
            components.push(Arc::new(c));
        }

        let mut context: Context<Type> = Context::empty();
        debug!("Context:");
        for component in &components {
            debug!("    {} : {}", component.name(), Type::ref_(component.clone()));
            context = context.extend(component.name().to_string(), Type::ref_(component.clone()));
        }
        for port in &ports {
            let typ = Type::ref_(Component::Port(port.clone()).into());
            debug!("    {} : {}", port.name(), &typ);
            context = context.extend(port.name().to_string(), typ);
        }

        let mut wires = vec![];
        for ast::Wire(visibility, sink_terminal_ref, ast_expr) in &mod_def.wires {
            driven_terminals.push(sink_terminal_ref.clone());

            let expr = self.expr(ast_expr);
            let sink_terminal: &Terminal = &terminals_by_ref.get(sink_terminal_ref).unwrap();
            let shape = sink_terminal.shape();
            debug!("Checking {:?} has shape {} in context {}", &expr, &shape, &context);
            context.check_type(expr.clone(), shape.clone())?;

            wires.push(Wire(*visibility, sink_terminal.clone(), expr));
        }

        Ok(())
    }

    fn shape_family(&self, name: &str) -> Option<Shape> {
        for shape_family in &self.shape_families {
            if shape_family.name() == name {
                return Some(shape_family.clone());
            }
        }
        None
    }

    fn module(&self, mod_def_ref: &ModDefRef) -> Option<Module> {
        for module in &self.modules {
            if module.name() == mod_def_ref.0 {
                return Some(module.clone());
            }
        }
        None
    }

    pub fn expr(&self, expr: &ast::Expr) -> Expr {
        match expr {
            ast::Expr::Var(loc, x) => Expr::var(loc.clone(), x.to_string()),
            ast::Expr::Field(loc, e, field) => Expr::field(loc.clone(), self.expr(e), field.to_string()),
            ast::Expr::Lit(loc, v) => Expr::lit(loc.clone(), Value::from(v.clone())),
            ast::Expr::Cast(loc, e, shape) => {
                let shape = self.shape(shape, &Context::empty()).expect("Unknown shape");
                Expr::cast_expr(loc.clone(), self.expr(e), shape)
            },
            ast::Expr::Let(loc, x, def, def_shape, body) => {
                match def_shape {
                    Some(def_shape0) => {
                        // todo!() context shouldn't be empty here?
                        let shape = self.shape(def_shape0, &Context::empty()).expect("Unknown shape");
                        Expr::let_expr(loc.clone(), x.to_string(), self.expr(def), Some(shape), self.expr(body))
                    },
                    None => {
                        Expr::let_expr(loc.clone(), x.to_string(), self.expr(def), None, self.expr(body))
                    },
                }
            }
            ast::Expr::BinOp(loc, op, op0, op1) => Expr::binop(loc.clone(), *op, self.expr(op0), self.expr(op1)),
            ast::Expr::Eq(loc, op0, op1) => Expr::eq(loc.clone(), self.expr(op0), self.expr(op1)),
            ast::Expr::Neq(loc, op0, op1) => Expr::neq(loc.clone(), self.expr(op0), self.expr(op1)),
            ast::Expr::Tuple(loc, es) => Expr::tuple(loc.clone(), es.iter().map(|e| self.expr(e)).collect()),
            ast::Expr::Struct(loc, fs) => {
                Expr::struct_expr(loc.clone(), fs.iter().map(|(field_name, e)| {
                    (field_name.clone(), self.expr(e))
                }).collect())
            },
            ast::Expr::Enum(loc, ctor_name, payload) => Expr::enum_expr(loc.clone(), ctor_name.to_string(), payload.as_ref().map(|e| self.expr(e))),
            ast::Expr::Match(loc, e, arms) => {
                let match_arms = arms.iter().map(|ast::MatchArm(pat, e)| {
                    MatchArm(*pat.clone(), self.expr(e))
                }).collect();
                Expr::match_expr(loc.clone(), self.expr(e), match_arms)
            },
            ast::Expr::If(loc, e, t, f) => Expr::if_expr(loc.clone(), self.expr(e), self.expr(t), self.expr(f)),
            ast::Expr::Slice(loc, subject, index) => {
                //if is_constant(index) {
                if false {
//                    Expr::slice_const(self.expr(subject), self.expr(index))
                    todo!()
                } else {
                    Expr::slice(loc.clone(), self.expr(subject), self.expr(index))
                }
            },
        }
    }

    /*
    fn shapes(&self) -> Vec<Shape> {
        let mut results = vec![];
        for shape_family in &self.shape_families {
            results.extend_from_slice(&shape_family.shapes());
        }
        results
    }
    */
}


#[derive(Debug, Clone)]
pub struct Terminal(ComponentName, PortName, Polarity, Type);

impl Terminal {
    pub fn name(&self) -> String {
        format!("{}.{}", &self.0, &self.1)
    }

    pub fn to_ref(&self) -> TerminalRef {
        TerminalRef(self.0.clone(), self.1.clone())
    }

    pub fn shape(&self) -> Type {
        self.3.clone()
    }

    /*
    pub fn to_expr(&self) -> Box<Expr> {
        Box::new(Expr::var(self.name()))
    }
    */
}

#[derive(Debug, Clone)]
pub enum Component {
    Reg(ComponentName, Visibility, RegComponent),
    Mod(ComponentName, Visibility, ModComponent),
    Gate(ComponentName, Visibility, GateComponent),
    Const(ComponentName, Visibility, Box<Value>, Type),
    Port(Port),
}

impl Component {
    pub fn name(&self) -> &str {
        match self {
            Component::Reg(name, _vis, _reg) => name,
            Component::Mod(name, _vis, _mod) => name,
            Component::Gate(name, _vis, _gate) => name,
            Component::Const(name, _vis, _value, _shape) => name,
            Component::Port(port) => port.name(),
        }
    }

    pub fn ports(&self) -> Vec<Terminal> {
        match self {
            Component::Const(name, _vis, val, shape) => {
                vec![
                    Terminal(name.to_string(), "val".to_string(), Polarity::Source, shape.clone()),
                ]
            },
            Component::Reg(name, _vis, reg) => {
                vec![
                    Terminal(name.to_string(), "val".to_string(), Polarity::Source, reg.shape.clone()),
                    Terminal(name.to_string(), "set".to_string(), Polarity::Sink, reg.shape.clone()),
                ]
            },
            Component::Gate(name, _vis, gate) => {
                match gate.gate.name() {
                    "Add" => todo!(),
                    _ => todo!(),
                }
            },
            // Component::Mod(name, _vis, _mod) => name,
            _ => { todo!() },
        }
    }

    pub fn port(&self, port_name: &str) -> Terminal {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GateComponent {
    pub gate: Gate,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModComponent {
    pub module: Module,
}

#[derive(Debug, Clone)]
pub struct RegComponent {
    pub shape: Type,
    // domain todo!()
    pub init: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct Port(PortName, Vec<Pin>);

impl Port {
    pub fn name(&self) -> &str {
        &self.0
    }

    pub fn pins(&self) -> &[Pin] {
        &self.1
    }
}

#[derive(Debug, Clone)]
pub struct Pin(PortName, Direction, Type);

impl Pin {
    pub fn name(&self) -> &str {
        &self.0
    }

    pub fn direction(&self) -> Direction {
        self.1
    }

    pub fn shape(&self) -> Type {
        self.2.clone()
    }
}

/*
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
*/

#[derive(Debug, Clone)]
pub struct MatchArm(pub MatchPattern, pub Expr);

#[derive(Debug, Clone)]
pub struct Wire(pub Visibility, pub Terminal, pub Expr);
