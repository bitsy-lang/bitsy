#![allow(unused, dead_code)]

use std::collections::{HashSet, HashMap};

use log::*;
use parser::NamespaceParser;

#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub parser);

mod common;
pub mod shapecheck;
pub mod kindcheck;
pub mod ast;
pub mod depends;
pub mod verilog;
pub mod defs;
pub mod flatten;
pub mod context;
// pub mod sim;
// pub mod nettle;

use defs::{Module, Gate, Shape, ShapeFamily, StructField, ShapeNode, Expr, EnumAlt};
pub use context::Context;

pub use common::*;

#[derive(Debug)]
pub struct Bitsy {
    modules: Vec<Module>,
    gates: Vec<Gate>,
    shape_families: Vec<ShapeFamily>,
}

impl Bitsy {
    pub fn new() -> Bitsy {
        Bitsy {
            modules: vec![],
            gates: Gate::builtins(),
            shape_families: ShapeFamily::builtins(),
        }
    }

    pub fn add(&mut self, text: &str)  {
        use lalrpop_util::ParseError;

        let parser = NamespaceParser::new();
        match parser.parse(text) {
            Ok(namespace) => self.add_namespace(&namespace),
            Err(e) => {
                match (e) {
                    ParseError::InvalidToken { location } => {
                        eprintln!("Syntax error: Invalid token");
                        eprintln!();

                        let mut bad_line_start = 0;
                        let mut bad_line_end = 0;
                        let mut bad_lineno = 1;

                        let mut text_lines: Vec<&str> = text.split("\n").collect();
                        if text_lines[text_lines.len()-1] == "" {
                            text_lines.pop();
                        }

                        for line in &text_lines {
                            if bad_line_start + line.len() >= location {
                                bad_line_end = bad_line_start + line.len() + 1;
                                break;
                            } else {
                                bad_line_start += line.len() + 1;
                                bad_lineno += 1;
                            }
                        }

                        let spaces = String::from(" ").repeat(location - bad_line_start);
                        let carrots = String::from("^");


                        for i in 0..text_lines.len() {
                            if i + 5 >= bad_lineno && i <= bad_lineno + 5 {
                                eprintln!("{:>6}    {}", i + 1, &text_lines[i]);
                            }
                            if i + 1 == bad_lineno {
                                eprintln!("          {spaces}{carrots}");
                            }
                        }

                        eprintln!();
                    },
                    ParseError::UnrecognizedToken { token, expected } => {
                        let (location_start, found, location_end) = token;
                        eprintln!("Syntax error: Expected one of {} but found {:?}", expected.join(", "), found.to_string());
                        eprintln!();

                        let mut bad_line_start = 0;
                        let mut bad_line_end = 0;
                        let mut bad_lineno = 1;

                        let mut text_lines: Vec<&str> = text.split("\n").collect();
                        if text_lines[text_lines.len()-1] == "" {
                            text_lines.pop();
                        }

                        for line in &text_lines {
                            if bad_line_start + line.len() >= location_start {
                                bad_line_end = bad_line_start + line.len() + 1;
                                break;
                            } else {
                                bad_line_start += line.len() + 1;
                                bad_lineno += 1;
                            }
                        }

                        let spaces = String::from(" ").repeat(location_start - bad_line_start);
                        let carrots = String::from("^").repeat(location_end - location_start);


                        for i in 0..text_lines.len() {
                            if i + 5 >= bad_lineno && i <= bad_lineno + 5 {
                                eprintln!("{:>6}    {}", i + 1, &text_lines[i]);
                            }
                            if i + 1 == bad_lineno {
                                eprintln!("          {spaces}{carrots}");
                            }
                        }

                        eprintln!();
                    },
                    ParseError::UnrecognizedEOF { location, expected } => {
                        eprintln!("Syntax error: Expected {} but found the end of the file", expected.join(", "));
                        eprintln!();

                        let mut bad_line_start = 0;
                        let mut bad_line_end = 0;
                        let mut bad_lineno = 1;

                        let mut text_lines: Vec<&str> = text.split("\n").collect();
                        if text_lines[text_lines.len()-1] == "" {
                            text_lines.pop();
                        }

                        for line in &text_lines {
                            if bad_line_start + line.len() >= location {
                                bad_line_end = bad_line_start + line.len() + 1;
                                break;
                            } else {
                                bad_line_start += line.len() + 1;
                                bad_lineno += 1;
                            }
                        }

                        let spaces = String::from(" ").repeat(location - bad_line_start);
                        let carrots = String::from("^");

                        for i in 0..text_lines.len() {
                            if i + 5 >= bad_lineno && i <= bad_lineno + 5 {
                                eprintln!("{:>6}    {}", i + 1, &text_lines[i]);
                            }
                            if i + 1 == bad_lineno {
                                eprintln!("          {spaces}{carrots}");
                            }
                        }

                        eprintln!();
                    },
                    _ => eprintln!("{e:?}"),
                }
            }
        }
    }

    fn add_namespace(&mut self, namespace: &ast::Namespace) {
        self.add_shape_defs(namespace);
        self.add_module_defs(namespace);
    }

    fn add_module_defs(&mut self, namespace: &ast::Namespace) {
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

    fn add_shape_defs(&mut self, namespace: &ast::Namespace) {
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
                self.add_shape_family(&namespace.shape_def(&shape_ref));
            }
        }
    }

    fn add_shape_family(&mut self, shape_def: &ast::ShapeDef) {
        match shape_def {
            ast::ShapeDef::EnumDef(enum_def) => self.add_enum_shape_family(enum_def),
            ast::ShapeDef::StructDef(struct_def) => self.add_struct_shape_family(struct_def),
        }
    }

    fn add_struct_shape_family(&mut self, struct_def: &ast::StructDef) {
        let mut fields = vec![];
        let context = &struct_def.params;
        for ast::StructField(field_name, shape_ref) in &struct_def.fields {
            if let Some(shape) = self.shape(shape_ref, context) {
                if !context.check(shape.clone(), Kind::Shape) {
                    panic!("Not a thing");
                }
                fields.push(StructField(field_name.to_string(), shape.clone()));
            } else {
                panic!("Uh oh");
            }
        }
        let shape_family = ShapeFamily::new_struct(&struct_def.name, struct_def.params.clone(), fields);
        self.shape_families.push(shape_family);
    }

    fn add_enum_shape_family(&mut self, enum_def: &ast::EnumDef) {
        let mut context: Context<Kind> = enum_def.params.clone();

        let mut alts = vec![];
        for ast::EnumAlt(ctor_name, payload_shape_ref) in &enum_def.alts {
            alts.push(EnumAlt::new(ctor_name.to_string(), payload_shape_ref.as_ref().map(|shape_ref| self.shape(shape_ref, &context)).flatten()));
        }

        let shape_family = ShapeFamily::new_enum(&enum_def.name, enum_def.params.clone(), alts);
        self.shape_families.push(shape_family);
    }

    fn shape(&self, shape_ref: &ShapeRef, context: &Context<Kind>) -> Option<Shape> {
        info!("Looking up shape {shape_ref:?} in context {context:?}");
        let ShapeRef(shape_family_name, shape_args) = shape_ref;

        info!("Is it ({shape_family_name}) in the context?");
        if let Some(_kind) = context.lookup(shape_family_name) {
            info!("    ... YES");
            if shape_args.len() == 0 {
                return Some(Shape::var(shape_family_name.to_string()));
            } else {
                return None;
            }
        }
        info!("    ... no");

        let shape_family = self.shape_family(shape_family_name).expect(&format!("Unknown shape family {shape_family_name}"));

        let mut shapes: Vec<Shape> = vec![];
        for shape_arg in shape_args {
            let shape_arg: Shape = match shape_arg {
                ShapeArg::Nat(n) => Shape::nat(*n),
                ShapeArg::Shape(shape_ref) => self.shape(shape_ref, context).expect("Unknown shape"),
            };
            shapes.push(shape_arg);
        }
        Some(Shape::family(shape_family, shapes))
    }

    fn gate(&self, gate_name: &str) -> Option<Gate> {
        for gate in &self.gates {
            if gate_name == gate.name() {
                return Some(gate.clone());
            }
        }
        None
    }

    fn add_module(&mut self, mod_def: &ast::ModDef) {
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
                    panic!("Unknown shape: {shape_ref:?}");
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
                        panic!("Unknown shape: {:?}", &reg_component.shape);
                    }
                },
                _ => (),
            }
        }

        let mut wires = vec![];
        for ast::Wire(visibility, sink_terminal_ref, ast_expr) in &mod_def.wires {
            driven_terminals.push(sink_terminal_ref.clone());

            let expr = self.expr(ast_expr);
            let sink_terminal: &Terminal = &terminals_by_ref.get(sink_terminal_ref).unwrap();
            let shape = sink_terminal.shape();
            let mut context: Context<Shape> = Context::empty();
            for terminal in &terminals {
                context = context.extend(terminal.name().clone(), Shape::clone(&terminal.shape()));
            }
            println!("Checking {:?} has shape {} in context {}", &expr, &shape, &context);
            if !context.check_shape(expr.clone(), shape.clone()) {
                panic!("Shape check failed: {:?} is not {:?}", expr, shape);
            }

            wires.push(Wire(*visibility, sink_terminal.clone(), expr));
        }

        let mut components: Vec<Component> = vec![];
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
                    Component::Const(name.to_string(), *visibility, value.clone(), shape)
                },
                ast::Component::Gate(name, visibility, gate)   => {
                    Component::Gate(name.to_string(), *visibility, GateComponent {
                        gate: self.gate(gate.gate_ref.name()).expect("Unknown gate"),
                    })
                },
            };
            components.push(c);
        }
    }

    fn shape_family(&self, name: &str) -> Option<ShapeFamily> {
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
            ast::Expr::Var(x) => Expr::var(x.to_string()),
            ast::Expr::Lit(v) => Expr::lit(Value::from(v.clone())),
            ast::Expr::Let(x, def, def_shape, body) => {
                match def_shape {
                    Some(def_shape0) => {
                        // todo!() context shouldn't be empty here?
                        let shape = self.shape(def_shape0, &Context::empty()).expect("Unknown shape");
                        Expr::let_expr(x.to_string(), self.expr(def), Some(shape), self.expr(body))
                    },
                    None => {
                        Expr::let_expr(x.to_string(), self.expr(def), None, self.expr(body))
                    },
                }
            }
            ast::Expr::Add(op0, op1) => Expr::add(self.expr(op0), self.expr(op1)),
            ast::Expr::Mul(op0, op1) => todo!(),
            ast::Expr::Eq(op0, op1) => Expr::eq(self.expr(op0), self.expr(op1)),
            ast::Expr::Neq(op0, op1) => Expr::neq(self.expr(op0), self.expr(op1)),
            ast::Expr::Tuple(es) => Expr::tuple(es.iter().map(|e| self.expr(e)).collect()),
            ast::Expr::Struct(fs) => {
                Expr::struct_expr(fs.iter().map(|(field_name, e)| {
                    (field_name.clone(), self.expr(e))
                }).collect())
            },
            ast::Expr::Enum(ctor_name, payload) => Expr::enum_expr(ctor_name.to_string(), payload.as_ref().map(|e| self.expr(e))),
            ast::Expr::Match(e, arms) => {
                let match_arms = arms.iter().map(|ast::MatchArm(pat, e)| {
                    MatchArm(*pat.clone(), self.expr(e))
                }).collect();
                Expr::match_expr(self.expr(e), match_arms)
            },
            ast::Expr::Slice(subject, index) => {
                //if is_constant(index) {
                if false {
//                    Expr::slice_const(self.expr(subject), self.expr(index))
                    todo!()
                } else {
                    Expr::slice(self.expr(subject), self.expr(index))
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
pub struct Terminal(ComponentName, PortName, Polarity, Shape);

impl Terminal {
    pub fn name(&self) -> String {
        format!("{}.{}", &self.0, &self.1)
    }

    pub fn to_ref(&self) -> TerminalRef {
        TerminalRef(self.0.clone(), self.1.clone())
    }

    pub fn shape(&self) -> Shape {
        self.3.clone()
    }

    pub fn to_expr(&self) -> Box<Expr> {
        Box::new(Expr::var(self.name()))
    }
}

#[derive(Debug, Clone)]
pub enum Component {
    Reg(ComponentName, Visibility, RegComponent),
    Mod(ComponentName, Visibility, ModComponent),
    Gate(ComponentName, Visibility, GateComponent),
    Const(ComponentName, Visibility, Value, Shape),
}

impl Component {
    pub fn name(&self) -> &str {
        match self {
            Component::Reg(name, _vis, _reg) => name,
            Component::Mod(name, _vis, _mod) => name,
            Component::Gate(name, _vis, _gate) => name,
            Component::Const(name, _vis, _value, _shape) => name,
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

#[derive(Debug, Clone)]
pub struct GateComponent {
    pub gate: Gate,
}


#[derive(Debug, Clone)]
pub struct ModComponent {
    pub module: Module,
}

#[derive(Debug, Clone)]
pub struct RegComponent {
    pub shape: Shape,
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
pub struct Pin(PortName, Direction, Shape);

impl Pin {
    pub fn name(&self) -> &str {
        &self.0
    }

    pub fn direction(&self) -> Direction {
        self.1
    }

    pub fn shape(&self) -> Shape {
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
