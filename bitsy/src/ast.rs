use std::sync::Arc;

pub type ComponentName = String;
pub type PortName = String;

#[derive(Debug)]
pub struct Circuit {
    pub decls: Vec<Decl>,
}

impl Circuit {
    pub fn mod_defs(&self) -> Vec<Arc<ModDef>> {
        let mut result = vec![];
        for decl in &self.decls {
            if let Decl::ModDef(mod_def) = decl {
                result.push(Arc::clone(mod_def));
            }
        }
        result
    }

    pub fn enum_defs(&self) -> Vec<Arc<EnumDef>> {
        let mut result = vec![];
        for decl in &self.decls {
            if let Decl::EnumDef(enum_def) = decl {
                result.push(Arc::clone(enum_def));
            }
        }
        result
    }

    pub fn mod_def(&self, name: &str) -> Arc<ModDef> {
        for mod_def in self.mod_defs() {
            if mod_def.name == name {
                return Arc::clone(&mod_def);
            }
        }

        panic!("No such module found: {name}")
    }

    pub fn enum_def(&self, name: &str) -> Arc<EnumDef> {
        for enum_def in self.enum_defs() {
            if enum_def.name == name {
                return Arc::clone(&enum_def);
            }
        }

        panic!("No such enum found: {name}")
    }
}


#[derive(Debug)]
pub enum Decl {
    ModDef(Arc<ModDef>),
    EnumDef(Arc<EnumDef>),
}

#[derive(Debug, Clone)]
pub struct EnumDef {
    pub name: String,
    pub visibility: Visibility,
    pub alts: Vec<EnumAlt>,
}

#[derive(Debug, Clone)]
pub struct EnumAlt {
    pub ctor_name: String,
    pub payload_shape: Option<Shape>,
}

#[derive(Debug, Clone)]
pub struct ModDef {
    pub name: String,
    pub visibility: Visibility,
    pub ports: Vec<Port>,
    pub components: Vec<Component>,
    pub wires: Vec<Wire>,
}

#[derive(Debug, Clone)]
pub struct Terminal(pub ComponentName, pub PortName);

impl Terminal {
    pub fn component(&self) -> &ComponentName {
        &self.0
    }

    pub fn port(&self) -> &PortName {
        &self.1
    }
}

#[derive(Debug, Clone)]
pub enum Wire {
    Simple(Visibility, Terminal, Terminal),
    Expr(Visibility, Terminal, Arc<Expr>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Term(Terminal),
    Lit(Value),
    Add(Arc<Expr>, Arc<Expr>),
    Mul(Arc<Expr>, Arc<Expr>),
}

impl Wire {
    pub fn visibility(&self) -> Visibility {
        match self {
            Wire::Simple(visibility, _sink, _source) => *visibility,
            Wire::Expr(visibility, _sink, _expr) => *visibility,
        }
    }

    pub fn sink(&self) -> &Terminal {
        match self {
            Wire::Simple(_visibility, sink, _source) => sink,
            Wire::Expr(_visibility, sink, _expr) => sink,
        }
    }

    /*
    pub fn source(&self) -> &Terminal {
        &self.2
    }
    */
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Direction {
    Incoming,
    Outgoing,
}

#[derive(Debug, Clone, Copy)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone)]
pub enum Component {
    Reg(ComponentName, Visibility, RegComponent),
    Mod(ComponentName, Visibility, ModComponent),
    Gate(ComponentName, Visibility, GateComponent),
    Const(ComponentName, Visibility, Value),
}

impl Component {
    pub fn name(&self) -> &str {
        match self {
            Component::Reg(name, _, _) => name,
            Component::Mod(name, _, _) => name,
            Component::Gate(name, _, _) => name,
            Component::Const(name, _, _) => name,
        }
    }

    pub fn port<T>(&self, port_name: T) -> Terminal
        where T: Into<PortName> {
        Terminal(self.name().to_string(), port_name.into())
    }
}

#[derive(Debug, Clone)]
pub struct RegComponent {
    pub shape: Shape,
    pub domain: Domain,
    pub init: Value,
}

#[derive(Debug, Clone)]
pub struct ModComponent {
    pub moddef_name: String,
}

#[derive(Debug, Clone)]
pub struct GateComponent {
    pub gate_name: String,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value {
    Unknown,
    Unobservable,
    Bool(bool),
    Word(u64),
    Tuple(Vec<Box<Value>>),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Value::Unknown => write!(f, "?")?,
            Value::Unobservable => write!(f, "X")?,
            Value::Bool(b) => write!(f,"{b}")?,
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
pub struct Port(pub String, pub Direction, pub Shape, pub Domain);

impl Port {
    pub fn name(&self) -> &str {
        &self.0
    }

    pub fn direction(&self) -> Direction {
        self.1
    }

    pub fn shape(&self) -> &Shape {
        &self.2
    }

    pub fn domain(&self) -> &Domain {
        &self.3
    }
}

#[derive(Debug, Clone)]
pub struct Shape(pub String, pub Vec<ShapeParam>);

#[derive(Debug, Clone)]
pub struct Domain;

impl Domain {
    pub fn name(&self) -> &str {
        &"d"
    }
}

#[derive(Debug, Clone)]
pub enum ShapeParam {
    Nat(u64),
    Shape(Arc<Shape>),
}

impl Circuit {
    pub fn flatten_exprs(&self) -> Circuit {
        let mut decls = vec![];

        for decl in &self.decls {
            match decl {
                Decl::ModDef(mod_def) => {
                    let mut flattened_mod_def: ModDef = ModDef::clone(mod_def);
                    flattened_mod_def.flatten_exprs();
                    decls.push(Decl::ModDef(Arc::new(flattened_mod_def)));
                },
                Decl::EnumDef(enum_def) => {
                    decls.push(Decl::EnumDef(Arc::clone(enum_def)));
                },
            }
        }
        Circuit {
            decls,
        }
    }
}

impl ModDef {
    fn flatten_exprs(&mut self) {
        let mut gensym_id = 0;
        let mut expr_wire_indexes: Vec<usize> = vec![];
        let mut wires_to_add: Vec<Wire> = vec![];
        let mut components_to_add: Vec<Component> = vec![];

        for (i, wire) in self.wires.iter().enumerate() {
            if let Wire::Expr(visibility, sink, expr) = wire {
                expr_wire_indexes.push(i);
                let (new_components, new_wires, source) = self.flatten_expr(
                    expr.clone(),
                    &mut gensym_id,
                );
                components_to_add.extend_from_slice(&new_components);
                wires_to_add.extend_from_slice(&new_wires);
                wires_to_add.push(Wire::Simple(*visibility, sink.clone(), source));
            }
        }

        for i in expr_wire_indexes.into_iter().rev() {
            self.wires.remove(i);
        }

        self.components.extend_from_slice(&components_to_add);
        self.wires.extend_from_slice(&wires_to_add);
    }

    fn flatten_expr(
        &self,
        expr: Arc<Expr>,
        gensym_id: &mut usize,
    ) -> (
        Vec<Component>,
        Vec<Wire>,
        Terminal,
    ) {
        match &*expr {
            Expr::Term(terminal) => {
                (
                    vec![],
                    vec![],
                    terminal.clone(),
                )
            },
            Expr::Lit(v) => {
                let name = format!("__gen_{gensym_id}");
                *gensym_id += 1;
                let gate = Component::Const(name, Visibility::Private, v.clone());
                (
                    vec![gate.clone()],
                    vec![],
                    gate.port("val"),
                )
            },
            Expr::Add(op0, op1) => {
                let (new_components0, new_wires0, op_terminal0) = self.flatten_expr(op0.clone(), gensym_id);
                let (new_components1, new_wires1, op_terminal1) = self.flatten_expr(op1.clone(), gensym_id);

                let name = format!("__gen_{gensym_id}");
                *gensym_id += 1;
                let gate = Component::Gate(name, Visibility::Private, GateComponent { gate_name: "Add".to_string() });
                (
                    vec![gate.clone()],
                    vec![
                        Wire::Simple(Visibility::Private, gate.port("in0"), op_terminal0),
                        Wire::Simple(Visibility::Private, gate.port("in1"), op_terminal1),
                    ],
                    gate.port("out"),
                )
            },
            Expr::Mul(op0, op1) => {
                let (new_components0, new_wires0, op_terminal0) = self.flatten_expr(op0.clone(), gensym_id);
                let (new_components1, new_wires1, op_terminal1) = self.flatten_expr(op1.clone(), gensym_id);

                let name = format!("__gen_{gensym_id}");
                *gensym_id += 1;
                let gate = Component::Gate(name, Visibility::Private, GateComponent { gate_name: "Mul".to_string() });
                (
                    vec![gate.clone()],
                    vec![
                        Wire::Simple(Visibility::Private, gate.port("in0"), op_terminal0),
                        Wire::Simple(Visibility::Private, gate.port("in1"), op_terminal1),
                    ],
                    gate.port("out"),
                )
            },
        }
    }
}
