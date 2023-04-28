
pub type ComponentName = String;
pub type PortName = String;

#[derive(Debug)]
pub struct Circuit {
    pub mod_defs: Vec<ModDef>,
}

impl Circuit {
    pub fn mod_def(&self, name: &str) -> &ModDef {
        for mod_def in &self.mod_defs {
            if mod_def.name == name {
                return mod_def;
            }
        }

        panic!("No such module found: {name}")
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Wire(pub Visibility, pub Terminal, pub Terminal);

impl Wire {
    pub fn visibility(&self) -> Visibility {
        self.0
    }

    pub fn sink(&self) -> &Terminal {
        &self.1
    }

    pub fn source(&self) -> &Terminal {
        &self.2
    }

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

#[derive(Debug)]
pub enum Component {
    Reg(ComponentName, Visibility, RegComponent),
    Mod(ComponentName, Visibility, ModComponent),
}

impl Component {
    pub fn name(&self) -> &str {
        match self {
            Component::Reg(name, _, _) => name,
            Component::Mod(name, _, _) => name,
        }
    }
}

#[derive(Debug)]
pub struct RegComponent {
    pub shape: Shape,
    pub domain: Domain,
    pub init: Value,
}

#[derive(Debug)]
pub struct ModComponent {
    pub moddef_name: String,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Value {
    Unknown,
    Unobservable,
    Bool(bool),
    Word(u64),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Unknown => write!(f, "?")?,
            Value::Unobservable => write!(f, "X")?,
            Value::Bool(b) => write!(f,"{b}")?,
            Value::Word(n) => write!(f, "{n}")?,
        }
        Ok(())
    }
}

#[derive(Debug)]
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
    Shape(Box<Shape>),
}
