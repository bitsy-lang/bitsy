#![allow(unused, dead_code)]

pub type SignalPath = String;

#[derive(Debug)]
pub struct Domain(pub String);

impl Domain {
    pub fn name(&self) -> &str {
        &self.0
    }
}

#[derive(Debug)]
pub struct Nettle {
    pub domains: Vec<Domain>,
    pub signals: Vec<Signal>,
}

#[derive(Debug)]
pub struct Signal {
    pub path: SignalPath,
    pub shape: Shape,
    pub domain: Domain,
    pub attrs: Vec<Attr>,
}

impl Signal {
    pub fn is_output_port(&self) -> bool {
        self.attrs.contains(&Attr::OutputPort)
    }

    pub fn is_input_port(&self) -> bool {
        self.attrs.contains(&Attr::InputPort)
    }

    pub fn init_value(&self) -> Option<Value> {
        for attr in &self.attrs {
            if let Attr::Init(val) = attr {
                return Some(*val);
            }
        }
        None
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Attr {
    InputPort,
    OutputPort,
    Prev(SignalPath),
    Peek(SignalPath),
    Init(Value),
    Gate(String, Vec<SignalPath>),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Value {
    Unknown,
    Unobservable,
    Bool(bool),
    Word(u64),
}

impl std::fmt::Display for Shape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Shape(name, params) = self;
        write!(f, "{name}")?;
        if !params.is_empty() {
            write!(f, "<")?;
            write!(f, "{}", params.iter().map(|p| p.to_string()).collect::<Vec<String>>().join(", "))?;
            write!(f, ">")?;
        }
        Ok(())
    }
}

impl std::fmt::Display for ShapeParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ShapeParam::Nat(n) => write!(f, "{n}")?,
            ShapeParam::Shape(s) => write!(f, "{s}")?,
        }
        Ok(())
    }
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

impl std::fmt::Display for Attr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Attr::InputPort => write!(f, "input")?,
            Attr::OutputPort => write!(f, "output")?,
            Attr::Prev(path) => write!(f, "prev {path}")?,
            Attr::Peek(path) => write!(f, "peek {path}")?,
            Attr::Init(value) => write!(f, "init {value}")?,
            Attr::Gate(gate_fn, paths) => write!(f, "gate {gate_fn}({})", paths.join(", "))?,
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Shape(pub String, pub Vec<ShapeParam>);

#[derive(Debug, Clone)]
pub enum ShapeParam {
    Nat(u64),
    Shape(Box<Shape>),
}
