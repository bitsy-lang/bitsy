pub type ComponentName = String;
pub type PortName = String;
pub type FieldName = String;
pub type CtorName = String;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Polarity {
    Sink,
    Source,
}

impl std::fmt::Display for Polarity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Polarity::Sink => write!(f, "-"),
            Polarity::Source => write!(f, "+"),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Direction {
    Incoming,
    Outgoing,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum ShapeArg {
    Nat(u64),
    Shape(ShapeRef),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Kind {
    Nat,
    Shape,
    Reference,
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct ShapeRef(pub String, pub Vec<ShapeArg>);
#[derive(Eq, PartialEq, Clone, Hash, PartialOrd, Ord, Debug)]
pub struct TerminalRef(pub String, pub String);
#[derive(Eq, PartialEq, Clone, Hash, PartialOrd, Ord, Debug)]
pub struct DomainRef(pub String);
#[derive(Eq, PartialEq, Clone, Hash, PartialOrd, Ord, Debug)]
pub struct ModDefRef(pub String);
#[derive(Eq, PartialEq, Clone, Hash, PartialOrd, Ord, Debug)]
pub struct GateRef(pub String);

impl Default for DomainRef {
    fn default() -> DomainRef {
        DomainRef("d".to_string())
    }
}

impl std::fmt::Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Kind::Nat => write!(f, "Nat"),
            Kind::Shape => write!(f, "Shape"),
            Kind::Reference => write!(f, "Reference"),
        }
    }
}

impl GateRef {
    pub fn name(&self) -> &str {
        &self.0
    }
}

impl ShapeRef {
    pub fn shape_family_name(&self) -> &str {
        &self.0
    }

    pub fn from(text: &str) -> ShapeRef {
        use crate::parser::ShapeParser;
        let parser = ShapeParser::new();
        parser.parse(text).unwrap()
    }

    pub fn internal_shape_refs(&self) -> Vec<ShapeRef> {
        let mut results = vec![];
        for arg in &self.1 {
            match arg {
                ShapeArg::Nat(_n) => (),
                ShapeArg::Shape(shape_ref) => {
                    results.push(shape_ref.clone());
                    results.extend_from_slice(&shape_ref.internal_shape_refs());
                }
            }
        }
        results
    }
}

impl std::fmt::Display for ModDefRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)
    }
}

impl std::fmt::Display for TerminalRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)?;
        std::fmt::Display::fmt(&self.1, f)
    }
}

impl std::fmt::Display for DomainRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)
    }
}

impl std::fmt::Display for GateRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)
    }
}

impl TerminalRef {
    pub fn component(&self) -> &ComponentName {
        &self.0
    }

    pub fn port(&self) -> &PortName {
        &self.1
    }
}

#[derive(Debug, Clone)]
pub enum MatchPattern {
    Ctor(String, Vec<Box<MatchPattern>>),
    Var(String),
    Lit(Value),
    Otherwise,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Unknown,
    Unobservable,
    Bit(bool),
    Word(u64),
    Tuple(Vec<Box<Value>>),
    Struct(Vec<(FieldName, Box<Value>)>),
    Component(Box<crate::defs::Type>),
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
            Value::Struct(field_vals) => {
                write!(f, "${{")?;
                for (i, (field_name, val)) in field_vals.iter().enumerate() {
                    write!(f, "{field_name} = {val}")?;
                    if i + 1 < field_vals.len() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "}}")?;
            },
            Value::Component(_typ) => write!(f, "(*component*)")?,
        }
        Ok(())
    }
}
