pub type ComponentName = String;
pub type PortName = String;

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

#[derive(Debug, Clone, Copy)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum ShapeParam {
    Nat(u64),
    Shape(ShapeRef),
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct ShapeRef(pub String, pub Vec<ShapeParam>);
#[derive(Eq, PartialEq, Clone, Hash, PartialOrd, Ord, Debug)]
pub struct TerminalRef(pub String, pub String);
#[derive(Eq, PartialEq, Clone, Hash, PartialOrd, Ord, Debug)]
pub struct DomainRef(pub String);
#[derive(Eq, PartialEq, Clone, Hash, PartialOrd, Ord, Debug)]
pub struct ModDefRef(pub String);
#[derive(Eq, PartialEq, Clone, Hash, PartialOrd, Ord, Debug)]
pub struct GateRef(pub String);

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
