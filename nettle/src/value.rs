type Width = u64;

#[derive(Eq, PartialEq, Clone, Copy)]
pub enum Type {
    Word(Width),
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Clone, Copy, Default)]
pub enum Value {
    #[default]
    X,
    Bit(bool),
    Word(Width, u64),
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Type::Word(1) => write!(f, "Bit"),
            Type::Word(8) => write!(f, "Byte"),
            Type::Word(n) => write!(f, "Word<{n}>"),
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Value::X => write!(f, "X"),
            Value::Bit(b) => write!(f, "{b}"),
            Value::Word(w, n) => write!(f, "{n}w{w}"),
        }
    }
}

impl From<bool> for Value {
    fn from(x: bool) -> Value {
        Value::Bit(x)
    }
}
