#![allow(dead_code)]
use std::collections::BTreeMap;
use std::sync::Arc;

type Terminal = String;

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone, Copy, Default)]
pub enum Value {
    #[default]
    X,
    Bit(bool),
    Word(u64),
}

impl From<bool> for Value {
    fn from(x: bool) -> Value {
        Value::Bit(x)
    }
}

impl From<u64> for Value {
    fn from(x: u64) -> Value {
        Value::Word(x)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expr {
    Terminal(Terminal),
    Lit(Value),
    //UnOp(UnOp, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum UnOp {
    Not,
    Inc,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum BinOp {
    Add,
    Sub,
    And,
    Or,
}

fn relative_to(top: &Terminal, terminal: &Terminal) -> Terminal {
    assert!(terminal.starts_with("top."));
    format!("{}.{}", top, &terminal[4..])
}

#[test]
fn relative_to_test() {
    assert_eq!(relative_to(&"top.foo".to_string(), &"top.bar".to_string()), "top.foo.bar".to_string())
}

impl Expr {
    pub fn terminals(&self) -> Vec<Terminal> {
        match self {
            Expr::Terminal(terminal) => vec![terminal.clone()],
            Expr::Lit(_value) => vec![],
            Expr::BinOp(_op, e1, e2) => {
                let mut result = e1.terminals();
                result.extend(e2.terminals());
                result.sort();
                result.dedup();
                result
            },
        }
    }

    pub fn depends_on(&self, terminal: &Terminal) -> bool {
        self.terminals().contains(terminal)
    }

    pub fn relative_to(self, top: &Terminal) -> Expr {
        match self {
            Expr::Terminal(terminal) => Expr::Terminal(relative_to(top, &terminal)),
            Expr::Lit(_value) => self,
            Expr::BinOp(op, e1, e2) => Expr::BinOp(op, Box::new(e1.relative_to(top)), Box::new(e2.relative_to(top))),
        }
    }

    pub fn eval(&self, nettle: &Nettle) -> Value {
        match self {
            Expr::Terminal(terminal) => nettle.peek(terminal),
            Expr::Lit(value) => *value,
            Expr::BinOp(op, e1, e2) => {
                match (op, e1.eval(nettle), e2.eval(nettle)) {
                    (BinOp::Add, Value::Word(a), Value::Word(b)) => Value::Word(a + b),
                    (BinOp::Sub, Value::Word(a), Value::Word(b)) => Value::Word(a - b),
                    (BinOp::And, Value::Word(a), Value::Word(b)) => Value::Word(a & b),
                    (BinOp::Or, Value::Word(a), Value::Word(b)) => Value::Word(a | b),
                    _ => Value::X,
                }
            },
        }
    }
}

#[derive(Debug, Clone)]
enum TerminalState {
    Node(Value),
    Reg(Value, Value),
}

impl TerminalState {
    fn peek(&self) -> Value {
        match self {
            TerminalState::Node(val) => *val,
            TerminalState::Reg(_set, val) => *val,
        }
    }

    fn poke(&mut self, value: Value) {
        match self {
            TerminalState::Node(val) => *val = value,
            TerminalState::Reg(set, _val) => *set = value,
        }
    }

    fn set(&mut self, value: Value) {
        match self {
            TerminalState::Node(_val) => panic!(),
            TerminalState::Reg(_set, val) => *val = value,
        }
    }

    fn clock(&mut self) {
        if let TerminalState::Reg(set, val) = self {
            *val = *set;
            *set = Value::X;
        }
    }

    fn is_reg(&self) -> bool {
        match *self {
            TerminalState::Node(_val) => false,
            TerminalState::Reg(_set, _val) => true,
        }
    }
}

#[derive(Debug)]
pub struct Nettle {
    circuit: Module,
    state: BTreeMap<Terminal, TerminalState>,
    indent: usize,
    debug: bool,
}

impl Nettle {
    pub fn new(circuit: &Module) -> Nettle {
        let mut state = BTreeMap::new();
        for (terminal, typ) in &circuit.terminals {
            let terminal_state = match typ {
                TerminalType::Node => TerminalState::Node(Value::X),
                TerminalType::Reg => TerminalState::Reg(Value::X, Value::X),
            };
            state.insert(terminal.to_string(), terminal_state);
        }
        Nettle {
            circuit: circuit.clone(),
            state,
            indent: 0,
            debug: false,
        }
    }

    fn wires(&self) -> &BTreeMap<Terminal, Expr> {
        &self.circuit.wires
    }

    pub fn terminals(&self) -> Vec<Terminal> {
        self.state.keys().cloned().collect()
    }

    pub fn peek(&self, terminal: &str) -> Value {
        assert!(self.terminals().contains(&terminal.to_string()));
        let value = self.state[terminal].peek();
        if self.debug {
            let padding = " ".repeat(self.indent * 4);
            eprintln!("{padding}peek({terminal}) = {:?}", value);
        }
        value
    }

    pub fn poke(&mut self, terminal: &str, value: Value) {
        if self.debug {
            let padding = " ".repeat(self.indent * 4);
            eprintln!("{padding}poke({terminal}, {value:?})");
            self.indent += 1;
        }

        let state = self.state.get_mut(terminal).unwrap();
        state.poke(value);

        if !state.is_reg() {
            self.update(&terminal.to_string());
        }

        if self.debug {
            self.indent -= 1;
        }
    }

    pub fn set(&mut self, terminal: &str, value: Value) {
        if self.debug {
            let padding = " ".repeat(self.indent * 4);
            eprintln!("{padding}set({terminal}, {value:?})");
            self.indent += 1;
        }

        let state = self.state.get_mut(terminal).unwrap();
        state.set(value);
        self.update(&terminal.to_string());

        if self.debug {
            self.indent -= 1;
        }
    }

    fn update(&mut self, terminal: &Terminal) {
        if self.debug {
            let padding = " ".repeat(self.indent * 4);
            eprintln!("{padding}update({terminal})");
            self.indent += 1;
        }

        let wires = self.wires().clone();
        for (target_terminal, expr) in &wires {
            if expr.depends_on(terminal) {
                if self.debug {
                    let padding = " ".repeat(self.indent * 4);
                    eprintln!("{padding}affected: {target_terminal}");
                }
                let value = expr.eval(&self);
                self.poke(target_terminal, value);
            }
        }

        if self.debug {
            self.indent -= 1;
        }
    }

    pub fn clock(&mut self) {
        if self.debug {
            let padding = " ".repeat(self.indent * 4);
            eprintln!("{padding}clock()");
            self.indent += 1;
        }

        for terminal in self.terminals() {
            let state = self.state.get_mut(&terminal).unwrap();
            if state.is_reg() {
                if self.debug {
                    let padding = " ".repeat(self.indent * 4);
                    eprintln!("{padding}register clocked: {terminal} {state:?}");
                }
                state.clock();
            }
        }

        for reg in self.terminals() {
            self.update(&reg);
        }

        if self.debug {
            self.indent -= 1;
        }
    }
}

#[derive(Debug, Clone)]
enum TerminalType {
    Node,
    Reg,
}

#[derive(Debug)]
pub struct ModuleDef {
    terminals: BTreeMap<Terminal, TerminalType>,
    wires: BTreeMap<Terminal, Expr>,
    path: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct Module(Arc<ModuleDef>);

impl std::ops::Deref for Module {
    type Target = ModuleDef;
    fn deref(&self) -> &ModuleDef {
        &self.0
    }
}

impl Module {
    pub fn new() -> ModuleDef {
        ModuleDef {
            terminals: BTreeMap::new(),
            wires: BTreeMap::new(),
            path: vec!["top".to_string()],
        }
    }
}

impl ModuleDef {
    pub fn module(mut self, path: &str, with_module: impl FnOnce(Self) -> Self) -> Self {
        self = self.push(path);
        self = with_module(self);
        self = self.pop();
        self
    }

    fn push(mut self, path: &str) -> Self {
        self.path.push(path.to_string());
        self
    }

    fn pop(mut self) -> Self {
        self.path.pop();
        self
    }

    fn terminal(&self, name: &str) -> Terminal {
        let path = self.path.join(".");
        format!("{path}.{name}")
    }

    pub fn node(mut self, name: &str) -> Self {
        let terminal = self.terminal(name);
        self.terminals.insert(terminal, TerminalType::Node);
        self
    }

    pub fn reg(mut self, name: &str) -> Self {
        let terminal = self.terminal(name);
        self.terminals.insert(terminal, TerminalType::Reg);
        self
    }

    pub fn wire(mut self, name: &str, expr: impl FnOnce(&dyn TermLookup) -> Expr) -> Self {
        let terminal = self.terminal(name);
        self.wires.insert(terminal, expr(&self));
        self
    }

    pub fn instantiate(mut self, name:  &str, circuit: &ModuleDef) -> Self {
        self = self.push(name);
        let path = self.path();
        eprintln!("{path:?}");

        for (terminal, typ) in &circuit.terminals {
            let target = relative_to(&path, terminal);
            self.terminals.insert(target, typ.clone());
        }

        for (terminal, expr) in &circuit.wires {
            let target = relative_to(&path, terminal);
            let expr = expr.clone().relative_to(&path);
            self.wires.insert(target, expr);
        }
        self = self.pop();
        self
    }

    fn path(&self) -> Terminal {
        self.path.join(".")
    }

    pub fn build(self) -> Module {
        Module(Arc::new(self))
    }
}

pub trait TermLookup {
    fn terminal(&self, name: &str) -> Expr;
}

impl TermLookup for ModuleDef {
    fn terminal(&self, name: &str) -> Expr {
        Expr::Terminal(ModuleDef::terminal(self, name))
    }
}

fn main() {
    let counter =
        Module::new()
            .node("out")
            .reg("state")
            .wire("out", |c| c.terminal("state"))
            .wire("state", |c| Expr::BinOp(BinOp::Add, Box::new(c.terminal("state")), Box::new(Expr::Lit(1.into()).into())))
            .build();

    let top =
        Module::new()
            .node("in")
            .node("out")
            .wire("out", |c| c.terminal("counter.out"))
            .instantiate("counter1", &counter)
            .instantiate("counter2", &counter)
            .build();

    println!("{top:#?}");
}

#[test]
fn buffer() {
    let buffer =
        Module::new()
            .node("in")
            .reg("r")
            .node("out")
            .wire("r", |c| c.terminal("in"))
            .wire("out", |c| c.terminal("r"))
            .build();

    let mut nettle = Nettle::new(&buffer);

    nettle.poke("top.in", true.into());
    assert_eq!(nettle.peek("top.r"), Value::X);
    assert_eq!(nettle.peek("top.out"), Value::X);

    nettle.clock();
    assert_eq!(nettle.peek("top.r"), true.into());
    assert_eq!(nettle.peek("top.out"), true.into());
}

#[test]
fn counter() {
    let counter =
        Module::new()
            .node("out")
            .reg("counter")
            .wire("out", |c| c.terminal("counter"))
            .wire("counter", |c|
                  Expr::BinOp(
                      BinOp::Add,
                      Box::new(c.terminal("counter")),
                      Box::new(Expr::Lit(1.into())),
                  )
            )
            .build();

    let mut nettle = Nettle::new(&counter);

    nettle.set("top.counter", 0.into());

    for i in 0..16 {
        assert_eq!(nettle.peek("top.out"), i.into());
        nettle.clock();
    }
}

#[test]
fn triangle_numbers() {
    let counter =
        Module::new()
            .node("out")
            .reg("counter")
            .wire("out", |c| c.terminal("counter"))
            .wire("counter", |c|
                  Expr::BinOp(
                      BinOp::Add,
                      Box::new(c.terminal("counter")),
                      Box::new(Expr::Lit(1.into())),
                  )
            )
            .build();

    let top =
        Module::new()
            .node("out")
            .reg("sum")
            .instantiate("counter", &counter)
            .wire("out", |c| c.terminal("sum"))
            .wire("sum", |c|
                Expr::BinOp(
                    BinOp::Add,
                    Box::new(c.terminal("sum")),
                    Box::new(c.terminal("counter.out")),
                )
            )
            .build();

    let mut nettle = Nettle::new(&top);

    nettle.set("top.sum", 0.into());
    nettle.set("top.counter.counter", 0.into());
    nettle.clock();

    for i in 0..16 {
        let triange = (i * (i + 1)) / 2;
        assert_eq!(nettle.peek("top.out"), triange.into());
        nettle.clock();
    }
}
