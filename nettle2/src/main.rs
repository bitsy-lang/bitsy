#![allow(dead_code)]
use std::collections::HashMap;
use std::sync::Arc;

type Terminal = String;

#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy, Default)]
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
    Add(Box<Expr>, Box<Expr>),
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
            Expr::Add(e1, e2) => {
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
            Expr::Add(e1, e2) => Expr::Add(Box::new(e1.relative_to(top)), Box::new(e2.relative_to(top))),
        }
    }

    pub fn eval(&self, nettle: &Nettle) -> Value {
        match self {
            Expr::Terminal(terminal) => nettle.peek(terminal),
            Expr::Lit(value) => *value,
            Expr::Add(e1, e2) => {
                match (e1.eval(nettle), e2.eval(nettle)) {
                    (Value::Word(a), Value::Word(b)) => Value::Word(a + b),
                    _ => Value::X,
                }
            }
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
    state: HashMap<Terminal, TerminalState>,
    wires: HashMap<Terminal, Expr>,
    indent: usize,
}

impl Nettle {
    pub fn new() -> Nettle {
        Nettle {
            state: HashMap::new(),
            wires: HashMap::new(),
            indent: 0,
        }
    }

    pub fn terminals(&self) -> Vec<Terminal> {
        self.state.keys().cloned().collect()
    }

    pub fn add_node(&mut self, terminal: &Terminal) {
        assert!(!self.terminals().contains(terminal));
        self.state.insert(terminal.clone(), TerminalState::Node(Value::X));
    }

    pub fn add_reg(&mut self, terminal: &Terminal) {
        assert!(!self.terminals().contains(terminal));
        self.state.insert(terminal.clone(), TerminalState::Reg(Value::X, Value::X));
    }

    pub fn add_wire(&mut self, terminal: &Terminal, expr: &Expr) {
        self.wires.insert(terminal.clone(), expr.clone());
    }

    pub fn peek(&self, terminal: &Terminal) -> Value {
        assert!(self.terminals().contains(terminal));

        let value = self.state[terminal].peek();
        let padding = " ".repeat(self.indent * 4);
        eprintln!("{padding}peek({terminal}) = {:?}", value);
        value
    }

    pub fn poke(&mut self, terminal: &Terminal, value: Value) {
        let padding = " ".repeat(self.indent * 4);
        eprintln!("{padding}poke({terminal}, {value:?})");
        self.indent += 1;

        let state = self.state.get_mut(terminal).unwrap();
        state.poke(value);

        if !state.is_reg() {
            self.update(terminal);
        }

        self.indent -= 1;
    }

    pub fn set(&mut self, terminal: &Terminal, value: Value) {
        let padding = " ".repeat(self.indent * 4);
        eprintln!("{padding}set({terminal}, {value:?})");
        self.indent += 1;

        let state = self.state.get_mut(terminal).unwrap();
        state.set(value);
        self.update(terminal);

        self.indent -= 1;
    }

    fn update(&mut self, terminal: &Terminal) {
        let padding = " ".repeat(self.indent * 4);
        eprintln!("{padding}update({terminal})");
        self.indent += 1;

        let wires = self.wires.clone();
        for (target_terminal, expr) in &wires {
            if expr.depends_on(terminal) {
                eprintln!("{padding}affected: {target_terminal}");
                let value = expr.eval(&self);
                self.poke(target_terminal, value);
            }
        }

        self.indent -= 1;
    }

    pub fn clock(&mut self) {
        let padding = " ".repeat(self.indent * 4);
        eprintln!("{padding}clock()");
        self.indent += 1;

        for terminal in self.terminals() {
            let state = self.state.get_mut(&terminal).unwrap();
            if state.is_reg() {
                let padding = " ".repeat(self.indent * 4);
                eprintln!("{padding}register clocked: {terminal} {state:?}");
                state.clock();
            }
        }

        for reg in self.terminals() {
            self.update(&reg);
        }

        self.indent -= 1;
    }
}

#[derive(Debug, Clone)]
enum TerminalType {
    Node,
    Reg,
}

#[derive(Debug)]
pub struct CircuitDef {
    terminals: HashMap<Terminal, TerminalType>,
    wires: HashMap<Terminal, Expr>,
    path: Vec<String>,
}

#[derive(Debug)]
pub struct Circuit(Arc<CircuitDef>);


impl Circuit {
    pub fn new() -> Circuit {
        Circuit {
            terminals: HashMap::new(),
            wires: HashMap::new(),
            path: vec!["top".to_string()],
        }
    }
}

impl CircuitDef {
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

    pub fn instantiate(mut self, name:  &str, circuit: &Circuit) -> Self {
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
}

pub trait TermLookup {
    fn terminal(&self, name: &str) -> Expr;
}

impl TermLookup for Circuit {
    fn terminal(&self, name: &str) -> Expr {
        Expr::Terminal(Circuit::terminal(self, name))
    }
}

fn main() {
    let counter =
        Circuit::new()
            .node("out")
            .reg("state")
            .wire("out", |c| c.terminal("state"))
            .wire("state", |c| Expr::Add(Box::new(c.terminal("state")), Box::new(Expr::Lit(1.into()).into())));

    let top =
        Circuit::new()
            .node("in")
            .node("out")
            .wire("out", |c| c.terminal("counter.out"))
            .instantiate("counter1", &counter)
            .instantiate("counter2", &counter);

    println!("{top:#?}");
}

#[test]
fn buffer() {
    let mut nettle = Nettle::new();

    let term_in = &"in".to_string();
    let term_out = &"out".to_string();
    let term_r = &"r".to_string();

    nettle.add_node(term_in);
    nettle.add_node(term_out);
    nettle.add_reg(term_r);

    nettle.add_wire(&term_r, &Expr::Terminal(term_in.clone()));
    nettle.add_wire(&term_out, &Expr::Terminal(term_r.clone()));

    nettle.poke(term_in, true.into());
    assert_eq!(nettle.peek(term_r), Value::X);
    assert_eq!(nettle.peek(term_out), Value::X);

    nettle.clock();
    assert_eq!(nettle.peek(term_r), true.into());
    assert_eq!(nettle.peek(term_out), true.into());
}

#[test]
fn counter() {
    let mut nettle = Nettle::new();

    let term_out = &"out".to_string();
    let term_r = &"r".to_string();

    nettle.add_node(term_out);
    nettle.add_reg(term_r);
    nettle.add_wire(term_out, &Expr::Terminal(term_r.clone()));
    let expr = &Expr::Add(
        Box::new(Expr::Terminal(term_r.clone())),
        Box::new(Expr::Lit(1.into())),
    );
    nettle.add_wire(term_r, expr);

    nettle.set(term_r, 0.into());

    for i in 0..16 {
        assert_eq!(nettle.peek(term_out), i.into());
        nettle.clock();
    }
}

#[test]
fn triangle_numbers() {
    let mut nettle = Nettle::new();

    let top_out = &"top.out".to_string();
    let top_sum = &"top.sum".to_string();
    let top_counter_out = &"top.counter.out".to_string();
    let top_counter_counter = &"top.counter.counter".to_string();

    nettle.add_node(top_out);
    nettle.add_reg(top_sum);
    nettle.add_node(top_counter_out);
    nettle.add_reg(top_counter_counter);

    nettle.add_wire(top_out, &Expr::Terminal(top_sum.clone()));
    nettle.add_wire(
        top_sum,
        &Expr::Add(
            Box::new(Expr::Terminal(top_sum.clone())),
            Box::new(Expr::Terminal(top_counter_out.clone())),
        )
    );
    nettle.add_wire(top_counter_out, &Expr::Terminal(top_counter_counter.clone()));
    nettle.add_wire(
        top_counter_counter,
        &Expr::Add(
            Box::new(Expr::Terminal(top_counter_counter.clone())),
            Box::new(Expr::Lit(1.into())),
        )
    );

    nettle.set(top_sum, 0.into());
    nettle.set(top_counter_counter, 0.into());
    nettle.clock();

    for i in 0..16 {
        let triange = (i * (i + 1)) / 2;
        assert_eq!(nettle.peek(top_out), triange.into());
        nettle.clock();
    }
}
