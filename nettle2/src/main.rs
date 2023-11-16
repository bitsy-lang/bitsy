#![allow(dead_code)]
use std::collections::HashMap;

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

#[derive(Debug)]
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

fn main() {
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

    println!("{nettle:#?}");
}
