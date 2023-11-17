#![allow(dead_code)]

use lalrpop_util::lalrpop_mod;
use std::collections::BTreeMap;
use std::sync::Arc;

lalrpop_mod!(pub parser);

type Terminal = String;
type Width = u64;

#[derive(Ord, PartialOrd, Eq, PartialEq, Clone, Copy, Default)]
pub enum Value {
    #[default]
    X,
    Bit(bool),
    Word(Width, u64),
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

#[derive(Eq, PartialEq, Clone)]
pub enum Expr {
    Terminal(Terminal),
    Lit(Value),
    //UnOp(UnOp, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Expr::Terminal(terminal) => write!(f, "{terminal}"),
            Expr::Lit(val) => write!(f, "{val:?}"),
            Expr::BinOp(op, e1, e2) => {
                let op_symbol = match op {
                    BinOp::Add => "+",
                    BinOp::Sub => "-",
                    BinOp::And => "&",
                    BinOp::Or => "|",
                };
                write!(f, "({e1:?} {op_symbol} {e2:?})")
            },
        }
    }
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

fn parent_of(terminal: &Terminal) -> Terminal {
    let mut path_parts: Vec<&str> = terminal.split('.').collect();
    path_parts.pop();
    path_parts.join(".")
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
                    (BinOp::Add, Value::Word(w, a), Value::Word(_w, b)) => Value::Word(w, a.wrapping_add(b) % pow2(w)),
                    (BinOp::Sub, Value::Word(w, a), Value::Word(_w, b)) => Value::Word(w, a.wrapping_sub(b) % pow2(w)),
                    (BinOp::And, Value::Word(w, a), Value::Word(_w, b)) => Value::Word(w, a & b),
                    (BinOp::Or, Value::Word(w, a), Value::Word(_w, b)) => Value::Word(w, a | b),
                    _ => Value::X,
                }
            },
        }
    }
}

fn pow2(n: u64) -> u64 {
    1 << n
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
            TerminalState::Reg(_set, _val) => true,
            _ => false,
        }
    }
}

#[allow(unused_variables)]
pub trait ExtInstance: std::fmt::Debug {
    fn peek(&mut self, port: &str) -> Value { panic!(); }
    fn poke(&mut self, port: &str, value: Value) -> Vec<&str> { panic!(); }
    fn clock(&mut self) {}
}

pub struct Nettle {
    circuit: Module,
    state: BTreeMap<Terminal, TerminalState>,
    exts: BTreeMap<Terminal, Box<dyn ExtInstance>>,
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
            exts: BTreeMap::new(),
            indent: 0,
            debug: false,
        }
    }

    pub fn ext(mut self, terminal: &str, ext_inst: Box<dyn ExtInstance>) -> Self {
        self.exts.insert(terminal.to_string(), ext_inst);
        self
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

        let ext_path = parent_of(terminal);
        let value = self.peek(terminal);
        if let Some(ext) = self.exts.get_mut(&ext_path) {
            let local_path = &terminal[ext_path.len() + 1..];
            ext.poke(local_path, value);
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

        for (path, ext) in &mut self.exts {
            ext.clock();
            if self.debug {
                let padding = " ".repeat(self.indent * 4);
                eprintln!("{padding}ext clocked: {path}");
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

impl std::fmt::Debug for Nettle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        writeln!(f)?;
        writeln!(f, "State:")?;
        let mut states: Vec<(_, _)> = self.state.iter().collect();
        states.sort_by_key(|(terminal, _)| terminal.to_string());
        states = states.into_iter().rev().collect();
        for (terminal, state) in states {
            match state {
                TerminalState::Node(val) => writeln!(f, "    {:>4}  {terminal}", format!("{val:?}"))?,
                TerminalState::Reg(set, val) => {
                    writeln!(f, "    {:>4}  {terminal}.set", format!("{set:?}"))?;
                    writeln!(f, "    {:>4}  {terminal}.val", format!("{val:?}"))?;
                },
            }
        }
        writeln!(f, "Wires:")?;
        for (terminal, expr) in &self.circuit.wires {
            writeln!(f, "    {terminal:<25} <= {expr:?}")?;
        }

        Ok(())
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
    exts: Vec<Terminal>,
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
            exts: vec![],
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

    pub fn ext(mut self, name: &str, terminals: &[&str]) -> Self {
        let ext = self.terminal(name);
        self.exts.push(ext.clone());

        for terminal in terminals {
            let target = format!("{ext}.{terminal}");
            self.terminals.insert(target, TerminalType::Node);
        }
        self
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
            .wire("state", |c| Expr::BinOp(BinOp::Add, Box::new(c.terminal("state")), Box::new(Expr::Lit(Value::Word(4, 1)).into())))
            .build();

    let top =
        Module::new()
            .node("out")
            .wire("out", |c| c.terminal("counter1.out"))
            .instantiate("counter1", &counter)
            .instantiate("counter2", &counter)
            .build();

    let mut nettle = Nettle::new(&top);
    nettle.set("top.counter1.state", Value::Word(4, 0));
    nettle.set("top.counter2.state", Value::Word(4, 0));

    dbg!(&nettle);
    nettle.clock();
    dbg!(&nettle);
    nettle.clock();
    dbg!(&nettle);
    nettle.clock();
    dbg!(&nettle);
    nettle.clock();
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
                      Box::new(Expr::Lit(Value::Word(4, 1))),
                  )
            )
            .build();

    let mut nettle = Nettle::new(&counter);

    nettle.set("top.counter", Value::Word(4, 0));

    for i in 0..16 {
        assert_eq!(nettle.peek("top.out"), Value::Word(4, i));
        nettle.clock();
    }
    assert_eq!(nettle.peek("top.out"), Value::Word(4, 0));
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
                      Box::new(Expr::Lit(Value::Word(4, 1))),
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

    nettle.set("top.sum", Value::Word(32, 0));
    nettle.set("top.counter.counter", Value::Word(32, 0));
    nettle.clock();

    for i in 0..16 {
        let triange = (i * (i + 1)) / 2;
        assert_eq!(nettle.peek("top.out"), Value::Word(32, triange));
        nettle.clock();
    }
}

#[derive(Debug)]
struct Monitor(Option<String>);

impl Monitor {
  pub fn new() -> Monitor {
      Monitor(None)
  }
}

impl ExtInstance for Monitor {
    fn poke(&mut self, _port: &str, value: Value) -> Vec<&str> {
        self.0 = Some(format!("{value:?}"));
        vec![]
    }

    fn clock(&mut self) {
        if let Some(s) = &self.0 {
            println!("{s}");
            self.0 = None
        }
    }
}

#[test]
fn vip() {
    let counter =
        Module::new()
            .node("out")
            .reg("counter")
            .wire("out", |c| c.terminal("counter"))
            .wire("counter", |c|
                  Expr::BinOp(
                      BinOp::Add,
                      Box::new(c.terminal("counter")),
                      Box::new(Expr::Lit(Value::Word(4, 1))),
                  )
            )
            .build();

    let top =
        Module::new()
            .instantiate("counter", &counter)
            .ext("vip", &["in"])
            .wire("vip.in", |c| c.terminal("counter.out"))
            .build();

    let monitor = Box::new(Monitor::new());

    let mut nettle =
        Nettle::new(&top)
            .ext("top.vip", monitor);

    nettle.set("top.counter.counter", Value::Word(4, 0));
    nettle.clock();
    nettle.clock();
    nettle.clock();
    nettle.clock();
}

impl From<&str> for Expr {
    fn from(expr: &str) -> Expr {
        *parser::ExprParser::new().parse(expr).unwrap()
    }
}

#[test]
fn test_parse() {
    let exprs = vec![
        "x",
        "1w8",
        "true",
        "false",
        "X",
        "(a + b)",
    ];
    for e in exprs {
        let expr: Expr = e.into();
        assert_eq!(format!("{:?}", expr), e);
    }
}
