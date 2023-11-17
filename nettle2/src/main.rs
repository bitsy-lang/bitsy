#![allow(dead_code)]

use std::collections::BTreeMap;
use std::sync::Arc;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(parse);

#[derive(Debug)]
pub struct Testbench(Vec<TestbenchCommand>);

#[derive(Debug)]
pub enum TestbenchCommand {
    Peek(Terminal),
    Poke(Terminal, Value),
    Set(Terminal, Value),
    Clock,
    Reset,
    Debug,
}

#[derive(Debug)]
struct Mod(String, Vec<ModDecl>);
#[derive(Debug)]
enum ModDecl {
    Node(String),
    Reg(String),
    Mod(Mod),
    Wire(Terminal, Expr),
    Ext(String, Vec<String>),
}
#[derive(Debug)]
struct Ext(String, Vec<String>);

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
    UnOp(UnOp, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Hole(Option<String>),
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Expr::Terminal(terminal) => write!(f, "{terminal}"),
            Expr::Lit(val) => write!(f, "{val:?}"),
            Expr::UnOp(op, e) => {
                let op_symbol = match op {
                    UnOp::Not => "!",
                };
                write!(f, "({op_symbol}{e:?})")
            },
            Expr::BinOp(op, e1, e2) => {
                let op_symbol = match op {
                    BinOp::Add => "+",
                    BinOp::Sub => "-",
                    BinOp::And => "&&",
                    BinOp::Or => "||",
                    BinOp::Eq => "==",
                    BinOp::Neq => "!=",
                    BinOp::Lt => "<",
                };
                write!(f, "({e1:?} {op_symbol} {e2:?})")
            },
            Expr::If(cond, e1, e2) => {
                write!(f, "if {cond:?} {{ {e1:?} }} else {{ {e2:?} }}")
            },
            Expr::Hole(opt_name) => {
                if let Some(_name) = opt_name {
                    todo!()
                } else {
                    write!(f, "?")
                }
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum UnOp {
    Not,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum BinOp {
    Add,
    Sub,
    And,
    Or,
    Eq,
    Neq,
    Lt,
}

fn relative_to(top: &Terminal, terminal: &Terminal) -> Terminal {
    if terminal.starts_with("top.") {
        format!("{}.{}", top, &terminal[4..])
    } else {
        format!("{}.{}", top, &terminal)
    }
}

fn parent_of(terminal: &Terminal) -> Terminal {
    let mut path_parts: Vec<&str> = terminal.split('.').collect();
    path_parts.pop();
    path_parts.join(".")
}

#[test]
fn relative_to_test() {
    assert_eq!(relative_to(&"top.foo".to_string(), &"top.bar".to_string()), "top.foo.bar".to_string());
    assert_eq!(relative_to(&"top.foo".to_string(), &"bar".to_string()), "top.foo.bar".to_string());
}

impl Expr {
    pub fn terminals(&self) -> Vec<Terminal> {
        match self {
            Expr::Terminal(terminal) => vec![terminal.clone()],
            Expr::Lit(_value) => vec![],
            Expr::UnOp(_op, e) => {
                let mut result = e.terminals();
                result.sort();
                result.dedup();
                result
            }
            Expr::BinOp(_op, e1, e2) => {
                let mut result = e1.terminals();
                result.extend(e2.terminals());
                result.sort();
                result.dedup();
                result
            },
            Expr::If(cond, e1, e2) => {
                let mut result = cond.terminals();
                result.extend(e1.terminals());
                result.extend(e2.terminals());
                result.sort();
                result.dedup();
                result
            },
            Expr::Hole(_name) => vec![],
        }
    }

    pub fn is_constant(&self) -> bool {
        self.terminals().is_empty()
    }

    pub fn depends_on(&self, terminal: &Terminal) -> bool {
        self.terminals().contains(terminal)
    }

    pub fn relative_to(self, top: &Terminal) -> Expr {
        match self {
            Expr::Terminal(terminal) => Expr::Terminal(relative_to(top, &terminal)),
            Expr::Lit(_value) => self,
            Expr::UnOp(op, e) => Expr::UnOp(op, Box::new(e.relative_to(top))),
            Expr::BinOp(op, e1, e2) => Expr::BinOp(op, Box::new(e1.relative_to(top)), Box::new(e2.relative_to(top))),
            Expr::If(cond, e1, e2) => Expr::If(Box::new(cond.relative_to(top)), Box::new(e1.relative_to(top)), Box::new(e2.relative_to(top))),
            Expr::Hole(name) => Expr::Hole(name),
        }
    }

    pub fn eval(&self, nettle: &Nettle) -> Value {
        match self {
            Expr::Terminal(terminal) => nettle.peek(terminal),
            Expr::Lit(value) => *value,
            Expr::UnOp(op, e) => {
                match (op, e.eval(nettle)) {
                    (UnOp::Not, Value::Bit(b)) => Value::Bit(!b),
                    _ => Value::X,
                }
            },
            Expr::BinOp(op, e1, e2) => {
                match (op, e1.eval(nettle), e2.eval(nettle)) {
                    (BinOp::Add, Value::Word(w, a),  Value::Word(_w, b)) => Value::Word(w, a.wrapping_add(b) % pow2(w)),
                    (BinOp::Sub, Value::Word(w, a),  Value::Word(_w, b)) => Value::Word(w, a.wrapping_sub(b) % pow2(w)),
                    (BinOp::And, Value::Word(w, a),  Value::Word(_w, b)) => Value::Word(w, a & b),
                    (BinOp::Or,  Value::Word(w, a),  Value::Word(_w, b)) => Value::Word(w, a | b),
                    (BinOp::Eq,  Value::Word(_w, a), Value::Word(_v, b)) => Value::Bit(a == b),
                    (BinOp::Neq, Value::Word(_w, a), Value::Word(_v, b)) => Value::Bit(a != b),
                    (BinOp::Eq,  Value::Bit(b1),     Value::Bit(b2)) => Value::Bit(b1 == b2),
                    (BinOp::Neq, Value::Bit(b1),     Value::Bit(b2)) => Value::Bit(b1 == b2),
                    _ => Value::X,
                }
            },
            Expr::If(cond, e1, e2) => {
                match cond.eval(nettle) {
                    Value::Bit(true) => e1.eval(nettle),
                    Value::Bit(false) => e2.eval(nettle),
                    _ => Value::X,
                }
            },
            Expr::Hole(opt_name) => {
                match opt_name {
                    Some(name) => panic!("EVALUATED A HOLE: ?{name}"),
                    None => panic!("EVALUATED A HOLE"),
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
        let mut nettle = Nettle {
            circuit: circuit.clone(),
            state,
            exts: BTreeMap::new(),
            indent: 0,
            debug: false,
        };
        nettle.update_constants();
        nettle
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

    fn update_constants(&mut self) {
        if self.debug {
            let padding = " ".repeat(self.indent * 4);
            eprintln!("{padding}update_constants()");
            self.indent += 1;
        }

        let wires = self.wires().clone();
        for (target_terminal, expr) in &wires {
            if expr.is_constant() {
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

    pub fn wire(mut self, name: &str, expr: &Expr) -> Self {
        let terminal = self.terminal(name);
        self.wires.insert(terminal, expr.clone().relative_to(&self.path()));
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

fn mod_to_module(m: Mod) -> ModuleDef {
    let Mod(_name, decls) = m;
    let mut module = Module::new();

    for decl in decls {
        match decl {
            ModDecl::Node(name) => module = module.node(&name),
            ModDecl::Reg(name) => module = module.reg(&name),
            ModDecl::Mod(subodule) => {
                let name = subodule.0.to_string();
                module = module.instantiate(&name, &mod_to_module(subodule))
            },
            ModDecl::Wire(terminal, expr) => module = module.wire(&terminal, &expr),
            ModDecl::Ext(name, terminals) => {
                let terminals: Vec<_> = terminals.iter().map(|s| s.as_str()).collect();
                module = module.ext(&name, &terminals)
            },
        }
    }
    module
}

pub fn parse_mod(circuit: &str) -> Module {
    let m = parse::ModParser::new().parse(circuit).unwrap();
    mod_to_module(m).build()
}

pub fn parse_testbench(testbench: &str) -> Testbench {
    parse::TestbenchParser::new().parse(testbench).unwrap()
}

fn read_testbench_file(filename: &str) -> Testbench {
    let text = std::fs::read_to_string(filename).unwrap();
    parse_testbench(&text)
}

fn main() {
    let argv: Vec<String> = std::env::args().collect();
    let default = "Top.nettle".to_string();
    let filename = argv.get(1).unwrap_or(&default);
    let text = std::fs::read_to_string(filename).unwrap();

    let tb = read_testbench_file("Top.tb");

    let top = parse_mod(&text);
    let monitor = Box::new(Monitor::new());

    let mut nettle =
        Nettle::new(&top)
            .ext("top.vip", monitor);

    for command in &tb.0 {
        match command {
            TestbenchCommand::Peek(terminal) => {
                println!("PEEK {terminal} => {:?}", nettle.peek(terminal));
            },
            TestbenchCommand::Poke(terminal, value) => {
                println!("POKE {terminal} <= {value:?}");
                nettle.poke(terminal, *value);
            },
            TestbenchCommand::Set(terminal, value) => {
                println!("SET {terminal} = {value:?}");
                nettle.set(terminal, *value);
            },
            TestbenchCommand::Clock => {
                println!("CLOCK");
                nettle.clock();
            }
            TestbenchCommand::Reset => {
                // nettle.reset();
                println!("RESET");
                todo!();
            }
            TestbenchCommand::Debug => {
                println!("DEBUG");
                println!("{nettle:#?}");
            }
        }
    }
}

#[test]
fn buffer() {
    let buffer = parse_mod("
        mod buffer {
            node in;
            reg r;
            node out;
            r <= in;
            out <= r;
        }
    ");

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
    let counter = parse_mod("
        mod counter {
            node out;
            reg counter;
            out <= counter;
            counter <= counter + 1w4;
        }
    ");

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
    let top = parse_mod("
        mod top {
            node out;
            reg sum;
            mod counter {
                node out;
                reg counter;
                out <= counter;
                counter <= counter + 1w4;
            }
            out <= sum;
            sum <= sum + counter.out;
        }
    ");

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
    let top = parse_mod("
        mod top {
            mod counter {
                node out;
                reg counter;
                counter <= counter + 1w4;
                out <= counter;
            }

            ext vip {
                node in;
            }

            vip.in <= counter.out;
        }
    ");

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

#[test]
fn ifs() {
    let top = parse_mod("
        mod top {
            node out;
            node in;

            out <= if in {
                42w8
            } else {
                100w8
            };
        }
    ");

    let mut nettle = Nettle::new(&top);

    nettle.poke("top.in", Value::Bit(true));
    assert_eq!(nettle.peek("top.out"), Value::Word(8, 42));
    nettle.poke("top.in", Value::Bit(false));
    assert_eq!(nettle.peek("top.out"), Value::Word(8, 100));
}

impl From<&str> for Expr {
    fn from(expr: &str) -> Expr {
        *parse::ExprParser::new().parse(expr).unwrap()
    }
}

#[test]
fn test_parse() {
    let exprs = vec![
        "x",
        "x.y",
        "1w8",
        "true",
        "false",
        "X",
        "(a + b)",
        "(a && b)",
        "(a || b)",
        "(a == b)",
        "(a != b)",
        "(!a)",
        "if c { e1 } else { e2 }",
    ];
    for e in exprs {
        let expr: Expr = e.into();
        assert_eq!(format!("{:?}", expr), e);
    }
}
