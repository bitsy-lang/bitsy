#![allow(dead_code)]

use std::collections::BTreeMap;
use std::sync::Arc;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(parse);

#[derive(Debug)]
pub struct Testbench(Vec<TestbenchCommand>);

#[derive(Debug)]
pub enum TestbenchCommand {
    Peek(Path),
    Poke(Path, Value),
    Set(Path, Value),
    Clock,
    Reset,
    Debug,
    Assert(Expr),
}

#[derive(Debug)]
struct Mod(String, Vec<ModDecl>);
#[derive(Debug)]
enum ModDecl {
    Node(String),
    Reg(String, Value),
    Mod(Mod),
    Wire(Path, Expr),
    Ext(String, Vec<String>),
}
#[derive(Debug)]
struct Ext(String, Vec<String>);

type Path = String;
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
    Path(Path),
    Lit(Value),
    UnOp(UnOp, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Hole(Option<String>),
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Expr::Path(path) => write!(f, "{path}"),
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

fn relative_to(top: &Path, path: &Path) -> Path {
    format!("{}.{}", top, &path)
}

fn parent_of(path: &Path) -> Path {
    let mut path_parts: Vec<&str> = path.split('.').collect();
    path_parts.pop();
    path_parts.join(".")
}

impl Expr {
    pub fn paths(&self) -> Vec<Path> {
        match self {
            Expr::Path(path) => vec![path.clone()],
            Expr::Lit(_value) => vec![],
            Expr::UnOp(_op, e) => {
                let mut result = e.paths();
                result.sort();
                result.dedup();
                result
            }
            Expr::BinOp(_op, e1, e2) => {
                let mut result = e1.paths();
                result.extend(e2.paths());
                result.sort();
                result.dedup();
                result
            },
            Expr::If(cond, e1, e2) => {
                let mut result = cond.paths();
                result.extend(e1.paths());
                result.extend(e2.paths());
                result.sort();
                result.dedup();
                result
            },
            Expr::Hole(_name) => vec![],
        }
    }

    pub fn is_constant(&self) -> bool {
        self.paths().is_empty()
    }

    pub fn depends_on(&self, path: &Path) -> bool {
        self.paths().contains(path)
    }

    pub fn relative_to(self, top: &Path) -> Expr {
        match self {
            Expr::Path(path) => Expr::Path(relative_to(top, &path)),
            Expr::Lit(_value) => self,
            Expr::UnOp(op, e) => Expr::UnOp(op, Box::new(e.relative_to(top))),
            Expr::BinOp(op, e1, e2) => Expr::BinOp(op, Box::new(e1.relative_to(top)), Box::new(e2.relative_to(top))),
            Expr::If(cond, e1, e2) => Expr::If(Box::new(cond.relative_to(top)), Box::new(e1.relative_to(top)), Box::new(e2.relative_to(top))),
            Expr::Hole(name) => Expr::Hole(name),
        }
    }

    pub fn eval(&self, nettle: &Nettle) -> Value {
        match self {
            Expr::Path(path) => nettle.peek(path),
            Expr::Lit(value) => *value,
            Expr::UnOp(op, e) => {
                match (op, e.eval(nettle)) {
                    (UnOp::Not, Value::Bit(b)) => Value::Bit(!b),
                    _ => Value::X,
                }
            },
            Expr::BinOp(op, e1, e2) => {
                match (op, e1.eval(nettle), e2.eval(nettle)) {
                    (BinOp::Add, Value::Word(w, a),  Value::Word(_w, b)) => Value::Word(w, a.wrapping_add(b) % (1 << w)),
                    (BinOp::Sub, Value::Word(w, a),  Value::Word(_w, b)) => Value::Word(w, a.wrapping_sub(b) % (1 << w)),
                    (BinOp::And, Value::Word(w, a),  Value::Word(_w, b)) => Value::Word(w, a & b),
                    (BinOp::Or,  Value::Word(w, a),  Value::Word(_w, b)) => Value::Word(w, a | b),
                    (BinOp::Eq,  Value::Word(_w, a), Value::Word(_v, b)) => Value::Bit(a == b),
                    (BinOp::Neq, Value::Word(_w, a), Value::Word(_v, b)) => Value::Bit(a != b),
                    (BinOp::Eq,  Value::Bit(b1),     Value::Bit(b2)) => Value::Bit(b1 == b2),
                    (BinOp::Neq, Value::Bit(b1),     Value::Bit(b2)) => Value::Bit(b1 == b2),
                    (BinOp::And, Value::Bit(b1),     Value::Bit(b2)) => Value::Bit(b1 && b2),
                    (BinOp::Or,  Value::Bit(b1),     Value::Bit(b2)) => Value::Bit(b1 || b2),
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

    fn expand_regs_as_val(self, regs: &[String]) -> Expr {
        match self {
            Expr::Path(path) => {
                if regs.contains(&&path) {
                    Expr::Path(format!("{path}.val"))
                } else {
                    Expr::Path(path)
                }
            },
            Expr::Lit(_value) => self,
            Expr::UnOp(op, e) => Expr::UnOp(op, Box::new(e.expand_regs_as_val(regs))),
            Expr::BinOp(op, e1, e2) => Expr::BinOp(op, Box::new(e1.expand_regs_as_val(regs)), Box::new(e2.expand_regs_as_val(regs))),
            Expr::If(cond, e1, e2) => Expr::If(Box::new(cond.expand_regs_as_val(regs)), Box::new(e1.expand_regs_as_val(regs)), Box::new(e2.expand_regs_as_val(regs))),
            Expr::Hole(name) => Expr::Hole(name),
        }
    }
}

#[allow(unused_variables)]
pub trait ExtInstance: std::fmt::Debug {
    fn peek(&mut self, port: &str) -> Value { panic!(); }
    fn poke(&mut self, port: &str, value: Value) -> Vec<&str> { panic!(); }
    fn clock(&mut self) {}
    fn reset(&mut self) {}
}

pub struct Nettle {
    circuit: Module,
    state: BTreeMap<Path, Value>,
    exts: BTreeMap<Path, Box<dyn ExtInstance>>,
    indent: usize,
    debug: bool,
}

impl Nettle {
    pub fn new(circuit: &Module) -> Nettle {
        let mut state = BTreeMap::new();
        for (terminal, typ) in &circuit.paths {
            if let PathType::Node = typ {
                state.insert(terminal.to_string(), Value::X);
            }
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

    pub fn ext(mut self, path: &str, ext_inst: Box<dyn ExtInstance>) -> Self {
        self.exts.insert(path.to_string(), ext_inst);
        self
    }

    fn wires(&self) -> &BTreeMap<Path, Expr> {
        &self.circuit.wires
    }

    pub fn paths(&self) -> Vec<Path> {
        self.state.keys().cloned().collect()
    }

    pub fn peek(&self, path: &str) -> Value {
        let value = if !self.is_reg(path) {
            self.state[path]
        } else {
            let val_path = format!("{path}.val");
            self.state[&val_path]
        };

        if self.debug {
            let padding = " ".repeat(self.indent * 4);
            eprintln!("{padding}peek({path}) = {:?}", value);
        }
        value
    }

    pub fn poke(&mut self, path: &str, value: Value) {
        if self.debug {
            let padding = " ".repeat(self.indent * 4);
            eprintln!("{padding}poke({path}, {value:?})");
            self.indent += 1;
        }

        if !self.is_reg(path) {
            self.state.insert(path.to_string(), value);
            self.update(&path.to_string());
        } else {
            let set_path = format!("{path}.set");
            self.state.insert(set_path.to_string(), value);
            self.update(&path.to_string());
        }


        if self.debug {
            self.indent -= 1;
        }
    }

    pub fn set(&mut self, path: &str, value: Value) {
        if self.debug {
            let padding = " ".repeat(self.indent * 4);
            eprintln!("{padding}set({path}, {value:?})");
            self.indent += 1;
        }

        let set_path = format!("{path}.set");
        self.state.insert(set_path, value);

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

    fn update(&mut self, terminal: &Path) {
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

    fn is_reg(&self, path: &str) -> bool {
        if let PathType::Reg(_reset) = self.circuit.paths[path] {
            true
        } else {
            false
        }
    }

    pub fn regs(&self) -> Vec<Path> {
        let mut result = vec![];
        for (terminal, typ) in &self.circuit.paths {
            if let PathType::Reg(_reset) = typ {
                result.push(terminal.to_string());
            }
        }
        result
    }

    pub fn clock(&mut self) {
        if self.debug {
            let padding = " ".repeat(self.indent * 4);
            eprintln!("{padding}clock()");
            self.indent += 1;
        }

        for path in self.regs() {
            let set_path = format!("{path}.set");
            let val_path = format!("{path}.val");
            let set_value = self.state[&set_path];

            if self.debug {
                let val_value = self.state[&val_path];
                let padding = " ".repeat(self.indent * 4);
                eprintln!("{padding}register clocked: {path} {val_value:?} => {set_value:?}");
            }

            self.state.insert(val_path.clone(), set_value);
        }

        for (path, ext) in &mut self.exts {
            ext.clock();
            if self.debug {
                let padding = " ".repeat(self.indent * 4);
                eprintln!("{padding}ext clocked: {path}");
            }
        }

        for path in self.regs() {
            let val_path = format!("{path}.val");
            self.update(&val_path);
        }

        if self.debug {
            self.indent -= 1;
        }
    }

    pub fn reset(&mut self) {
        if self.debug {
            let padding = " ".repeat(self.indent * 4);
            eprintln!("{padding}reset()");
            self.indent += 1;
        }

        for path in self.regs() {
            let val_path = format!("{path}.val");
            match self.circuit.paths.get(&path).unwrap() {
                PathType::Node => (),
                PathType::Reg(reset) => {
                    if *reset != Value::X {
                        if self.debug {
                            let padding = " ".repeat(self.indent * 4);
                            let val_value = self.state[&val_path];
                            eprintln!("{padding}register reset: {path} {val_value:?}");
                        }
                        self.state.insert(val_path, *reset);
                    }
                },
            }
        }

        for (path, ext) in &mut self.exts {
            ext.reset();
            if self.debug {
                let padding = " ".repeat(self.indent * 4);
                eprintln!("{padding}ext reset: {path}");
            }
        }

        for path in self.regs() {
            let val_path = format!("{path}.val");
            self.update(&val_path);
        }

        if self.debug {
            self.indent -= 1;
        }
    }
}

impl std::fmt::Debug for Nettle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        // writeln!(f, "State:")?;
        let mut states: Vec<(_, _)> = self.state.iter().collect();
        states.sort_by_key(|(terminal, _)| terminal.to_string());
        states = states.into_iter().rev().collect();
        for (terminal, value) in states {
            writeln!(f, "    {:>5}  {terminal}", format!("{value:?}"))?;
        }
        // writeln!(f, "Wires:")?;
        // for (terminal, expr) in &self.circuit.wires {
        //     writeln!(f, "    {terminal:<25} <= {expr:?}")?;
        // }

        Ok(())
    }
}

#[derive(Debug, Clone)]
enum PathType {
    Node,
    Reg(Value),
}

#[derive(Debug)]
pub struct ModuleDef {
    paths: BTreeMap<Path, PathType>,
    wires: BTreeMap<Path, Expr>,
    path: Vec<String>,
    exts: Vec<Path>,
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
    pub fn new(name: &str) -> ModuleDef {
        ModuleDef {
            paths: BTreeMap::new(),
            wires: BTreeMap::new(),
            path: vec![name.to_string()],
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

    fn terminal(&self, name: &str) -> Path {
        let path = self.path.join(".");
        format!("{path}.{name}")
    }

    pub fn node(mut self, name: &str) -> Self {
        let terminal = self.terminal(name);
        self.paths.insert(terminal, PathType::Node);
        self
    }

    pub fn reg(mut self, name: &str, reset: Value) -> Self {
        let path = self.terminal(name);
        let set_path = format!("{path}.set");
        let val_path = format!("{path}.val");

        self.paths.insert(path, PathType::Reg(reset));
        self.paths.insert(set_path, PathType::Node);
        self.paths.insert(val_path, PathType::Node);
        self
    }

    pub fn wire(mut self, name: &str, expr: &Expr) -> Self {
        let terminal = self.terminal(name);
        self.wires.insert(terminal, expr.clone().relative_to(&self.current_path()));
        self
    }

    pub fn instantiate(mut self, name:  &str, circuit: &ModuleDef) -> Self {
        let path = self.current_path();
        self = self.push(name);

        for (terminal, typ) in &circuit.paths {
            let target = relative_to(&path, terminal);
            self.paths.insert(target, typ.clone());
        }

        for (terminal, expr) in &circuit.wires {
            let target = relative_to(&path, terminal);
            let expr = expr.clone().relative_to(&path);
            self.wires.insert(target, expr);
        }
        self = self.pop();
        self
    }

    fn current_path(&self) -> Path {
        self.path.join(".")
    }

    pub fn ext(mut self, name: &str, terminals: &[&str]) -> Self {
        let ext = self.terminal(name);
        self.exts.push(ext.clone());

        for terminal in terminals {
            let target = format!("{ext}.{terminal}");
            self.paths.insert(target, PathType::Node);
        }
        self
    }

    fn regs(&self) -> Vec<Path> {
        let mut result = vec![];
        for (path, typ) in &self.paths {
            if let PathType::Reg(_reset) = typ {
                result.push(path.clone());
            }
        }
        result
    }

    fn expand_regs(mut self) -> Self {
        let regs: Vec<Path> = self.regs();

        // fix sets (on the right)
        let targets: Vec<Path> = self.wires.keys().cloned().collect();
        for target in targets {
            if regs.contains(&target) {
                let set_path = format!("{target}.set");
                let expr = self.wires.remove(&target).unwrap();
                self.wires.insert(set_path, expr);
            }
        }

        // fix vals (on the left)
        let mut wires: Vec<(Path, Expr)> = vec![];
        for (target, expr) in &self.wires {
            let expr = expr.clone().expand_regs_as_val(&regs);
            wires.push((target.to_string(), expr));
        }
        self.wires = wires.into_iter().collect();
        self
    }

    pub fn build(self) -> Module {
        Module(Arc::new(self.expand_regs()))
    }
}

#[test]
fn expand_regs() {
    let m = Module::new("top")
        .reg("r", Value::X)
        .node("n")
        .node("m")
        .wire("r", &Expr::Path("n".to_string()))
        .wire("m", &Expr::Path("r".to_string()))
        .build();

    assert!(m.wires.contains_key("top.r.set"));
    assert!(!m.wires.contains_key("top.r"));
    assert!(m.wires.contains_key("top.m"));
    assert_eq!(m.wires["top.m"], Expr::Path("top.r.val".to_string()));
}

fn mod_to_module(m: Mod) -> ModuleDef {
    let Mod(name, decls) = m;
    let mut module = Module::new(&name);

    for decl in decls {
        match decl {
            ModDecl::Node(name) => module = module.node(&name),
            ModDecl::Reg(name, reset) => module = module.reg(&name, reset),
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

pub fn parse_top(circuit: &str) -> Module {
    let m = parse::TopParser::new().parse(circuit).unwrap();
    mod_to_module(m).build()
}

pub fn parse_testbench(testbench: &str) -> Testbench {
    parse::TestbenchParser::new().parse(testbench).unwrap()
}

fn read_testbench_file(filename: &str) -> Option<Testbench> {
    if let Ok(text) = std::fs::read_to_string(filename) {
        Some(parse_testbench(&text))
    } else {
        None
    }
}

fn testbench_for(filename: &str) -> Option<String> {
    let path = std::path::Path::new(filename);
    let parent: &std::path::Path = path.parent().unwrap();
    let file = format!("{}.tb", path.file_stem().unwrap().to_str().unwrap());
    let tb_filename = parent.join(file).into_os_string().into_string().unwrap();
    let exists = std::fs::metadata(&tb_filename).map(|metadata| metadata.is_file()).unwrap_or(false);

    if exists {
        Some(tb_filename)
    } else {
        None
    }
}

fn main() {
    let argv: Vec<String> = std::env::args().collect();
    let default = "Top.nettle".to_string();
    let filename = argv.get(1).unwrap_or(&default);
    let text = std::fs::read_to_string(filename).unwrap();

    let tb = if let Some(tb_filename) = testbench_for(filename) {
        println!("Using testbench file: {tb_filename}");
        read_testbench_file(&tb_filename).unwrap()
    } else {
        println!("No testbench file");
        Testbench(vec![TestbenchCommand::Debug])
    };

    let top = parse_top(&text);
    let monitor = Box::new(Monitor::new());

    let mut nettle =
        Nettle::new(&top)
            .ext("top.vip", monitor);

    let verbose = true;
    for command in &tb.0 {
        match command {
            TestbenchCommand::Peek(terminal) => {
                print!("PEEK {terminal} ");
                let value = nettle.peek(terminal);
                if verbose {
                    println!("=> {value:?}");
                }
            },
            TestbenchCommand::Poke(terminal, value) => {
                if verbose {
                    println!("POKE {terminal} <= {value:?}");
                }
                nettle.poke(terminal, *value);
            },
            TestbenchCommand::Set(terminal, value) => {
                if verbose {
                    println!("SET {terminal} = {value:?}");
                }
                nettle.set(terminal, *value);
            },
            TestbenchCommand::Clock => {
                if verbose {
                    println!("CLOCK");
                }
                nettle.clock();
            },
            TestbenchCommand::Reset => {
                if verbose {
                    println!("RESET");
                }
                nettle.reset();
            },
            TestbenchCommand::Debug => {
                println!("{nettle:#?}");
            },
            TestbenchCommand::Assert(e) => {
                let result = e.eval(&nettle);
                if result != Value::Bit(true) {
                    println!("Assertion failed: {e:?}");
                    for path in e.paths() {
                        println!("    {path} => {:?}", nettle.peek(&path));

                    }
                    panic!("");
                }
            },
        }
    }
}

#[test]
fn buffer() {
    let buffer = parse_top("
        top {
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
    let counter = parse_top("
        top {
            node out;
            reg counter reset 0w4;
            out <= counter;
            counter <= counter.val + 1w4;
        }
    ");

    let mut nettle = Nettle::new(&counter);

    nettle.reset();

    for i in 0..16 {
        assert_eq!(nettle.peek("top.out"), Value::Word(4, i));
        nettle.clock();
    }
    assert_eq!(nettle.peek("top.out"), Value::Word(4, 0));
}

#[test]
fn triangle_numbers() {
    let top = parse_top("
        top {
            node out;
            reg sum reset 0w32;
            mod counter {
                node out;
                reg counter reset 1w32;
                out <= counter;
                counter <= counter + 1w4;
            }
            out <= sum;
            sum <= sum + counter.out;
        }
    ");

    let mut nettle = Nettle::new(&top);
    nettle.reset();

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
            println!("Monitor: {s}");
            self.0 = None
        }
    }
}

#[test]
fn vip() {
    let top = parse_top("
        top {
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
    let top = parse_top("
        top {
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
