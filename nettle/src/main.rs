#![allow(dead_code)]

mod parse;
mod value;
mod expr;
mod nettle;
mod testbench;
mod path;
mod ext;
#[cfg(test)]
mod tests;

use parse::*;
use value::*;
use expr::*;
use nettle::*;
use testbench::*;
use path::*;
use ext::*;

use std::collections::BTreeMap;
use std::sync::Arc;

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
        format!("{path}.{name}").into()
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
        self.paths.insert(set_path.into(), PathType::Node);
        self.paths.insert(val_path.into(), PathType::Node);
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
        self.path.join(".").into()
    }

    pub fn ext(mut self, name: &str, terminals: &[&str]) -> Self {
        let ext = self.terminal(name);
        self.exts.push(ext.clone());

        for terminal in terminals {
            let target = format!("{ext}.{terminal}");
            self.paths.insert(target.into(), PathType::Node);
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
                self.wires.insert(set_path.into(), expr);
            }
        }

        // fix vals (on the left)
        let mut wires: Vec<(Path, Expr)> = vec![];
        for (target, expr) in &self.wires {
            let expr = expr.clone().expand_regs_as_val(&regs);
            wires.push((target.clone(), expr));
        }
        self.wires = wires.into_iter().collect();
        self
    }

    pub fn build(self) -> Module {
        Module(Arc::new(self.expand_regs()))
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
    for command in tb.0 {
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
                nettle.poke(terminal, value);
            },
            TestbenchCommand::Set(terminal, value) => {
                if verbose {
                    println!("SET {terminal} = {value:?}");
                }
                nettle.set(terminal, value);
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
                        println!("    {path} => {:?}", nettle.peek(path.clone()));

                    }
                    panic!("");
                }
            },
        }
    }
}
