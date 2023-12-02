use std::collections::BTreeMap;
use crate::path::Path;
use crate::expr::Expr;
use crate::reference::Ref;
use crate::value::Value;
use crate::loc::Loc;

pub type NetId = usize;
pub type CombId = usize;
pub type ExtId = usize;
pub type RegId = usize;

#[derive(Debug)]
pub struct Circuit {
    pub components: BTreeMap<Path, Component>,
    pub combs: Vec<Comb>, // indexed by NetId
    pub dependents: Vec<Dependents>, // indexed by NetId
}

#[derive(Clone)]
pub struct Net(pub Vec<Path>);

#[derive(Debug, Clone)]
pub struct Comb(pub Terminal, pub Expr);

#[derive(Debug)]
pub struct Dependents {
    pub combs: Vec<CombId>,
}

#[derive(Debug)]
pub enum Component {
    Terminal(Terminal),
    Register(Register),
}

impl Circuit {
    pub fn new() -> Circuit {
        Circuit {
            components: BTreeMap::new(),
            combs: vec![],
            dependents: vec![],
        }
    }

    pub fn add_terminal<P: Into<Path>>(&mut self, path: P) -> Terminal {
        let path: Path = path.into();
        let net_id = self.next_net_id();
        let terminal = Terminal(path.to_string(), net_id);
        self.components.insert(path, Component::Terminal(terminal.clone()));
        terminal
    }

    pub fn add_register<P: Into<Path>>(&mut self, path: P) -> Register {
        let path: Path = path.into();
        let val = format!("{path}.set");
        let set = format!("{path}");
        let val_net_id = self.next_net_id();
        let set_net_id = val_net_id + 1;
        let register = Register {
            val: Terminal(val, val_net_id),
            set: Terminal(set, set_net_id),
            reset: Expr::Lit(Loc::unknown(), Value::X),
        };
        self.components.insert(path, Component::Register(register.clone()));
        register
    }

    fn next_net_id(&self) -> NetId {
        self.terminals().len()
    }

    pub fn connect(&mut self, terminal: Terminal, expr: Expr) -> CombId {
        let comb_id = self.combs.len();
        self.combs.push(Comb(terminal, expr));
        comb_id
    }

    pub fn latch(&mut self, register: Register, expr: Expr) -> CombId {
        self.connect(register.set().clone(), expr)
    }

    pub fn terminals(&self) -> Vec<Terminal> {
        let mut results = vec![];
        for (_path, component) in &self.components {
            match component {
                Component::Terminal(terminal) => results.push(terminal.clone()),
                Component::Register(register) => {
                    results.push(register.set().clone());
                    results.push(register.val().clone());
                },
            }
        }
        results
    }

    pub fn registers(&self) -> Vec<Register> {
        let mut results = vec![];
        for (_path, component) in &self.components {
            match component {
                Component::Register(register) => results.push(register.clone()),
                Component::Terminal(_terminal) => (),
            }
        }
        results
    }

    pub fn net_ids(&self) -> Vec<NetId> {
        self.terminals().into_iter().map(|terminal| terminal.net_id()).collect()
    }

    pub fn combs_dependent_on(&self, net_id: NetId) -> Vec<Comb> {
        let mut results = vec![];
        for comb in &self.combs {
            let Comb(_target, expr) = &comb;
            if expr.depends_on_net(net_id) {
                results.push(comb.clone());
            }
        }
        results
    }
}

#[derive(Clone, Debug)]
pub struct Terminal(String, NetId);

impl Terminal {
    pub fn name(&self) -> &str {
        &self.0
    }

    pub fn net_id(&self) -> NetId {
        self.1
    }
}

/*
impl From<Terminal> for Ref<NetId> {
    fn from(t: Terminal) -> Ref<NetId> {
        t.0
    }
}
*/

#[derive(Clone, Debug)]
pub struct Register {
    pub set: Terminal,
    pub val: Terminal,
    pub reset: Expr,
}

impl Register {
    pub fn val(&self) -> Terminal {
        self.val.clone()
    }

    pub fn set(&self) -> Terminal {
        self.set.clone()
    }
}
