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

pub struct Circuit {
    pub terminals: Vec<Path>,
    pub nets: Vec<Net>, // indexed by NetId
    pub combs: Vec<Comb>, // indexed by NetId
    pub regs: Vec<Register>, // indexed by RegId
    pub dependents: Vec<Dependents>, // indexed by NetId
    pub net_id_by_path: BTreeMap<Path, NetId>,
}

#[derive(Clone)]
pub struct Net(pub Vec<Path>);

#[derive(Debug, Clone)]
pub struct Comb(pub Terminal, pub Expr);

#[derive(Debug)]
pub struct Dependents {
    pub combs: Vec<CombId>,
}

impl Circuit {
    pub fn new() -> Circuit {
        Circuit {
            terminals: vec![],
            nets: vec![],
            combs: vec![],
            regs: vec![],
            dependents: vec![],
            net_id_by_path: BTreeMap::new(),
        }
    }

    pub fn add_terminal(&mut self, path: &str) -> Terminal {
        Terminal(Ref::new(path.to_string()))
    }

    pub fn add_register(&mut self, path: &str) -> Register {
        let set = format!("{path}");
        let val = format!("{path}.set");
        Register {
            set: Terminal(Ref::new(set)),
            val: Terminal(Ref::new(val)),
            reset: Expr::Lit(Loc::unknown(), Value::X),
        }
    }

    pub fn connect(&mut self, terminal: Terminal, expr: Expr) -> CombId {
        let comb_id = self.combs.len();
        self.combs.push(Comb(terminal, expr));
        comb_id
    }

    pub fn latch(&mut self, register: Register, expr: Expr) -> CombId {
        let comb_id = self.combs.len();
        self.combs.push(Comb(register.set().clone(), expr));
        comb_id
    }

    pub fn net_ids(&self) -> Vec<NetId> {
        (0..self.nets.len()).into_iter().collect()
    }
}

#[derive(Clone, Debug)]
pub struct Terminal(Ref<NetId>);

impl Terminal {
    pub fn net_id(&self) -> NetId {
        *self.0.get().unwrap()
    }
}

impl From<Terminal> for Ref<NetId> {
    fn from(t: Terminal) -> Ref<NetId> {
        t.0
    }
}

#[derive(Clone, Debug)]
pub struct Register {
    pub set: Terminal,
    pub val: Terminal,
    pub reset: Expr,
}

impl Register {
    pub fn val(&self) -> &Terminal {
        &self.val
    }

    pub fn set(&self) -> &Terminal {
        &self.set
    }
}
