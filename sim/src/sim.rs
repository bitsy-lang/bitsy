use crate::circuit::*;
use crate::path::*;
use crate::value::*;

use std::collections::BTreeMap;
use std::sync::Arc;

pub struct Sim {
    circuit: Arc<Circuit>,
    net_values: Vec<Value>,
}

impl Sim {
    pub fn new() -> Sim {
        todo!()
    }

    /*
    pub fn net_values(&self) -> BTreeMap<NetId, Value> {
        self.net_values.iter().cloned().enumerate().collect()
    }

    pub fn net(&self, net_id: NetId) -> &Net {
        &self.circuit.nets[net_id]
    }
    */

    pub fn poke_net(&mut self, net_id: NetId, value: Value) {
        self.net_values[net_id] = value.clone();

        // update dependent nets through all combs
        let dependents = &self.circuit.clone().dependents[net_id];
        for comb_id in dependents.combs.iter() {
            let comb = &self.circuit.clone().combs[*comb_id];
            let Comb(target_net_id, expr) = comb;
            let value = expr.eval(&self);
            self.poke_net(*target_net_id, value);
        }
    }

    pub fn peek_net(&self, net_id: NetId) -> Value {
        self.net_values[net_id].clone()
    }

    /*
    pub fn set<P: Into<Path>>(&mut self, _path: P, _value: Value) {
        todo!()
    }

    fn broadcast_update_constants(&mut self) {
        for Comb(target_net_id, expr) in self.circuit.clone().combs.iter() {
            if expr.is_constant() {
                let value = expr.eval(&self);
                self.poke_net(*target_net_id, value);
            }
        }
    }

    pub fn clock(&mut self) {
        let mut updates = vec![];
        for reginfo in &self.circuit.clone().regs {
            let value = self.peek_net(reginfo.set_net_id);
            updates.push((reginfo.val_net_id, value));
        }
        for (val_net_id, value) in updates {
            self.poke_net(val_net_id, value);
        }

        /*
        for ext in &mut self.exts {
            if let Some(ext) = ext {
                ext.clock();
            } else {
                // TODO
                panic!("Unlinked ext")
            }
        }
        */
    }

    pub fn reset(&mut self) {
        for reginfo in &self.circuit.clone().regs {
            self.poke_net(reginfo.val_net_id, reginfo.reset.eval(&self));
        }

        /*
        for ext in &mut self.exts {
            if let Some(ext) = ext {
                ext.reset();
            } else {
                // TODO
                panic!("Unlinked ext")
            }
        }
        */
    }
    */
}


impl Comb {
    pub fn depends_on(&self, net_id: NetId) -> bool {
        let Comb(_net_id, expr) = self;
        expr.depends_on_net(net_id)
    }
}

/*
fn driver_for(terminal: Path, immediate_driver_for: &BTreeMap<Path, Path>) -> Path {
    let mut driver: &Path = &terminal;
    while let Some(immediate_driver) = &immediate_driver_for.get(driver) {
        driver = immediate_driver;
    }
    driver.clone()
}

impl std::fmt::Debug for Net {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "Net({} {})", self.0, self.1.iter().map(|path| path.to_string()).collect::<Vec<_>>().join(" "))
    }
}

impl Net {
    pub fn from(terminal: Path) -> Net {
        Net(terminal, vec![])
    }

    pub fn add(&mut self, terminal: Path) {
        if self.0 != terminal {
            self.1.push(terminal);
            self.1.sort();
            self.1.dedup();
        }
    }

    pub fn driver(&self) -> Path {
        self.0.clone()
    }

    pub fn drivees(&self) -> &[Path] {
        &self.1
    }

    pub fn terminals(&self) -> Vec<Path> {
        let mut results = vec![self.0.clone()];
        for terminal in &self.1 {
            results.push(terminal.clone());
        }
        results
    }

    pub fn contains(&self, terminal: Path) -> bool {
        if terminal == self.0 {
            true
        } else {
            self.1.contains(&terminal)
        }
    }
}
*/
