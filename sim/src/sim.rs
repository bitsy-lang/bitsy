use crate::circuit::*;
use crate::value::*;

use std::sync::Arc;

#[derive(Debug)]
pub struct Sim {
    circuit: Arc<Circuit>,
    net_values: Vec<Value>,
}

impl Sim {
    pub fn new(circuit: Circuit) -> Sim {
        let net_values = circuit.net_ids().iter().map(|_net_id| Value::X).collect();
        let circuit = Arc::new(circuit);
        let mut sim = Sim {
            circuit,
            net_values,
        };
        sim.update_constants();
        sim
    }

    pub fn poke_net(&mut self, net_id: NetId, value: Value) {
        eprintln!("poke_net({net_id}, {value:?})");
        self.net_values[net_id] = value.clone();

        // update dependent nets through all combs
        for Comb(target, expr) in self.combs_dependent_on(net_id) {
            let value = expr.eval(&self);
            self.poke_net(target.net_id(), value);
        }
    }
    pub fn peek_net(&self, net_id: NetId) -> Value {
        self.net_values[net_id].clone()
    }

    fn update_constants(&mut self) {
        eprintln!("update_constants()");
        for Comb(target, expr) in self.combs() {
            if expr.is_constant() {
                let value = expr.eval(&self);
                self.poke_net(target.net_id(), value);
            }
        }
    }

    pub fn clock(&mut self) {
        let mut updates = vec![];
        for register in self.circuit.registers() {
            let value = self.peek_net(register.set().net_id());
            updates.push((register.val().net_id(), value));
        }
        for (val_net_id, value) in updates {
            self.poke_net(val_net_id, value);
        }
    }

    pub fn reset(&mut self) {
        for register in self.registers() {
            self.poke_net(register.val().net_id(), register.reset.eval(&self));
        }
    }

    fn registers(&self) -> Vec<Register> {
        self.circuit.registers().clone()
    }

    fn combs(&self) -> Vec<Comb> {
        self.circuit.combs.clone()
    }

    fn combs_dependent_on(&self, net_id: NetId) -> Vec<Comb> {
        self.circuit.combs_dependent_on(net_id)
    }

    pub fn state(&self) -> Vec<(Terminal, Value)> {
        let mut results = vec![];
        for terminal in &self.circuit.terminals() {
            results.push((terminal.clone(), self.net_values[terminal.net_id()].clone()));
        }
        results
    }
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
