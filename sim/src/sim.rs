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

    pub fn peek(&mut self, terminal: Terminal) -> Value {
        self.peek_net(terminal.net_id())
    }

    pub fn poke(&mut self, terminal: Terminal, value: Value) {
        self.poke_net(terminal.net_id(), value)
    }

    fn poke_net(&mut self, net_id: NetId, value: Value) {
        self.net_values[net_id] = value.clone();

        // update dependent nets through all combs
        for comb in self.combs_dependent_on(net_id) {
            let inputs: Vec<Value> = comb.inputs().iter().map(|terminal| self.net_values[terminal.net_id()].clone()).collect();
            for (terminal, value) in comb.outputs().iter().zip(comb.eval(&inputs).iter()) {
                self.poke_net(terminal.net_id(), value.clone());
            }
        }
    }
    fn peek_net(&self, net_id: NetId) -> Value {
        self.net_values[net_id].clone()
    }

    fn update_constants(&mut self) {
        for comb in self.combs() {
            if comb.is_constant() {
                let inputs: Vec<Value> = comb.inputs().iter().map(|terminal| self.net_values[terminal.net_id()].clone()).collect();
                for (terminal, value) in comb.outputs().iter().zip(comb.eval(&inputs).iter()) {
                    self.poke_net(terminal.net_id(), value.clone());
                }
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
            self.poke_net(register.val().net_id(), register.reset());
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
