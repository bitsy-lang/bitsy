#![allow(unused, dead_code)]

use std::collections::BTreeMap;
use std::sync::Arc;
use super::ast::*;
use pyo3::prelude::*;
use pyo3::types::{IntoPyDict, PyDict, PyString, PyBool, PyInt, PyTuple};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Signal(usize);

impl Signal {
    fn id(&self) -> usize {
        self.0
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct Domain(usize);

impl Domain {
    fn id(&self) -> usize {
        self.0
    }
}

pub struct Simulator {
    circuit: Arc<Circuit>,
    top: String,
    domains: Vec<DomainState>,
    signals: Vec<SignalState>,
}

struct DomainState {
    id: Domain,
    name: String,
    cycle: usize,
    reseting: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GateFn {
    Named(&'static str),
}

fn gate_op(gate_fn: GateFn, args: Vec<Value>) -> Value {
    match gate_fn {
        GateFn::Named(gate_fn) => {
            Python::with_gil(|py| {
                py.run("import sys", None, None).unwrap();
                py.run("sys.path.append('.')", None, None).unwrap();
                let gates_module = py.import("gates").unwrap();
                let func = gates_module.getattr(gate_fn).unwrap();

                let pyargs: Vec<PyObject> = args.into_iter().map(|arg| value_to_pyobject(py, arg)).collect();
                let obj = func.call(PyTuple::new(py, pyargs), None).unwrap();
                pyany_to_value(obj)
            })
        },
    }
}

fn value_to_pyobject(py: Python, value: Value) -> PyObject {
    match value {
        Value::Bit(b) => b.into_py(py),
        Value::Word(n) => n.into_py(py),
        _ => Python::None(py),
    }
}

fn pyany_to_value(obj: &PyAny) -> Value {
    match obj.get_type().name().unwrap() {
        "int" => Value::Word(obj.extract().unwrap()),
        "bool" => Value::Bit(obj.extract().unwrap()),
        type_name => panic!("Unknown type: {type_name}"),
    }
}

#[derive(Debug, Clone)]
enum Dep {
    Disconnected,
    Query(Signal),
    Reg(Signal),
    Gate(Vec<Signal>, GateFn),
}

struct SignalState {
    id: Signal,
    path: String,
    domain_id: Domain,
    dep: Dep,
    values: [Value; 2],
    init_value: Option<Value>,
}

impl Simulator {
    pub fn new(circuit: Arc<Circuit>, top: &str) -> Simulator {
        let mut simulator = Simulator {
            circuit: circuit.clone(),
            top: top.to_string(),
            domains: vec![],
            signals: vec![],
        };

        simulator.add_domains(top);
        simulator.add_signals(top, vec!["top".to_string()]);

        simulator
    }

    fn add_domains(&mut self, module: &str) {
        self.domains.push(DomainState {
            id: Domain(0),
            name: "d".to_string(),
            cycle: 0,
            reseting: true,
        });
    }

    fn add_signals(&mut self, module: &str, mut component_path: Vec<String>) {
        let circuit = self.circuit.clone();
        let mod_def = circuit.mod_def(module).clone();

        let mut local_signals: BTreeMap<String, Signal> = BTreeMap::new();

        for Port(name, _dir, _shape, domain) in &mod_def.ports {
            let id = Signal(self.signals.len());
            self.signals.push(SignalState {
                id,
                domain_id: Domain(0),
                path: format!("{}.{}", component_path.join("."), name),
                dep: Dep::Disconnected, // to be set shortly
                values: [Value::Unknown, Value::Unknown],
                init_value: None,
            });
            local_signals.insert(format!("io.{name}"), id);
        }

        for component in &mod_def.components {
            component_path.push(component.name().to_string());
            match component {
                Component::Mod(_name, _visibility, module) => {
                    self.add_signals(&module.moddef_name, component_path.clone());
                },
                Component::Gate(_name, _visibility, gate) => {
                    /*
                    match gate.gate_name.as_ref() {
                        "Adder" => {
                            let a_id = Signal(self.signals.len());
                            self.signals.push(SignalState {
                                id: a_id,
                                domain_id: Domain(0),
                                path: format!("{}.a", component_path.join(".")),
                                dep: Dep::Disconnected,
                                values: [Value::Unknown, Value::Unknown],
                                init_value: None,
                            });

                            let b_id = Signal(self.signals.len());
                            self.signals.push(SignalState {
                                id: b_id,
                                domain_id: Domain(0),
                                path: format!("{}.b", component_path.join(".")),
                                dep: Dep::Disconnected,
                                values: [Value::Unknown, Value::Unknown],
                                init_value: None,
                            });

                            let out_id = Signal(self.signals.len());
                            self.signals.push(SignalState {
                                id: out_id,
                                domain_id: Domain(0),
                                path: format!("{}.out", component_path.join(".")),
                         //       dep: Dep::Gate(vec![a_id, b_id], GateFn::Named("Add".to_string())),
                                dep: Dep::Gate(vec![a_id, b_id], GateFn::Add),
                                values: [Value::Unknown, Value::Unknown],
                                init_value: None,
                            });
                        },
                        "Mux" => {
                            let sel_id = Signal(self.signals.len());
                            self.signals.push(SignalState {
                                id: sel_id,
                                domain_id: Domain(0),
                                path: format!("{}.sel", component_path.join(".")),
                                dep: Dep::Disconnected,
                                values: [Value::Unknown, Value::Unknown],
                                init_value: None,
                            });

                            let a_id = Signal(self.signals.len());
                            self.signals.push(SignalState {
                                id: a_id,
                                domain_id: Domain(0),
                                path: format!("{}.a", component_path.join(".")),
                                dep: Dep::Disconnected,
                                values: [Value::Unknown, Value::Unknown],
                                init_value: None,
                            });

                            let b_id = Signal(self.signals.len());
                            self.signals.push(SignalState {
                                id: b_id,
                                domain_id: Domain(0),
                                path: format!("{}.b", component_path.join(".")),
                                dep: Dep::Disconnected,
                                values: [Value::Unknown, Value::Unknown],
                                init_value: None,
                            });

                            let out_id = Signal(self.signals.len());
                            self.signals.push(SignalState {
                                id: out_id,
                                domain_id: Domain(0),
                                path: format!("{}.out", component_path.join(".")),
                                dep: Dep::Gate(vec![sel_id, a_id, b_id], GateFn::Named("Mux")),
                                values: [Value::Unknown, Value::Unknown],
                                init_value: None,
                            });
                        },
                        "Not" => {
                            let a_id = Signal(self.signals.len());
                            self.signals.push(SignalState {
                                id: a_id,
                                domain_id: Domain(0),
                                path: format!("{}.a", component_path.join(".")),
                                dep: Dep::Disconnected,
                                values: [Value::Unknown, Value::Unknown],
                                init_value: None,
                            });

                            let out_id = Signal(self.signals.len());
                            self.signals.push(SignalState {
                                id: out_id,
                                domain_id: Domain(0),
                                path: format!("{}.out", component_path.join(".")),
                                dep: Dep::Gate(vec![a_id], GateFn::Named("Not")),
                                values: [Value::Unknown, Value::Unknown],
                                init_value: None,
                            });
                        },
                        "And" => {
                            let a_id = Signal(self.signals.len());
                            self.signals.push(SignalState {
                                id: a_id,
                                domain_id: Domain(0),
                                path: format!("{}.a", component_path.join(".")),
                                dep: Dep::Disconnected,
                                values: [Value::Unknown, Value::Unknown],
                                init_value: None,
                            });

                            let b_id = Signal(self.signals.len());
                            self.signals.push(SignalState {
                                id: b_id,
                                domain_id: Domain(0),
                                path: format!("{}.b", component_path.join(".")),
                                dep: Dep::Disconnected,
                                values: [Value::Unknown, Value::Unknown],
                                init_value: None,
                            });

                            let out_id = Signal(self.signals.len());
                            self.signals.push(SignalState {
                                id: out_id,
                                domain_id: Domain(0),
                                path: format!("{}.out", component_path.join(".")),
                                dep: Dep::Gate(vec![a_id, b_id], GateFn::Named("And")),
                                values: [Value::Unknown, Value::Unknown],
                                init_value: None,
                            });
                        },
                        _ => panic!("Unknown gate type: {}", gate.gate_name),

                    }
                    */
                },
                Component::Reg(_name, _visibility, reg) => {
                    let set_id = Signal(self.signals.len());
                    self.signals.push(SignalState {
                        id: set_id,
                        domain_id: Domain(0),
                        path: format!("{}.set", component_path.join(".")),
                        dep: Dep::Disconnected,
                        values: [Value::Unknown, reg.init.clone()],
                        init_value: None,
                    });

                    let val_id = Signal(self.signals.len());
                    self.signals.push(SignalState {
                        id: val_id,
                        domain_id: Domain(0),
                        path: format!("{}.val", component_path.join(".")),
                        dep: Dep::Reg(set_id),
                        values: [Value::Unknown, Value::Unknown],
                        init_value: Some(reg.init.clone()),
                    });
                }
                Component::Const(_name, _visibility, value) => {
                    todo!()
                },
            }
            component_path.pop();
        }

        let terminal_to_signal_path = |terminal: &Terminal| -> String {
            if terminal.component() == "io" {
                format!("{}.{}", component_path.join("."), terminal.port())
            } else {
                format!("{}.{}.{}", component_path.join("."), terminal.component(), terminal.port())
            }
        };

        for wire in &mod_def.wires {
            match wire {
                Wire::Simple(_visibility, sink, source) => {
                    let sink_path = terminal_to_signal_path(sink);
                    let source_path = terminal_to_signal_path(source);

                    let sink_signal = self.signal_by_path(&sink_path).expect(&format!("Looking for {sink_path}"));
                    let source_signal = self.signal_by_path(&source_path).expect(&format!("Looking for {source_path}"));

                    let sink_signal_state = &mut self.signals[sink_signal.id()];
                    sink_signal_state.dep = Dep::Query(source_signal);
                },
                Wire::Expr(_visibility, sink, expr) => {
                    todo!()
                },
            }
        }
    }

    pub fn step(
        &mut self,
        domain: Domain,
        pokes: &[(Signal, Value)],
    ) {
        let mut domain_state = &mut self.domains[domain.id()];
        domain_state.cycle += 1;

        for signal in self.signals_in_domain(domain).into_iter() {
            let mut signal_state = &mut self.signals[signal.id()];
            signal_state.values[0] = signal_state.values[1].clone();
            signal_state.values[1] = Value::Unknown;
        }

        for (signal, value) in pokes.into_iter() {
            self.poke(*signal, value.clone());
        }

        println!();
        println!("--------------------------------------------------------------------------------");
        println!();
        self.dump();
        println!();

        for signal in self.top_output_signals().into_iter() {
            self.query(signal, 0);
        }

        println!();
        self.dump();
        println!();
    }

    pub fn reset(
        &mut self,
        domain: Domain,
        pokes: &[(Signal, Value)],
     ) {
        let signals = self.signals_in_domain(domain);
        let mut domain_state = &mut self.domains[domain.id()];
        domain_state.reseting = true;

        self.step(domain, pokes);

        while !self.all_signals_observable(&signals) {
            self.step(domain, pokes);
        }

        let mut domain_state = &mut self.domains[domain.id()];
        domain_state.reseting = false;
    }

    fn all_signals_observable(&self, signals: &[Signal]) -> bool {
        for signal in signals {
            let signal_state = &self.signals[signal.id()];

            if signal_state.values[1] == Value::Unobservable {
                return false;
            }
        }
        true
    }

    fn top_output_signals(&self) -> Vec<Signal> {
        let mut result = vec![];
        let circuit = self.circuit.clone();
        let mod_def = circuit.mod_def(&self.top).clone();
        for Port(name, dir, _shape, _domain) in &mod_def.ports {
            if *dir == Direction::Outgoing {
                let signal = self.signal_by_path(&format!("top.{name}")).unwrap();
                result.push(signal);
            }
        }
        result
    }

    fn dump(&self) {
        for signal_state in &self.signals {
            println!("{:<30} {:>8} {:>8}", signal_state.path, signal_state.values[0].to_string(), signal_state.values[1].to_string());
        }
    }

    fn query(&mut self, signal: Signal, depth: usize) -> Value {
        let spaces = &vec![b' '; 4 * depth];
        let spaces = String::from_utf8_lossy(spaces);
        let signal_state = &self.signals[signal.id()];
        println!("{spaces}Querying {}", signal_state.path);

        let current_value = self.peek(signal);
        if current_value != Value::Unknown {
            println!("{spaces}    Value has already been computed: {current_value}");
            return current_value;
        }

        match signal_state.dep.clone() {
            Dep::Query(depend_signal) => {
                println!("{spaces}    {} depends on {}", self.signal_path(signal), self.signal_path(depend_signal));
                let val = self.query(depend_signal, depth + 1).clone();
                println!("{spaces}    returning {val}");
                assert!(val != Value::Unknown, "Query failed");
                self.poke(signal, val.clone());
                val
            }
            Dep::Reg(set_signal) => {
                let domain = &self.domains[signal_state.domain_id.id()];
                if domain.reseting {
                    let val = signal_state.init_value.clone().unwrap();
                    println!("{spaces}    Resetting holds {} at its init value {val}", signal_state.path);
                    self.poke(signal, val.clone());
                    println!("{spaces}    Querying set pin");
                    self.query(set_signal, depth + 1);
                    println!("{spaces}    returning {}", val.clone());
                    val.clone()
                } else {
                    let set_signal_state = &self.signals[set_signal.id()];
                    let val = set_signal_state.values[0].clone();
                    println!("{spaces}    The previous cycle of the register {} gives {}", set_signal_state.path, val.clone());
                    assert!(val != Value::Unknown, "Query failed");
                    self.poke(signal, val.clone());

                    println!("{spaces}    Querying set pin");
                    self.query(set_signal, depth + 1);

                    println!("{spaces}    returning {}", val.clone());
                    val.clone()
                }
            },
            Dep::Gate(depend_signals, gate_fn) => {
                let mut vals = vec![];
                for depend_signal in &depend_signals {
                    println!("{spaces}    {} depends on {}", self.signal_path(signal), self.signal_path(*depend_signal));
                    let val = self.query(*depend_signal, depth + 1);
                    vals.push(val);
                }
                println!("{spaces}    applying gate op {gate_fn:?} to values {vals:?}");
                let val = gate_op(gate_fn, vals);
                self.poke(signal, val.clone());
                println!("{spaces}    returning {}", val.clone());
                val
            },
            Dep::Disconnected => panic!("Disconnected: Signal {} has no driver", signal_state.path)
        }
    }

    pub fn signals(&self) -> Vec<Signal> {
        let mut result = vec![];
        for i in 0..self.signals.len() {
            result.push(Signal(i));
        }
        result
    }

    pub fn signal_path(&self, signal: Signal) -> &str {
        let signal_state = &self.signals[signal.id()];
        &signal_state.path
    }

    pub fn signal_by_path(&self, path: &str) -> Option<Signal> {
        for signal in &self.signals {
            if signal.path == path {
                return Some(signal.id)
            }
        }
        None
    }

    pub fn signals_in_domain(&self, domain: Domain) -> Vec<Signal> {
        let mut result = vec![];
        for signal_state in &self.signals {
            if signal_state.domain_id == domain {
                result.push(signal_state.id);
            }
        }
        result
    }

    pub fn domains(&self) -> Vec<Domain> {
        let mut result = vec![];
        for i in 0..self.domains.len() {
            result.push(Domain(i));
        }
        result
    }

    pub fn peek(&self, signal: Signal) -> Value {
        let signal_state = &self.signals[signal.id()];
        signal_state.values[1].clone()
    }

    fn poke(&mut self, signal: Signal, value: Value) {
        let mut signal_state = &mut self.signals[signal.id()];
        signal_state.values[1] = value;
    }
}
