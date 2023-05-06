#![allow(unused, dead_code)]

use std::fmt::Write;
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

#[derive(Debug)]
pub struct Simulator {
    domains: Vec<DomainState>,
    signals: Vec<SignalState>,
}

#[derive(Debug)]
struct DomainState {
    id: Domain,
    name: String,
    cycle: usize,
    resetting: bool,
}

fn gate_op(gate_fn: &String, args: Vec<Value>) -> Value {
    Python::with_gil(|py| {
        py.run("import sys", None, None).unwrap();
        py.run("sys.path.append('.')", None, None).unwrap();
        let gates_module = py.import("gates").unwrap();
        let func = gates_module.getattr(&**gate_fn).unwrap();

        for arg in &args {
            if let Value::Unobservable = arg {
                return Value::Unobservable;
            }
        }

        let pyargs: Vec<PyObject> = args.into_iter().map(|arg| value_to_pyobject(py, arg)).collect();
        let obj = func.call(PyTuple::new(py, pyargs), None).unwrap();
        pyany_to_value(obj)
    })
}

fn value_to_pyobject(py: Python, value: Value) -> PyObject {
    match value {
        Value::Bool(b) => b.into_py(py),
        Value::Word(n) => n.into_py(py),
        _ => Python::None(py),
    }
}

fn pyany_to_value(obj: &PyAny) -> Value {
    match obj.get_type().name().unwrap() {
        "int" => Value::Word(obj.extract().unwrap()),
        "bool" => Value::Bool(obj.extract().unwrap()),
        type_name => panic!("Unknown type: {type_name}"),
    }
}

#[derive(Debug, Clone)]
pub enum Dep {
    Disconnected,
    Peek(Signal),
    Prev(Signal),
    Gate(String, Vec<Signal>),
}

#[derive(Debug)]
struct SignalState {
    id: Signal,
    path: String,
    domain_id: Domain,
    dep: Dep,
    values: [Value; 2],
    init_value: Option<Value>,
    is_output_port: bool,
    is_input_port: bool,
}

impl Simulator {
    pub fn new(circuit: Arc<Nettle>) -> Simulator {
        let mut domains = vec![];
        let mut signals = vec![];

        let mut path_to_signal: BTreeMap<String, Signal> = BTreeMap::new();

        domains.push(DomainState {
            id: Domain(0),
            name: "d".to_string(),
            cycle: 0,
            resetting: true,
        });

        for signal in &circuit.signals {
            let signal_state = SignalState {
                id: Signal(signals.len()),
                path: signal.path.clone(),
                domain_id: Domain(0),
                dep: Dep::Disconnected,
                values: [Value::Unobservable; 2],
                init_value: signal.init_value(),
                is_output_port: signal.is_output_port(),
                is_input_port: signal.is_input_port(),
            };
            path_to_signal.insert(signal_state.path.to_string(), signal_state.id);
            signals.push(signal_state);
        }

        let mut simulator = Simulator {
            domains,
            signals,
        };

        simulator.add_signal_deps(&circuit, path_to_signal);
        simulator
    }

    fn add_signal_deps(
        &mut self,
        circuit: &Nettle,
        path_to_signal: BTreeMap<String, Signal>,
    ) {
        for (i, signal) in circuit.signals.iter().enumerate() {
            let mut dep = Dep::Disconnected;

            for attr in &signal.attrs {
                if let Attr::Peek(path) = attr {
                    let dep_signal = path_to_signal[path];
                    dep = Dep::Peek(dep_signal);
                } else if let Attr::Prev(path) = attr {
                    let dep_signal = path_to_signal[path];
                    dep = Dep::Prev(dep_signal);
                } else if let Attr::Gate(gate_fn, paths) = attr {
                    let dep_signals = paths.iter().map(|path| path_to_signal[path]).collect();
                    dep = Dep::Gate(gate_fn.to_string(), dep_signals);
                }
            }

            let signal_state = &mut self.signals[i];
            signal_state.dep = dep;
        }
    }

    fn add_domains(&mut self) {
        self.domains.push(DomainState {
            id: Domain(0),
            name: "d".to_string(),
            cycle: 0,
            resetting: true,
        });
    }

    fn delete_me(&mut self) {
        /*
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
                        _ => panic!("Unknown gate type: {}", gate.gate_name),

                    }
                },
                Component::Reg(_name, _visibility, reg) => {
                    let set_id = Signal(self.signals.len());
                    self.signals.push(SignalState {
                        id: set_id,
                        domain_id: Domain(0),
                        path: format!("{}.set", component_path.join(".")),
                        dep: Dep::Disconnected,
                        values: [Value::Unknown, reg.init],
                        init_value: None,
                    });

                    let val_id = Signal(self.signals.len());
                    self.signals.push(SignalState {
                        id: val_id,
                        domain_id: Domain(0),
                        path: format!("{}.val", component_path.join(".")),
                        dep: Dep::Reg(set_id),
                        values: [Value::Unknown, Value::Unknown],
                        init_value: Some(reg.init),
                    });
                }
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

        for Wire(_visibility, sink, source) in &mod_def.wires {
            let sink_path = terminal_to_signal_path(sink);
            let source_path = terminal_to_signal_path(source);

            let sink_signal = self.signal_by_path(&sink_path).expect(&format!("Looking for {sink_path}"));
            let source_signal = self.signal_by_path(&source_path).expect(&format!("Looking for {source_path}"));

            let sink_signal_state = &mut self.signals[sink_signal.id()];
            sink_signal_state.dep = Dep::Query(source_signal);
        }
        */
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
            signal_state.values[0] = signal_state.values[1];
            signal_state.values[1] = Value::Unknown;
        }

        for (signal, value) in pokes.into_iter() {
            self.poke(*signal, *value);
        }

//        println!();
//        println!("--------------------------------------------------------------------------------");
//        println!();
//        self.dump();
//        println!();

        for signal in self.top_output_signals().into_iter() {
            self.query(signal, 0);
        }

//        println!();
//        self.dump();
//        println!();
    }

    pub fn reset(
        &mut self,
        domain: Domain,
        pokes: &[(Signal, Value)],
     ) {
        let signals = self.signals_in_domain(domain);
        let mut domain_state = &mut self.domains[domain.id()];
        domain_state.resetting = true;

        self.step(domain, pokes);

        while !self.all_signals_observable(&signals) {
            self.step(domain, pokes);
        }

        let mut domain_state = &mut self.domains[domain.id()];
        domain_state.resetting = false;
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
        for signal in &self.signals {
            if signal.is_output_port {
                result.push(signal.id);
            }
        }
        result
    }

    fn top_input_signals(&self) -> Vec<Signal> {
        let mut result = vec![];
        for signal in &self.signals {
            if signal.is_input_port {
                result.push(signal.id);
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
        // println!("{spaces}Querying {}", signal_state.path);

        let current_value = self.peek(signal);
        if current_value != Value::Unknown {
            // println!("{spaces}    Value has already been computed: {current_value}");
            return current_value;
        }

        match signal_state.dep.clone() {
            Dep::Peek(depend_signal) => {
                // println!("{spaces}    {} depends on {}", self.signal_path(signal), self.signal_path(depend_signal));
                let val = self.query(depend_signal, depth + 1);
                // println!("{spaces}    returning {val}");
                assert!(val != Value::Unknown, "Query failed");
                self.poke(signal, val);
                return val;
            }
            Dep::Prev(set_signal) => {
                let domain = &self.domains[signal_state.domain_id.id()];
                if domain.resetting {
                    let init_val = signal_state.init_value.unwrap();

                    let val = if let Value::Unobservable = init_val {
                        let set_signal_state = &self.signals[set_signal.id()];
                        let val = set_signal_state.values[0];
                        // println!("{spaces}    The previous cycle of the register {} gives {val}", set_signal_state.path);
                        assert!(val != Value::Unknown, "Query failed");
                        val
                    } else {
                        // println!("{spaces}    Resetting holds {} at its init value {init_val}", signal_state.path);
                        init_val
                    };

                    self.poke(signal, val);
                    // println!("{spaces}    Querying set pin");
                    self.query(set_signal, depth + 1);
                    // println!("{spaces}    returning {val}");
                    val
                } else {
                    let set_signal_state = &self.signals[set_signal.id()];
                    let val = set_signal_state.values[0];
                    // println!("{spaces}    The previous cycle of the register {} gives {val}", set_signal_state.path);
                    assert!(val != Value::Unknown, "Query failed");
                    self.poke(signal, val);

                    // println!("{spaces}    Querying set pin");
                    self.query(set_signal, depth + 1);

                    // println!("{spaces}    returning {val}");
                    val
                }
            },
            Dep::Gate(gate_fn, depend_signals) => {
                let mut vals = vec![];
                // println!("{spaces}    {} depends on {}", self.signal_path(signal), depend_signals.iter().map(|depend_signal| self.signal_path(*depend_signal)).collect::<Vec<_>>().join(", "));
                for depend_signal in &depend_signals {
                    let val = self.query(*depend_signal, depth + 1);
                    vals.push(val);
                }
                // println!("{spaces}    applying gate op {gate_fn:?} to values {vals:?}");
                let val = gate_op(&gate_fn, vals);
                self.poke(signal, val);
                // println!("{spaces}    returning {val}");
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
        signal_state.values[1]
    }

    fn poke(&mut self, signal: Signal, value: Value) {
        let mut signal_state = &mut self.signals[signal.id()];
        signal_state.values[1] = value;
    }

    pub fn write_header(&self, f: &mut dyn Write) -> std::fmt::Result {
        writeln!(f, "$date")?;
        writeln!(f, "	Sat Apr 29 15:46:50 2023")?;
        writeln!(f, "$end")?;

        writeln!(f, "$version")?;
        writeln!(f, "	Nettle")?;
        writeln!(f, "$end")?;

        Ok(())
    }

    pub fn write_definitions(&self, f : &mut dyn Write) -> std::fmt::Result {
        writeln!(f, "$scope module Top $end")?;
        writeln!(f, "$var reg 8 ! data [7:0] $end")?;
        writeln!(f, "$upscope $end")?;
        writeln!(f, "$enddefinitions $end")?;

        writeln!(f, "$dumpvars")?;

        Ok(())
    }

    pub fn write_change(&self, f : &mut dyn Write) -> std::fmt::Result {
        writeln!(f, "#0")?;
        writeln!(f, "$var reg 8 ! data [7:0] $end")?;

        Ok(())
    }
}
