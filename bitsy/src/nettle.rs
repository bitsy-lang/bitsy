/*
fn to_nettle(
    circuit: &Circuit,
    module: &str,
    mut component_path: Vec<String>,
) {
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
}
*/
