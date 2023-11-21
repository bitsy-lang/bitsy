use super::*;

const DEBUG: bool = true;

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct NetId(usize);

pub struct Sim {
    circuit: Circuit,
    exts: BTreeMap<Path, Box<dyn ExtInstance>>,
    indent: usize,
    nets: Vec<Net>,
    net_values: BTreeMap<NetId, Value>,
}

impl Sim {
    pub fn new(circuit: &Circuit) -> Sim {
        let nets = circuit.nets();
        let net_values = nets.iter().enumerate().map(|(net_id, _net)| (NetId(net_id), Value::X)).collect();

        let mut nettle = Sim {
            circuit: circuit.clone(),
            exts: BTreeMap::new(),
            nets,
            net_values,
            indent: 0,
        };
        nettle.broadcast_update_constants();
        nettle
    }

    pub fn ext<P: Into<Path>>(mut self, path: P, ext_inst: Box<dyn ExtInstance>) -> Self {
        self.exts.insert(path.into(), ext_inst);
        self
    }

    fn net_id_for(&self, terminal: Path) -> NetId {
        for (net_id, net) in self.nets.iter().enumerate() {
            if net.contains(terminal.clone()) {
                return NetId(net_id);
            }
        }
        panic!("No net found for terminal: {terminal:?}")
    }

    fn net_for(&mut self, terminal: Path) -> &mut Net {
        let NetId(net_id) = self.net_id_for(terminal);
        &mut self.nets[net_id]
    }

    fn peek_net(&self, net_id: NetId) -> Value {
        self.net_values[&net_id]
    }

    fn poke_net(&mut self, net_id: NetId, value: Value) {
        self.net_values.insert(net_id, value);
    }

    pub fn peek<P: Into<Path>>(&self, path: P) -> Value {
        let path: Path = path.into();

        if DEBUG {
            let padding = " ".repeat(self.indent * 4);
            eprint!("{padding}peek({path}) = ");
            use std::io::Write;
            std::io::stderr().flush().unwrap();
        }

        if !self.circuit.components().contains_key(&path.parent()) {
            eprintln!("NO PATH: {path}");
        }

        let terminal_path = match &self.circuit.components()[&path.parent()] {
            Component::Outgoing(_typ) => path.set(),
            Component::Incoming(_typ) => path.val(),
            Component::Node(_typ) => path.val(),
            Component::Reg(_typ, _reset) => path.val(),
            Component::Mod => panic!(),
            Component::Ext => panic!(),
        };

        let net_id = self.net_id_for(terminal_path);
        let value = self.peek_net(net_id);

        if DEBUG {
            eprintln!("{:?}", value);
        }

        value
    }

    pub fn poke<P: Into<Path>>(&mut self, path: P, value: Value) {
        let path: Path = path.into();

        if DEBUG {
            let padding = " ".repeat(self.indent * 4);
            eprintln!("{padding}poke({path}, {value:?})");
            self.indent += 1;
        }

        let terminal_path = match &self.circuit.components()[&path] {
            Component::Outgoing(_typ) => path.set(),
            Component::Incoming(_typ) => path.val(),
            Component::Node(_typ) => path.set(),
            Component::Reg(_typ, _reset) => path.set(),
            Component::Mod => panic!(),
            Component::Ext => panic!(),
        };

        let net_id = self.net_id_for(terminal_path.clone());
        self.poke_net(net_id, value.clone());

        if !self.is_reg(&path) {
            self.broadcast_update(terminal_path);
        }

        if DEBUG {
            self.indent -= 1;
        }
    }

    pub fn set<P: Into<Path>>(&mut self, path: P, value: Value) {
        let path: Path = path.into();
        if DEBUG {
            let padding = " ".repeat(self.indent * 4);
            eprintln!("{padding}set({path}, {value:?})");
            self.indent += 1;
        }

        let val_path: Path = format!("{path}.val").into();
        let net_id = self.net_id_for(val_path.clone());
        self.poke_net(net_id, value.clone());
        self.broadcast_update(val_path);

        if DEBUG {
            self.indent -= 1;
        }
    }

    fn broadcast_update_constants(&mut self) {
        if DEBUG {
            let padding = " ".repeat(self.indent * 4);
            eprintln!("{padding}update_constants()");
            self.indent += 1;
        }

        let wires = self.circuit.wires().clone();
        for (target_terminal, expr) in &wires {
            if expr.is_constant() {
                if DEBUG {
                    let padding = " ".repeat(self.indent * 4);
                    eprintln!("{padding}affected: {target_terminal}");
                }
                let value = expr.eval(&self);
                self.poke(target_terminal.clone(), value);
            }
        }

        if DEBUG {
            self.indent -= 1;
        }
    }

    fn broadcast_update(&mut self, terminal: Path) {
        if DEBUG {
            let padding = " ".repeat(self.indent * 4);
            eprintln!("{padding}broadcast_update({terminal})");
            self.indent += 1;
        }

        let wires = self.circuit.wires().clone();
        for (target_terminal, expr) in &wires {
            if expr.depends_on(terminal.clone()) {
                if DEBUG {
                    let padding = " ".repeat(self.indent * 4);
                    eprintln!("{padding}affected: {target_terminal}");
                }
                let value = expr.eval(&self);
                self.poke(target_terminal.parent(), value);
            }
        }

        let ext_path = parent_of(terminal.clone());
        let value = self.peek(terminal.clone());
        if self.exts.contains_key(&ext_path) {
            let ext = self.exts.get_mut(&ext_path).unwrap();
            let local_path: Path = terminal[ext_path.len() + 1..].into();
            let updates = ext.poke(local_path.to_string(), value);
            let poke_values: Vec<(Path, Value)> = updates
                .into_iter()
                .map(|(port_name, value)| {
                    let affected_path: Path = format!("{ext_path}.{port_name}").into();
                    (affected_path, value)
                })
                .collect();

            for (path, value) in poke_values {
                let net_id = self.net_id_for(path.clone());
                self.poke_net(net_id, value);
            }
        }

        if DEBUG {
            self.indent -= 1;
        }
    }

    fn is_reg(&self, path: &Path) -> bool {
        if let Component::Reg(_typ, _reset) = self.circuit.components()[&path.parent()] {
            true
        } else {
            false
        }
    }

    pub fn clock(&mut self) {
        if DEBUG {
            let padding = " ".repeat(self.indent * 4);
            eprintln!("{padding}clock()");
            self.indent += 1;
        }

        for path in self.circuit.regs() {
            let set_value = self.peek(path.clone());

            if DEBUG {
                let padding = " ".repeat(self.indent * 4);
                let val_value = self.peek(path.clone());
                eprintln!("{padding}register clocked: {path} {val_value:?} => {set_value:?}");
            }

            self.poke(path, set_value);
        }

        let mut poke_values: Vec<(Path, Value)> = vec![];
        for (ext_path, ext) in &mut self.exts {
            if DEBUG {
                let padding = " ".repeat(self.indent * 4);
                eprintln!("{padding}ext clocked: {ext_path}");
            }
            let updates = ext.clock();
            poke_values.extend(updates
                .into_iter()
                .map(|(port_name, value)| {
                    let affected_path: Path = format!("{ext_path}.{port_name}").into();
                    (affected_path, value)
                }));
        }

        for (path, value) in poke_values {
            self.poke(path, value);
//            let net_id = self.net_id_for(path.clone());
//            self.poke_net(net_id, value);
//            self.broadcast_update(path);
        }

        for path in self.circuit.regs() {
            self.broadcast_update(path);
        }

        if DEBUG {
            self.indent -= 1;
        }
    }

    pub fn reset(&mut self) {
        if DEBUG {
            let padding = " ".repeat(self.indent * 4);
            eprintln!("{padding}reset()");
            self.indent += 1;
        }

        for path in self.circuit.regs() {
            let val_path = format!("{path}.val");
            let component = self.circuit.components().get(&path).unwrap();
            if let Component::Reg(_typ, reset) = component {
                if *reset != Value::X {
                    if DEBUG {
                        let padding = " ".repeat(self.indent * 4);
                        let val_value = self.peek(val_path.clone());
                        eprintln!("{padding}register reset: {path} {val_value:?}");
                    }
                    self.poke(val_path, *reset);
                } else {
                    unreachable!()
                }
            }
        }

        for (path, ext) in &mut self.exts {
            ext.reset();
            if DEBUG {
                let padding = " ".repeat(self.indent * 4);
                eprintln!("{padding}ext reset: {path}");
            }
        }

        for path in self.circuit.regs() {
            let val_path: Path = format!("{path}.val").into();
            self.broadcast_update(val_path);
        }

        if DEBUG {
            self.indent -= 1;
        }
    }
}

impl std::fmt::Debug for Sim {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        for (NetId(net_id), value) in &self.net_values {
            let net = &self.nets[*net_id];
            write!(f, "    {:>5}   ", format!("{value:?}"))?;
            writeln!(f, "{}", net.terminals().iter().map(|t| t.to_string()).collect::<Vec<String>>().join(" "))?;
        }

        Ok(())
    }
}
