use super::*;

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct NetId(usize);

pub struct Sim {
    circuit: Circuit,
    exts: BTreeMap<Path, Box<dyn ExtInstance>>,
    indent: usize,
    nets: Vec<Net>,
    net_values: BTreeMap<NetId, Value>,
    debug: bool,
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
            debug: false,
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
        let value = if !self.is_reg(&path) {
            // let value = self.state[&path];
            let net_id = self.net_id_for(path.clone());
            let value = self.peek_net(net_id);
            value
        } else {
            let val_path = format!("{path}.val").into();
            let net_id = self.net_id_for(val_path);
            let value = self.peek_net(net_id);
            value
        };

        if self.debug {
            let padding = " ".repeat(self.indent * 4);
            eprintln!("{padding}peek({path}) = {:?}", value);
        }
        value
    }

    pub fn poke<P: Into<Path>>(&mut self, path: P, value: Value) {
        let path: Path = path.into();

        if self.debug {
            let padding = " ".repeat(self.indent * 4);
            eprintln!("{padding}poke({path}, {value:?})");
            self.indent += 1;
        }

        if !self.is_reg(&path) {
            let net_id = self.net_id_for(path.clone());
            self.poke_net(net_id, value.clone());
            self.broadcast_update(path);
        } else {
            let set_path: Path = format!("{path}.set").into();
            let net_id = self.net_id_for(set_path.clone());
            self.poke_net(net_id, value.clone());
        }


        if self.debug {
            self.indent -= 1;
        }
    }

    pub fn set<P: Into<Path>>(&mut self, path: P, value: Value) {
        let path: Path = path.into();
        if self.debug {
            let padding = " ".repeat(self.indent * 4);
            eprintln!("{padding}set({path}, {value:?})");
            self.indent += 1;
        }

        let val_path: Path = format!("{path}.val").into();
        let net_id = self.net_id_for(val_path.clone());
        self.poke_net(net_id, value.clone());
        self.broadcast_update(val_path);

        if self.debug {
            self.indent -= 1;
        }
    }

    fn broadcast_update_constants(&mut self) {
        if self.debug {
            let padding = " ".repeat(self.indent * 4);
            eprintln!("{padding}update_constants()");
            self.indent += 1;
        }

        let wires = self.circuit.wires().clone();
        for (target_terminal, expr) in &wires {
            if expr.is_constant() {
                if self.debug {
                    let padding = " ".repeat(self.indent * 4);
                    eprintln!("{padding}affected: {target_terminal}");
                }
                let value = expr.eval(&self);
                self.poke(target_terminal.clone(), value);
            }
        }

        if self.debug {
            self.indent -= 1;
        }
    }

    fn broadcast_update(&mut self, path: Path) {
        if self.debug {
            let padding = " ".repeat(self.indent * 4);
            eprintln!("{padding}broadcast_update({path})");
            self.indent += 1;
        }

        let wires = self.circuit.wires().clone();
        for (target_terminal, expr) in &wires {
            if expr.depends_on(path.clone()) {
                if self.debug {
                    let padding = " ".repeat(self.indent * 4);
                    eprintln!("{padding}affected: {target_terminal}");
                }
                let value = expr.eval(&self);
                self.poke(target_terminal.clone(), value);
            }
        }

        let ext_path = parent_of(path.clone());
        let value = self.peek(path.clone());
        if let Some(ext) = self.exts.get_mut(&ext_path) {
            let local_path: Path = path[ext_path.len() + 1..].into();
            ext.poke(&local_path, value);
        }

        if self.debug {
            self.indent -= 1;
        }
    }

    fn is_reg(&self, path: &Path) -> bool {
        if !self.circuit.paths().contains_key(&path) {
            dbg!(path);
        }

        if let PathType::Reg(_typ, _reset) = self.circuit.paths()[&path] {
            true
        } else {
            false
        }
    }

    pub fn regs(&self) -> Vec<Path> {
        let mut result = vec![];
        for (terminal, typ) in self.circuit.paths() {
            if let PathType::Reg(_typ, _reset) = typ {
                result.push(terminal.clone());
            }
        }
        result
    }

    pub fn clock(&mut self) {
        if self.debug {
            let padding = " ".repeat(self.indent * 4);
            eprintln!("{padding}clock()");
            self.indent += 1;
        }

        for path in self.regs() {
            let set_path: Path = format!("{path}.set").into();
            let val_path: Path = format!("{path}.val").into();
            let set_value = self.peek(set_path);

            if self.debug {
                let padding = " ".repeat(self.indent * 4);
                let val_value = self.peek(val_path.clone());
                eprintln!("{padding}register clocked: {path} {val_value:?} => {set_value:?}");
            }

            self.poke(val_path, set_value);
        }

        for (path, ext) in &mut self.exts {
            ext.clock();
            if self.debug {
                let padding = " ".repeat(self.indent * 4);
                eprintln!("{padding}ext clocked: {path}");
            }
        }

        for path in self.regs() {
            let val_path: Path = format!("{path}.val").into();
            self.broadcast_update(val_path);
        }

        if self.debug {
            self.indent -= 1;
        }
    }

    pub fn reset(&mut self) {
        if self.debug {
            let padding = " ".repeat(self.indent * 4);
            eprintln!("{padding}reset()");
            self.indent += 1;
        }

        for path in self.regs() {
            let val_path = format!("{path}.val");
            match self.circuit.paths().get(&path).unwrap() {
                PathType::Node(_typ) => (),
                PathType::Incoming(_typ) => (),
                PathType::Outgoing(_typ) => (),
                PathType::Reg(_typ, reset) => {
                    if *reset != Value::X {
                        if self.debug {
                            let padding = " ".repeat(self.indent * 4);
                            let val_value = self.peek(val_path.clone());
                            eprintln!("{padding}register reset: {path} {val_value:?}");
                        }
                        self.poke(val_path, *reset);
                    }
                },
            }
        }

        for (path, ext) in &mut self.exts {
            ext.reset();
            if self.debug {
                let padding = " ".repeat(self.indent * 4);
                eprintln!("{padding}ext reset: {path}");
            }
        }

        for path in self.regs() {
            let val_path: Path = format!("{path}.val").into();
            self.broadcast_update(val_path);
        }

        if self.debug {
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
