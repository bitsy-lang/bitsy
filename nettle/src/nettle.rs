use super::*;

pub struct Nettle {
    circuit: Circuit,
    state: BTreeMap<Path, Value>,
    exts: BTreeMap<Path, Box<dyn ExtInstance>>,
    indent: usize,
    debug: bool,
}

impl Nettle {
    pub fn new(circuit: &Circuit) -> Nettle {
        let mut state = BTreeMap::new();
        for (terminal, typ) in &circuit.paths {
            if let PathType::Node = typ {
                state.insert(terminal.clone(), Value::X);
            }
        }
        let mut nettle = Nettle {
            circuit: circuit.clone(),
            state,
            exts: BTreeMap::new(),
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

    fn wires(&self) -> &BTreeMap<Path, Expr> {
        &self.circuit.wires
    }

    pub fn paths(&self) -> Vec<Path> {
        self.state.keys().cloned().collect()
    }

    pub fn peek<P: Into<Path>>(&self, path: P) -> Value {
        let path: Path = path.into();
        let value = if !self.is_reg(&path) {
            self.state[&path]
        } else {
            let val_path = format!("{path}.val");
            self.state[&val_path.into()]
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
            self.state.insert(path.clone(), value);
            self.broadcast_update(path);
        } else {
            let set_path = format!("{path}.set");
            self.state.insert(set_path.into(), value);
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
        self.state.insert(val_path.clone(), value);
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

        let wires = self.wires().clone();
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

        let wires = self.wires().clone();
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
        if !self.circuit.paths.contains_key(&path) {
            dbg!(path);
        }

        if let PathType::Reg(_reset) = self.circuit.paths[&path] {
            true
        } else {
            false
        }
    }

    pub fn regs(&self) -> Vec<Path> {
        let mut result = vec![];
        for (terminal, typ) in &self.circuit.paths {
            if let PathType::Reg(_reset) = typ {
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
            let set_value = self.state[&set_path.into()];

            if self.debug {
                let val_value = self.state[&val_path.clone()];
                let padding = " ".repeat(self.indent * 4);
                eprintln!("{padding}register clocked: {path} {val_value:?} => {set_value:?}");
            }

            self.state.insert(val_path, set_value);
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
            match self.circuit.paths.get(&path).unwrap() {
                PathType::Node => (),
                PathType::Reg(reset) => {
                    if *reset != Value::X {
                        if self.debug {
                            let padding = " ".repeat(self.indent * 4);
                            let val_value = self.state[&val_path.clone().into()];
                            eprintln!("{padding}register reset: {path} {val_value:?}");
                        }
                        self.state.insert(val_path.into(), *reset);
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

impl std::fmt::Debug for Nettle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        // writeln!(f, "State:")?;
        let mut states: Vec<(_, _)> = self.state.iter().collect();
        states.sort_by_key(|(terminal, _)| terminal.to_string());
        states = states.into_iter().rev().collect();
        for (terminal, value) in states {
            writeln!(f, "    {:>5}  {terminal}", format!("{value:?}"))?;
        }
        // writeln!(f, "Wires:")?;
        // for (terminal, expr) in &self.circuit.wires {
        //     writeln!(f, "    {terminal:<25} <= {expr:?}")?;
        // }

        Ok(())
    }
}

