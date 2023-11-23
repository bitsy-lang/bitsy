use super::*;

use std::time::Duration;
use std::time::SystemTime;

const DEBUG: bool = false;

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct NetId(usize);

pub struct Sim {
    pub circuit: Circuit,
    exts: BTreeMap<Path, Box<dyn ExtInstance>>,
    indent: usize,
    nets: Vec<Net>,
    net_values: BTreeMap<NetId, Value>,
    clock_ticks: u64,
    start_time: SystemTime,
    clock_freq_cap: Option<f64>,
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
            start_time: SystemTime::now(),
            clock_ticks: 0,
            clock_freq_cap: None,
        };
        nettle.broadcast_update_constants();
        nettle
    }

    pub fn cap_clock_freq(mut self, freq: f64) -> Self {
        self.clock_freq_cap = Some(freq);
        self
    }

    pub fn ext<P: Into<Path>>(mut self, path: P, ext_inst: Box<dyn ExtInstance>) -> Self {
        self.exts.insert(path.into(), ext_inst);
        self.broadcast_update_constants();
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

    pub fn peek<P: Into<Path>>(&self, path: P) -> Value {
        let path: Path = path.into();

        if DEBUG {
            let padding = " ".repeat(self.indent * 4);
            eprint!("{padding}peek({path}) = ");
            use std::io::Write;
            std::io::stderr().flush().unwrap();
        }

        let terminal_path = path.clone();
        let net_id = self.net_id_for(terminal_path);
        let value = self.net_values[&net_id];

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

        let terminal_path = if !self.is_reg(&path) {
            path.clone()
        } else {
            path.clone()
        };

        let net_id = self.net_id_for(terminal_path.clone());
        self.net_values.insert(net_id, value);

        if !self.is_reg(&path) {
            self.broadcast_update(path);
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

        let net_id = self.net_id_for(path.clone());
        self.net_values.insert(net_id, value);
        self.broadcast_update(path);

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
                self.poke(target_terminal.clone(), value);
            }
        }

        let ext_path = parent_of(terminal.clone());
        let value = self.peek(terminal.clone());
        if self.exts.contains_key(&ext_path) {
            if DEBUG {
                let padding = " ".repeat(self.indent * 4);
                eprintln!("{padding}ext affected: {ext_path}");
            }

            let ext = self.exts.get_mut(&ext_path).unwrap();
            let port_name: PortName = terminal[ext_path.len() + 1..].into();
            if ext.incoming_ports().contains(&port_name) {
                if DEBUG {
                    let padding = " ".repeat(self.indent * 4);
                    eprintln!("{padding}ext port affected: {ext_path}.{port_name}");
                }
                let updates = ext.update(port_name.to_string(), value);
                if DEBUG {
                    let padding = " ".repeat(self.indent * 4);
                    eprintln!("{padding}    updates:");
                    for (portname, value) in &updates {
                        eprintln!("{padding}        {portname} => {value:?}");
                    }
                }
                let poke_values: Vec<(Path, Value)> = updates
                    .into_iter()
                    .map(|(port_name, value)| {
                        let affected_path: Path = format!("{ext_path}.{port_name}").into();
                        (affected_path, value)
                    })
                    .collect();

                for (path, value) in poke_values {
                    self.poke(path, value);
                }
            }
        }

        if DEBUG {
            self.indent -= 1;
        }
    }

    fn is_reg(&self, path: &Path) -> bool {
        if !self.circuit.components().contains_key(&path.parent()) &&
           !self.circuit.components().contains_key(&path) {
            eprintln!("is_reg({path}) failed");
        }

        if let Component::Reg(_typ, _reset) = self.circuit.components()[&path.parent()] {
            true
        } else if let Component::Reg(_typ, _reset) = self.circuit.components()[&path] {
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

        self.clock_ticks += 1;

        // frequency cap
        if let Some(clock_freq_cap) = self.clock_freq_cap {
            let mut clock_freq = self.clocks_per_second();
            while clock_freq.is_finite() && clock_freq > clock_freq_cap {
                clock_freq = self.clocks_per_second();
            }
        }


        for path in self.circuit.regs() {
            let set_value = self.peek(path.set());

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
            let reset = self.circuit.reset_for_reg(path.clone());
            if DEBUG {
                if reset != Value::X {
                    let padding = " ".repeat(self.indent * 4);
                    eprintln!("{padding}register reset: {path}");
                }
            }
            self.poke(path, reset);
        }

        for (path, ext) in &mut self.exts {
            ext.reset();
            if DEBUG {
                let padding = " ".repeat(self.indent * 4);
                eprintln!("{padding}ext reset: {path}");
            }
        }

        for path in self.circuit.regs() {
            self.broadcast_update(path);
        }

        if DEBUG {
            self.indent -= 1;
        }
    }

    pub fn clocks_per_second(&self) -> f64 {
        let end_time = SystemTime::now();
        let duration: Duration = end_time.duration_since(self.start_time).unwrap();
        1_000_000.0 * self.clock_ticks as f64 / duration.as_micros() as f64
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
