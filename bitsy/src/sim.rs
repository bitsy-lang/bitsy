use super::*;

#[cfg(test)]
mod tests;
mod value;
mod eval;
pub mod ext;

pub use value::Value;
use ext::*;

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::sync::Arc;
use std::time::Duration;
use std::time::SystemTime;

pub type NetId = usize;
pub type CombId = usize;
pub type ExtInstId = usize;
pub type ExtId = usize;
pub type RegId = usize;

#[derive(Debug)]
pub struct RegInfo {
    set_net_id: NetId,
    val_net_id: NetId,
    reset: Option<Arc<Expr>>,
}

#[derive(Debug)]
pub struct Dependents {
    pub combs: Vec<CombId>,
    pub ext_inst_ports: Vec<(ExtInstId, PortName)>,
}

#[derive(Debug)]
pub struct SimCircuit {
    pub nets: Vec<Net>, // indexed by NetId
    pub combs: Vec<Comb>, // indexed by NetId
    pub regs: Vec<RegInfo>, // indexed by RegId

    pub dependents: Vec<Dependents>, // indexed by NetId

    pub net_id_by_ext_port: BTreeMap<(ExtInstId, PortName), NetId>,

    pub net_id_by_path: BTreeMap<Path, NetId>,
    pub ext_inst_id_by_path: BTreeMap<Path, ExtInstId>,
    pub path_by_ext_inst_id: BTreeMap<ExtInstId, Path>,
}

fn make_net_id_by_path(circuit: &Circuit, nets: &[Net]) -> BTreeMap<Path, NetId> {
    /*
        Supply a Path and get a NetId out.
        Used extensively to build the Sim instance.
        Otherwise, it's only used for the outward peek() and poke() calls
        through the net_id() helper.

        The definition just takes each path and finds which net contains it.
    */
    circuit
        .paths()
        .iter()
        .map(|path| {
            for (net_id, net) in nets.iter().enumerate() {
                if net.contains(path.clone()) {
                    return (path.clone(), net_id);
                }
            }
            unreachable!()
        })
        .collect()
}

fn make_regs(circuit: &Circuit, net_id_by_path: &BTreeMap<Path, NetId>) -> Vec<RegInfo> {
    /*
        Straightforward resolution of the Circuit data to net data for all registers.
    */
    circuit
        .regs()
        .iter()
        .cloned()
        .map(|path| {
            let set_net_id = net_id_by_path[&path.set()];
            let val_net_id = net_id_by_path[&path.clone()];
            let reset = circuit.reset_for_reg(path);

            RegInfo {
                set_net_id,
                val_net_id,
                reset,
            }
        })
        .collect()
}

fn make_combs(circuit: &Circuit, net_id_by_path: &BTreeMap<Path, NetId>) -> Vec<Comb> {
    /*
        Created from the Wires of the Circuit.
        Look at the WireType and decide if we need to capture the val or set terminal.
        Converts everything to nets.
        Don't add a comb when the two point at the same net.
        (To avoid errors from the ad-hoc optimization).
    */
    circuit
        .wires()
        .iter()
        .cloned()
        .map(|(path, Wire(_loc, target, expr, wiretype))| {
            let abs_target = path.clone().join(target);
            let abs_expr = expr.rebase(path.clone());
            let target_net_id = match wiretype {
                WireType::Direct => net_id_by_path[&abs_target],
                WireType::Latch => net_id_by_path[&abs_target.set()],
                _ => todo!(), // Proc Wires not handled
            };
            (target_net_id, abs_expr.references_to_nets(&net_id_by_path), wiretype)
        })
        .filter(|(target_net_id, expr, _wiretype)| {
            if let Expr::Net(_loc, _typ, net_id) = &**expr {
                target_net_id != net_id
            } else {
                true
            }
        })
        .map(|(target_net_id, expr, _wiretype)| {
            Comb(target_net_id, expr)
        })
        .collect()
}

fn make_ext_inst_id_by_path(
    circuit: &Circuit,
    net_id_by_path: &BTreeMap<Path, NetId>,
    nets: &[Net],
) -> (BTreeMap<Path, ExtInstId>, BTreeMap<ExtInstId, Path>) {
    let mut ext_inst_id_by_path: BTreeMap<Path, ExtInstId> = BTreeMap::new();
    let mut path_by_ext_inst_id: BTreeMap<ExtInstId, Path> = BTreeMap::new();
    let mut ext_dependencies = vec![vec![]; nets.len()];

    for (ext_id, (path, ext_component)) in circuit.exts().iter().enumerate() {
        ext_inst_id_by_path.insert(path.clone(), ext_id);
        path_by_ext_inst_id.insert(ext_id, path.clone());

        for child in ext_component.children() {
            if let Component::Incoming(_loc, name, _typ) = &*child {
                let incoming_path = path.join(name.to_string().into());
                let net_id = net_id_by_path[&incoming_path];
                ext_dependencies[net_id].push((ext_id, name.to_string()));
            }
        }
    }

    (ext_inst_id_by_path, path_by_ext_inst_id)
}

fn make_dependents(
    circuit: &Circuit,
    net_ids: &[NetId],
    combs: &[Comb],
    net_id_by_path: &BTreeMap<Path, NetId>,
    ext_id_by_path: &BTreeMap<Path, ExtInstId>,
) -> Vec<Dependents> {
    net_ids.iter()
    .map(|net_id| {
        let combs: Vec<CombId> = combs.iter().enumerate().filter(|(_comb_id, comb)| comb.depends_on(*net_id)).map(|(comb_id, _comb)| comb_id).collect();
        let mut ext_ports: Vec<(ExtInstId, PortName)> = vec![];
        for (path, ext_component) in circuit.exts() {
            let ext_id = ext_id_by_path[&path];
            match &*ext_component {
                Component::Ext(_loc, _name, children) => {
                    for child in children {
                        match &**child {
                            Component::Incoming(_loc, name, _typ) => {
                                let port_path = path.join(name.clone().into());
                                let port_net_id = net_id_by_path[&port_path];
                                if  port_net_id == *net_id {
                                    ext_ports.push((ext_id, name.to_string()))
                                }
                            },
                            _ => (),
                        }
                    }
                },
                _ => unreachable!(),
            }
        }

        let dependents = Dependents {
            combs,
            ext_inst_ports: ext_ports,
        };
        dependents
    })
    .collect()
}

fn make_net_id_by_ext_port(
    circuit: &Circuit,
    net_id_by_path: &BTreeMap<Path, NetId>,
    ext_id_by_path: &BTreeMap<Path, ExtInstId>,
) -> BTreeMap<(ExtInstId, PortName), NetId> {
    let mut net_id_by_ext_port = BTreeMap::new();

    for (path, ext_component) in circuit.exts() {
        let ext_id = ext_id_by_path[&path];
        match &*ext_component {
            Component::Ext(_loc, _name, children) => {
                for child in children {
                    match &**child {
                        Component::Outgoing(_loc, name, _typ) => {
                            let port_path = path.join(name.clone().into());
                            let net_id = net_id_by_path[&port_path];
                            net_id_by_ext_port.insert((ext_id, name.clone()), net_id);
                        },
                        _ => (),
                    }
                }
            },
            _ => unreachable!(),
        }
    }
    net_id_by_ext_port
}

impl SimCircuit {
    pub fn new(circuit: &Circuit) -> SimCircuit {
        let nets = nets(circuit);
        let net_ids: Vec<NetId> = (0..nets.len()).into_iter().collect();
        let net_id_by_path: BTreeMap<Path, NetId> = make_net_id_by_path(&circuit, &nets);
        let regs: Vec<RegInfo> = make_regs(&circuit, &net_id_by_path);
        let combs: Vec<Comb> = make_combs(&circuit, &net_id_by_path);
        let (ext_inst_id_by_path, path_by_ext_inst_id) = make_ext_inst_id_by_path(&circuit, &net_id_by_path, &nets);

        let dependents: Vec<Dependents> = make_dependents(
            &circuit,
            &net_ids,
            &combs,
            &net_id_by_path,
            &ext_inst_id_by_path,
        );

        let net_id_by_ext_port = make_net_id_by_ext_port(
            &circuit,
            &net_id_by_path,
            &ext_inst_id_by_path,
        );

        SimCircuit {
            nets,
            combs,
            regs,

            dependents,
            net_id_by_ext_port,

            net_id_by_path,
            ext_inst_id_by_path,
            path_by_ext_inst_id,
        }
    }

    pub fn net_ids(&self) -> Vec<NetId> {
        (0..self.nets.len()).into_iter().collect()
    }
}

pub struct Sim {
    sim_circuit: Arc<SimCircuit>,
    net_values: Vec<Value>,
    exts: Vec<Box<dyn Ext>>,
    ext_id_by_ext_inst_id: BTreeMap<ExtInstId, ExtId>,
    clock_ticks: u64,
    start_time: SystemTime,
    clock_freq_cap: Option<f64>,
}

impl Sim {
    pub fn new(circuit: &Circuit, exts: Vec<Box<dyn Ext>>) -> Sim {
        let sim_circuit = Arc::new(SimCircuit::new(circuit));
        let net_ids = sim_circuit.net_ids();
        let net_values: Vec<Value> = net_ids.iter().map(|_net| Value::X).collect();

        let ext_id_by_ext_inst_id: BTreeMap<ExtInstId, ExtId> = BTreeMap::new();

        let mut sim = Sim {
            sim_circuit,
            net_values,
            exts,
            ext_id_by_ext_inst_id,
            start_time: SystemTime::now(),
            clock_ticks: 0,
            clock_freq_cap: None,
        };

        for (ext_inst_id, (path, ext_component)) in circuit.exts().iter().enumerate() {
            let ext_name = ext_component.name();
            let ext_id = sim.ext_id_by_name(&ext_name);
            sim.ext_id_by_ext_inst_id.insert(ext_inst_id, ext_id);
            sim.instantiate_ext(path.clone(), ext_id);
        }
        sim.broadcast_update_constants();
        sim
    }

    fn ext_id_by_name(&self, name: &str) -> ExtId {
        for (ext_id, ext) in self.exts.iter().enumerate() {
            if ext.name() == name {
                return ext_id;
            }
        }
        panic!("No such ext: {name}")
    }

    pub fn net_values(&self) -> BTreeMap<NetId, Value> {
        self.net_values.iter().cloned().enumerate().collect()
    }

    pub fn net(&self, net_id: NetId) -> &Net {
        &self.sim_circuit.nets[net_id]
    }

    pub fn cap_clock_freq(mut self, freq: f64) -> Self {
        self.clock_freq_cap = Some(freq);
        self
    }

    fn instantiate_ext<P: Into<Path>>(&mut self, path: P, ext_id: ExtId) {
        let path: Path = path.into();
        let ext = &mut self.exts[ext_id];
        ext.instantiate(path.clone());
    }

    pub(crate) fn poke_net(&mut self, net_id: NetId, value: Value) {
        self.net_values[net_id] = value.clone();

        // update dependent nets through all combs
        let dependents = &self.sim_circuit.clone().dependents[net_id];
        for comb_id in dependents.combs.iter() {
            let comb = &self.sim_circuit.clone().combs[*comb_id];
            let Comb(target_net_id, expr) = comb;
            let value = expr.eval(&self);
            self.poke_net(*target_net_id, value);
        }

        for (ext_id, port_name) in dependents.ext_inst_ports.iter() {
            let ext = &mut *self.exts[*ext_id];
            let path = self.sim_circuit.path_by_ext_inst_id[ext_id].clone();
            for (updated_port_name, updated_value) in ext.update(path, port_name, value.clone()) {
                let net_id = self.sim_circuit.net_id_by_ext_port[&(*ext_id, updated_port_name)];
                self.poke_net(net_id, updated_value);
            }
        }
    }

    pub(crate) fn peek_net(&self, net_id: NetId) -> Value {
        self.net_values[net_id].clone()
    }

    fn net_id(&self, path: Path) -> NetId {
        if let Some(net_id) = self.sim_circuit.net_id_by_path.get(&path) {
            *net_id
        } else {
            panic!("No net for {path}")
        }
    }

    pub fn peek<P: Into<Path>>(&self, path: P) -> Value {
        let net_id = self.net_id(path.into());
        self.net_values[net_id].clone()
    }

    pub fn poke<P: Into<Path>>(&mut self, path: P, value: Value) {
        let net_id = self.net_id(path.into());
        self.poke_net(net_id, value);
    }

    pub fn type_of<P: Into<Path>>(&self, path: P) -> Type {
        let net_id = self.net_id(path.into());
        let Net(_driver, _terminals, typ) = &self.sim_circuit.nets[net_id];
        typ.clone()
    }

    fn broadcast_update_constants(&mut self) {
        for Comb(target_net_id, expr) in self.sim_circuit.clone().combs.iter() {
            if expr.is_constant() {
                let value = expr.eval(&self);
                self.poke_net(*target_net_id, value);
            }
        }
    }

    pub fn clock(&mut self) {
        self.clock_ticks += 1;

        // frequency cap
        if let Some(clock_freq_cap) = self.clock_freq_cap {
            let mut clock_freq = self.clocks_per_second();
            while clock_freq.is_finite() && clock_freq > clock_freq_cap {
                clock_freq = self.clocks_per_second();
            }
        }

//        if self.clock_ticks % 10000 == 0 {
//            eprintln!("CPS: {:.2}", self.clocks_per_second());
//        }

        let mut updates = vec![];
        for reginfo in &self.sim_circuit.clone().regs {
            let value = self.peek_net(reginfo.set_net_id);
            updates.push((reginfo.val_net_id, value));
        }
        for (val_net_id, value) in updates {
            self.poke_net(val_net_id, value);
        }

        for (ext_inst_id, path) in &self.sim_circuit.path_by_ext_inst_id {
            let ext_id = self.ext_id_by_ext_inst_id[ext_inst_id];
            let ext = &mut self.exts[ext_id];
            ext.clock(path.clone());
        }
    }

    pub fn reset(&mut self) {
        for reginfo in &self.sim_circuit.clone().regs {
            if let Some(reset) = &reginfo.reset {
                self.poke_net(reginfo.val_net_id, reset.eval(&self));
            }
        }
        for (ext_inst_id, path) in &self.sim_circuit.path_by_ext_inst_id {
            let ext_id = self.ext_id_by_ext_inst_id[ext_inst_id];
            let ext = &mut self.exts[ext_id];
            ext.reset(path.clone());
        }
    }

    pub fn clocks_per_second(&self) -> f64 {
        let end_time = SystemTime::now();
        let duration: Duration = end_time.duration_since(self.start_time).unwrap();
        1_000_000.0 * self.clock_ticks as f64 / duration.as_micros() as f64
    }
}

#[derive(Debug, Clone)]
pub struct Comb(NetId, Arc<Expr>);

impl Comb {
    pub fn depends_on(&self, net_id: NetId) -> bool {
        let Comb(_net_id, expr) = self;
        expr.depends_on_net(net_id)
    }
}

impl std::fmt::Debug for Sim {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        for (net_id, value) in self.net_values.iter().enumerate() {
            let net = &self.sim_circuit.nets[net_id];
            write!(f, "    {:>5}   ", format!("{value:?}"))?;
            writeln!(f, "{}", net.terminals().iter().map(|t| t.to_string()).collect::<Vec<String>>().join(" "))?;
        }

        Ok(())
    }
}

pub fn nets(circuit: &Circuit) -> Vec<Net> {
    let mut immediate_driver_for: BTreeMap<Path, Path> = BTreeMap::new();

    for (path, Wire(_loc, target, expr, wire_type)) in circuit.wires() {
        let abs_expr = expr.rebase(path.clone());
        let target_terminal: Path = match wire_type {
            WireType::Direct => path.join(target).clone(),
            WireType::Latch => path.join(target).set(),
            WireType::Proc => path.join(target).set(),
        };
        if let Expr::Reference(_loc, _typ, driver) = &*abs_expr {
            immediate_driver_for.insert(target_terminal.clone(), driver.clone());
         }
    }

    let mut drivers: BTreeSet<Path> = BTreeSet::new();
    for terminal in circuit.paths() {
        drivers.insert(driver_for(terminal, &immediate_driver_for));
    }

    let mut nets: BTreeMap<Path, Net> = BTreeMap::new();
    for driver in &drivers {
        let component = circuit.component(driver.clone()).unwrap();
        let typ = component.type_of().unwrap();
        nets.insert(driver.clone(), Net::from(driver.clone(), typ));
    }

    for terminal in circuit.paths() {
        let driver = driver_for(terminal.clone(), &immediate_driver_for);
        let net = nets.get_mut(&driver).unwrap();
        net.add(terminal);
    }

    let nets: Vec<Net> = nets.values().into_iter().cloned().collect();
    nets
}

fn driver_for(terminal: Path, immediate_driver_for: &BTreeMap<Path, Path>) -> Path {
    let mut driver: &Path = &terminal;
    while let Some(immediate_driver) = &immediate_driver_for.get(driver) {
        driver = immediate_driver;
    }
    driver.clone()
}

impl Net {
    fn from(terminal: Path, typ: Type) -> Net {
        Net(terminal, vec![], typ)
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

#[derive(Clone)]
pub struct Net(Path, Vec<Path>, Type);

impl std::fmt::Debug for Net {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "Net({} {})", self.0, self.1.iter().map(|path| path.to_string()).collect::<Vec<_>>().join(" "))
    }
}

impl Expr {
    pub fn rebase(&self, current_path: Path) -> Arc<Expr> {
        self.rebase_rec(current_path, &BTreeSet::new())
    }

    fn rebase_rec(&self, current_path: Path, shadowed: &BTreeSet<Path>) -> Arc<Expr> {
        Arc::new(match self {
            Expr::Reference(loc, typ, path) => {
                if !shadowed.contains(path) {
                    Expr::Reference(loc.clone(), typ.clone(), current_path.join(path.clone()))
                } else {
                    self.clone()
                }
            },
            Expr::Net(_loc, _typ, _net_id) => panic!("rebase() only works on reference expressions."),
            Expr::Word(_loc, _typ, _width, _value) => self.clone(),
            Expr::Enum(_loc, _typ, _typedef, _name) => self.clone(),
            Expr::Ctor(loc, typ, name, es) => Expr::Ctor(loc.clone(), typ.clone(), name.clone(), es.iter().map(|e| e.rebase_rec(current_path.clone(), shadowed)).collect()),
            Expr::Struct(loc, typ, fields) => {
                Expr::Struct(
                    loc.clone(),
                    typ.clone(),
                    fields
                        .iter()
                        .map(|(name, e)| (name.clone(), e.rebase_rec(current_path.clone(), shadowed)))
                        .collect(),
                    )
            },
            Expr::Let(loc, typ, name, e, b) => {
                let new_e = e.rebase_rec(current_path.clone(), shadowed);
                let mut new_shadowed = shadowed.clone();
                new_shadowed.insert(name.clone().into());
                let new_b = b.rebase_rec(current_path, &new_shadowed);
                Expr::Let(loc.clone(), typ.clone(), name.clone(), new_e, new_b)
            },
            Expr::Match(loc, typ, e, arms) => {
                let new_arms = arms.iter().map(|MatchArm(pat, e)| {
                    let mut new_shadowed = shadowed.clone();
                    new_shadowed.extend(pat.bound_vars());
                    MatchArm(pat.clone(), e.rebase_rec(current_path.clone(), &new_shadowed))

                }).collect();
                Expr::Match(
                    loc.clone(),
                    typ.clone(),
                    e.rebase_rec(current_path.clone(), shadowed),
                    new_arms,
                )
            },
            Expr::UnOp(loc, typ, op, e) => Expr::UnOp(loc.clone(), typ.clone(), *op, e.rebase_rec(current_path, shadowed)),
            Expr::BinOp(loc, typ, op, e1, e2) => {
                Expr::BinOp(
                    loc.clone(),
                    typ.clone(),
                    *op,
                    e1.rebase_rec(current_path.clone(), shadowed),
                    e2.rebase_rec(current_path, shadowed),
                )
            },
            Expr::If(loc, typ, cond, e1, e2) => {
                Expr::If(
                    loc.clone(),
                    typ.clone(),
                    cond.rebase_rec(current_path.clone(), shadowed),
                    e1.rebase_rec(current_path.clone(), shadowed),
                    e2.rebase_rec(current_path, shadowed),
                )
            },
            Expr::Mux(loc, typ, cond, e1, e2) => {
                Expr::Mux(
                    loc.clone(),
                    typ.clone(),
                    cond.rebase_rec(current_path.clone(), shadowed),
                    e1.rebase_rec(current_path.clone(), shadowed),
                    e2.rebase_rec(current_path, shadowed),
                )
            },
            Expr::Cat(loc, typ, es) => {
                Expr::Cat(
                    loc.clone(),
                    typ.clone(),
                    es.iter().map(|e| e.rebase_rec(current_path.clone(), shadowed)).collect(),
                )
            },
            Expr::Sext(loc, typ, e) => Expr::Sext(loc.clone(), typ.clone(), e.rebase_rec(current_path, shadowed)),
            Expr::Zext(loc, typ, e) => Expr::Zext(loc.clone(), typ.clone(), e.rebase_rec(current_path, shadowed)),
            Expr::ToWord(loc, typ, e) => Expr::ToWord(loc.clone(), typ.clone(), e.rebase_rec(current_path, shadowed)),
            Expr::Vec(loc, typ, es) => Expr::Vec(loc.clone(), typ.clone(), es.iter().map(|e| e.rebase_rec(current_path.clone(), shadowed)).collect()),
            Expr::IdxField(loc, typ, e, field) => Expr::IdxField(loc.clone(), typ.clone(), e.rebase_rec(current_path, shadowed), field.clone()),
            Expr::Idx(loc, typ, e, i) => Expr::Idx(loc.clone(), typ.clone(), e.rebase_rec(current_path, shadowed), *i),
            Expr::IdxRange(loc, typ, e, j, i) => Expr::IdxRange(loc.clone(), typ.clone(), e.rebase_rec(current_path, shadowed), *j, *i),
            Expr::Call(loc, typ, fndef, es) => {
                Expr::Call(
                    loc.clone(),
                    typ.clone(),
                    fndef.clone(),
                    es.iter().map(|e| e.rebase_rec(current_path.clone(), shadowed)).collect(),
                )
            },
            Expr::Hole(loc, typ, name) => Expr::Hole(loc.clone(), typ.clone(), name.clone()),
        })
    }

    fn references_to_nets(&self, net_id_by_path: &BTreeMap<Path, NetId>) -> Arc<Expr> {
        self.references_to_nets_rec(net_id_by_path, &BTreeSet::new())
    }

    fn references_to_nets_rec(&self, net_id_by_path: &BTreeMap<Path, NetId>, shadowed: &BTreeSet<Path>) -> Arc<Expr> {
        Arc::new(match self {
            Expr::Reference(loc, typ, path) => {
                if !shadowed.contains(path) {
                    Expr::Net(loc.clone(), typ.clone(), net_id_by_path[path])
                } else {
                    self.clone()
                }
            },
            Expr::Net(_loc, _typ, _net_id) => panic!("references_to_nets() only works on reference expressions."),
            Expr::Word(_loc, _typ, _width, _value) => self.clone(),
            Expr::Enum(_loc, _typ, _typedef, _name) => self.clone(),
            Expr::Ctor(loc, typ, name, es) => {
                Expr::Ctor(
                    loc.clone(),
                    typ.clone(),
                    name.clone(),
                    es.iter().map(|e| e.references_to_nets_rec(net_id_by_path, shadowed)).collect(),
                )
            },
            Expr::Struct(loc, typ, fields) => {
                Expr::Struct(
                    loc.clone(),
                    typ.clone(),
                    fields
                        .iter()
                        .map(|(name, e)| (name.clone(), e.references_to_nets_rec(net_id_by_path, shadowed)))
                        .collect(),
                    )
            },
            Expr::Let(loc, typ, name, e, b) => {
                let new_e = e.references_to_nets_rec(net_id_by_path, shadowed);
                let mut new_shadowed = shadowed.clone();
                new_shadowed.insert(name.clone().into());
                let new_b = b.references_to_nets_rec(net_id_by_path, &new_shadowed);
                Expr::Let(loc.clone(), typ.clone(), name.clone(), new_e, new_b)
            },
            Expr::UnOp(loc, typ, op, e) => Expr::UnOp(loc.clone(), typ.clone(), *op, e.references_to_nets_rec(net_id_by_path, shadowed)),
            Expr::BinOp(loc, typ, op, e1, e2) => {
                Expr::BinOp(
                    loc.clone(),
                    typ.clone(),
                    *op,
                    e1.references_to_nets_rec(net_id_by_path, shadowed),
                    e2.references_to_nets_rec(net_id_by_path, shadowed),
                )
            },
            Expr::If(loc, typ, cond, e1, e2) => {
                Expr::If(
                    loc.clone(),
                    typ.clone(),
                    cond.references_to_nets_rec(net_id_by_path, shadowed),
                    e1.references_to_nets_rec(net_id_by_path, shadowed),
                    e2.references_to_nets_rec(net_id_by_path, shadowed),
                )
            },
            Expr::Match(loc, typ, e, arms) => {
                let new_arms = arms.iter().map(|MatchArm(pat, e)| {
                    let mut new_shadowed = shadowed.clone();
                    new_shadowed.extend(pat.bound_vars());
                    MatchArm(pat.clone(), e.references_to_nets_rec(net_id_by_path, &new_shadowed))

                }).collect();
                Expr::Match(
                    loc.clone(),
                    typ.clone(),
                    e.references_to_nets_rec(net_id_by_path, shadowed),
                    new_arms,
                )
            },
            Expr::Mux(loc, typ, cond, e1, e2) => {
                Expr::Mux(
                    loc.clone(),
                    typ.clone(),
                    cond.references_to_nets_rec(net_id_by_path, shadowed),
                    e1.references_to_nets_rec(net_id_by_path, shadowed),
                    e2.references_to_nets_rec(net_id_by_path, shadowed),
                )
            },
            Expr::Cat(loc, typ, es) => {
                Expr::Cat(
                    loc.clone(),
                    typ.clone(),
                    es.iter().map(|e| e.references_to_nets_rec(net_id_by_path, shadowed)).collect(),
                )
            },
            Expr::Sext(loc, typ, e) => Expr::Sext(loc.clone(), typ.clone(), e.references_to_nets_rec(net_id_by_path, shadowed)),
            Expr::Zext(loc, typ, e) => Expr::Zext(loc.clone(), typ.clone(), e.references_to_nets_rec(net_id_by_path, shadowed)),
            Expr::ToWord(loc, typ, e) => Expr::ToWord(loc.clone(), typ.clone(), e.references_to_nets_rec(net_id_by_path, shadowed)),
            Expr::Vec(loc, typ, es) => Expr::Vec(loc.clone(), typ.clone(), es.iter().map(|e| e.references_to_nets_rec(net_id_by_path, shadowed)).collect()),
            Expr::IdxField(loc, typ, e, field) => Expr::IdxField(loc.clone(), typ.clone(), e.references_to_nets_rec(net_id_by_path, shadowed), field.clone()),
            Expr::Idx(loc, typ, e, i) => Expr::Idx(loc.clone(), typ.clone(), e.references_to_nets_rec(net_id_by_path, shadowed), *i),
            Expr::IdxRange(loc, typ, e, j, i) => Expr::IdxRange(loc.clone(), typ.clone(), e.references_to_nets_rec(net_id_by_path, shadowed), *j, *i),
            Expr::Call(loc, typ, fndef, es) => {
                Expr::Call(
                    loc.clone(),
                    typ.clone(),
                    fndef.clone(),
                    es.iter().map(|e| e.references_to_nets_rec(net_id_by_path, shadowed)).collect(),
                )
            },
            Expr::Hole(loc, typ, name) => Expr::Hole(loc.clone(), typ.clone(), name.clone()),
        })
    }
}
