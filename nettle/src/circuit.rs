use super::*;

pub type Name = String;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Component {
    Top(Vec<Component>, Vec<(Path, Expr)>),
    Mod(Name, Vec<Component>, Vec<(Path, Expr)>),
    Ext(Name, Vec<Component>),
    Incoming(Name, Type),
    Outgoing(Name, Type),
    Node(Name, Type),
    Reg(Name, Type, Value),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Circuit(Arc<Component>);

impl std::ops::Deref for Circuit {
    type Target = Component;
    fn deref(&self) -> &Component {
        &self.0
    }
}

impl Circuit {
    pub fn modules(&self) -> Vec<Path> {
        let mut results = vec!["top".into()];
        results.extend(self.modules_rec("top".into()));
        results
    }

    fn modules_rec(&self, current_path: Path) -> Vec<Path> {
        let mut results = vec![];
        for child in self.children() {
            if let Component::Mod(name, _children, _wires) = child {
                results.push(name.clone().into());
                self.modules_rec(current_path.join(name.clone().into()));
            }
        }
        results
    }

    pub fn new(&self) -> Self { todo!() }
    pub fn wire(&self) -> Vec<(Path, Expr)> { todo!() }
    pub fn wires(&self) -> Vec<(Path, Expr)> { todo!() }
    pub fn ext(&self, path: Path) -> Option<Component> { todo!() }
    pub fn component(&self, path: Path) -> Option<Component> { todo!() }
    pub fn components(&self) { todo!() }
    pub fn nets(&self) -> Vec<Net> { todo!() }
    pub fn reset_for_reg(&self, path: Path) -> Option<Value> { todo!() }
    pub fn regs(&self) -> Vec<Path> { todo!() }
}

impl Component {
    pub fn name(&self) -> &str {
        match self {
            Component::Top(_children, _wires) => "top",
            Component::Mod(name, _children, _wires) => name.as_str(),
            Component::Ext(name, _children) => name.as_str(),
            Component::Incoming(name, _typ) => name.as_str(),
            Component::Outgoing(name, _typ) => name.as_str(),
            Component::Node(name, _typ) => name.as_str(),
            Component::Reg(name, _typ, Value) => name.as_str(),
        }
    }

    pub fn children(&self) -> Vec<&Component> {
        match self {
            Component::Top(children, _wires) => children.iter().collect(),
            Component::Mod(_name, children, _wires) => children.iter().collect(),
            Component::Ext(_name, children) => children.iter().collect(),
            Component::Incoming(_name, _typ) => vec![],
            Component::Outgoing(_name, _typ) => vec![],
            Component::Node(_name, _typ) => vec![],
            Component::Reg(_name, _typ, Value) => vec![],
        }
    }
}




/*
    pub fn nets(&self) -> Vec<Net> {
        let mut immediate_driver_for: BTreeMap<Path, Path> = BTreeMap::new();
        for (target, expr) in self.wires() {
            if let Expr::Reference(driver) = expr {
                immediate_driver_for.insert(target.clone(), driver.clone());
            }
        }

        let mut drivers: BTreeSet<Path> = BTreeSet::new();
        for terminal in self.terminals() {
            drivers.insert(driver_for(terminal, &immediate_driver_for));
        }

        let mut nets: BTreeMap<Path, Net> = BTreeMap::new();
        for driver in &drivers {
            nets.insert(driver.clone(), Net::from(driver.clone()));
        }

        for terminal in self.terminals() {
            let driver = driver_for(terminal.clone(), &immediate_driver_for);
            let net = nets.get_mut(&driver).unwrap();
            net.add(terminal);
        }

        let nets: Vec<Net> = nets.values().into_iter().cloned().collect();
        nets
    }
}

fn driver_for(terminal: Path, immediate_driver_for: &BTreeMap<Path, Path>) -> Path {
    let mut driver: &Path = &terminal;
    while let Some(immediate_driver) = &immediate_driver_for.get(driver) {
        driver = immediate_driver;
    }
    driver.clone()
}

impl CircuitNode {
    fn push(mut self, path: &str) -> Self {
        self.path.push(path.to_string());
        self
    }

    fn pop(mut self) -> Self {
        self.path.pop();
        self
    }

    fn to_abs_path(&self, name: &str) -> Path {
        let path = self.path.join(".");
        format!("{path}.{name}").into()
    }

    pub(crate) fn node(mut self, name: &str, typ: Type) -> Self {
        let path = self.to_abs_path(name);
        self.components.insert(path.clone(), Component::Node(typ));
        self
    }

    pub(crate) fn incoming(mut self, name: &str, typ: Type) -> Self {
        let path = self.to_abs_path(name);
        self.components.insert(path, Component::Incoming(typ));
        self
    }

    pub(crate) fn outgoing(mut self, name: &str, typ: Type) -> Self {
        let path = self.to_abs_path(name);
        self.components.insert(path, Component::Outgoing(typ));
        self
    }

    pub(crate) fn reg(mut self, name: &str, typ: Type, reset: Value) -> Self {
        let path = self.to_abs_path(name);
        self.components.insert(path, Component::Reg(typ, reset));
        self
    }

    pub(crate) fn wire(mut self, name: &str, expr: &Expr) -> Self {
        let path: Path = self.to_abs_path(name).into();
        self.wires.insert(path, expr.clone().to_absolute(&self.current_path()));
        self
    }

    pub fn instantiate(mut self, name: &str, circuit: &CircuitNode) -> Self {
        let mod_path = self.current_path();
        self = self.push(name);

        for (path, component) in &circuit.components {
            if path != &"top".into() {
                let target = mod_path.join(path.clone());
                self.components.insert(target, component.clone());
            }
        }

        let mut wires = vec![];
        for (path, expr) in &circuit.wires {
            let target = mod_path.join(path.clone());
            let expr = expr.clone().to_absolute(&mod_path);
            self.wires.insert(target, expr);
        }

        self.components.insert(self.current_path(), Component::Mod);
        self = self.pop();
        self
    }

    fn current_path(&self) -> Path {
        self.path.join(".").into()
    }

    pub(crate) fn ext(mut self, name: &str, ports: &[(String, PortDirection, Type)]) -> Self {
        let ext = self.to_abs_path(name);

        for (port, dir, typ) in ports {
            let target = format!("{ext}.{port}");
            match dir {
                PortDirection::Incoming => self.components.insert(target.into(), Component::Incoming(*typ)),
                PortDirection::Outgoing => self.components.insert(target.into(), Component::Outgoing(*typ)),
            };
        }
        self.components.insert(ext, Component::Ext);
        self
    }

    pub fn build(self) -> Circuit {
        for (_path, expr) in &self.wires {
            assert!(expr.clone().is_absolute(), "{expr:?} is not absolute!");
        }
        Circuit(Arc::new(self))
    }
}
*/

impl Net {
    fn from(terminal: Path) -> Net {
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

#[derive(Debug, Clone)]
pub enum PathType {
    Node(Type),
    Incoming(Type),
    Outgoing(Type),
    Reg(Type, Value),
}

#[derive(Debug, Clone)]
pub struct Net(Path, Vec<Path>);

#[derive(Debug, Clone, Copy)]
pub enum PortDirection {
    Incoming,
    Outgoing,
}
