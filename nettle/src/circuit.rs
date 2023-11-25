use super::*;

pub type Name = String;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WireType {
    Connect,
    Latch,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Wire(pub Path, pub Expr, pub WireType);

impl Wire {
    pub fn new(target: Path, expr: Expr, typ: WireType) -> Wire {
        Wire(target, expr, typ)
    }

    pub fn rebase(self, base: Path) -> Wire {
        let Wire(target, expr, typ) = self;
        Wire(base.join(target), expr.rebase(base), typ)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Component {
    Top(Vec<Component>, Vec<Wire>),
    Mod(Name, Vec<Component>, Vec<Wire>),
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
    pub fn new(component: Component) -> Circuit {
        Circuit(Arc::new(component))
    }

    pub fn modules(&self) -> Vec<Path> {
        let path: Path = "top".into();
        let mut results = vec![path.clone()];
        for child in self.children() {
            if let Component::Mod(name, _children, _wires) = child {
                results.extend(child.modules_rec(path.join(name.clone().into())));
            }
        }
        results
    }

    pub fn component(&self, path: Path) -> Option<&Component> {
        if path == "top".into() {
            return Some(&self.0);
        }
        assert!(path.starts_with("top."));
        // strip "top." from the front
        let path: Path = path[4..].into();
        self.0.component(path)
    }

    pub fn wires(&self) -> Vec<Wire> {
        self.0.wires_rec("top".into())
    }

    fn walk(&self) -> Vec<(Path, &Component)> {
        self.0.walk_rec("top".into())
    }

    pub fn regs(&self) -> Vec<Path> {
        let mut results = vec![];
        for (path, component) in self.walk() {
            if let Component::Reg(_name, _typ, _reset) = component {
                results.push(path);
            }
        }
        results
    }

    pub fn terminals(&self) -> Vec<Path> {
        self.0.terminals_rec("top".into())
    }

    pub fn reset_for_reg(&self, path: Path) -> Option<Value> {
        if let Some(Component::Reg(_name, _typ, reset)) = self.component(path) {
            Some(*reset)
        } else {
            None
        }
    }

    pub fn check(&self) -> Result<(), Vec<(Path, CircuitError)>> {
        let mut errors = vec![];
        for (path, component) in self.walk() {
            if let Err(component_errors) = component.check() {
                for component_error in component_errors {
                    errors.push((path.clone(), component_error));
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
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
            Component::Reg(name, _typ, _value) => name.as_str(),
        }
    }

    fn component(&self, path: Path) -> Option<&Component> {
        let mut result: &Component = &self;
        for part in path.split(".") {
            result = result.child(part)?;
        }
        Some(result)
    }

    fn child(&self, name: &str) -> Option<&Component> {
        for child in self.children() {
            if child.name() == name {
                return Some(child);
            }
        }
        None
    }

    fn check(&self) -> Result<(), Vec<CircuitError>> {
        let mut errors = vec![];

        match self {
            Component::Top(_children, _wires) => {
                errors.extend(self.check_wires_no_such_component());
                errors.extend(self.check_children_duplicate_names());
                errors.extend(self.check_wires_duplicate_targets());
                errors.extend(self.check_missing_drivers());
                errors.extend(self.check_wires_wiretype());
                errors.extend(self.check_incoming_port_driven());
            },
            Component::Mod(_name, _children, _wires) => {
                errors.extend(self.check_wires_no_such_component());
                errors.extend(self.check_children_duplicate_names());
                errors.extend(self.check_wires_duplicate_targets());
                errors.extend(self.check_missing_drivers());
                errors.extend(self.check_wires_wiretype());
                errors.extend(self.check_incoming_port_driven());
            },
            Component::Ext(_name, children) => {
                for component in children {
                    if !component.is_port() {
                        errors.push(CircuitError::ExtHasNonPort(component.name().to_string()));
                    }
                }
            },
            _ => (),
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn check_children_duplicate_names(&self) -> Vec<CircuitError> {
        let mut errors = vec![];
        let mut seen = BTreeSet::new();
        for child in self.children() {
            if !seen.contains(child.name()) {
                seen.insert(child.name());
            } else {
                errors.push(CircuitError::DuplicateComponent(child.name().to_string()));
            }
        }
        errors

    }

    fn check_wires_duplicate_targets(&self) -> Vec<CircuitError> {
        let mut errors = vec![];
        let mut seen = BTreeSet::new();
        for Wire(target, _expr, _typ) in &self.wires() {
            if !seen.contains(target) {
                seen.insert(target);
            } else {
                errors.push(CircuitError::MultipleDrivers(target.to_string()));
            }
        }
        errors
    }

    fn check_wires_no_such_component(&self) -> Vec<CircuitError> {
        let mut errors = vec![];

        for Wire(target, _expr, _wiretype) in &self.wires() {
            if self.component(target.clone()).is_none() {
                errors.push(CircuitError::NoSuchComponent(target.to_string()));
            }
        }
        errors
    }

    fn check_wires_wiretype(&self) -> Vec<CircuitError> {
        let mut errors = vec![];

        for Wire(target, _expr, wiretype) in &self.wires() {
            if let Some(component) = self.component(target.clone()) {
                match (component, wiretype) {
                    (Component::Reg(name, _typ, _reset), WireType::Connect) =>
                        errors.push(CircuitError::WrongWireType(name.clone(), WireType::Connect)),
                    (Component::Node(name, _typ), WireType::Latch) =>
                        errors.push(CircuitError::WrongWireType(name.clone(), WireType::Latch)),
                    (Component::Outgoing(name, _typ), WireType::Latch) =>
                        errors.push(CircuitError::WrongWireType(name.clone(), WireType::Latch)),
                    (_, _) => (),
                }
            }
        }
        errors
    }

    fn check_incoming_port_driven(&self) -> Vec<CircuitError> {
        let mut errors = vec![];

        for Wire(target, _expr, _wiretype) in &self.wires() {
            if let Some(component) = self.component(target.clone()) {
                let is_local = !target.contains(".");
                if is_local {
                    match component {
                        Component::Incoming(name, _typ) =>
                            errors.push(CircuitError::IncomingPortDriven(name.clone())),
                        _ => (),
                    }
                }
            }
        }
        errors
    }

    fn check_missing_drivers(&self) -> Vec<CircuitError> {
        let mut errors = vec![];
        let mut terminals_remaining: BTreeSet<Path> = self.visible_terminals().into_iter().collect();

        for Wire(target, _expr, _typ) in &self.wires() {
            terminals_remaining.remove(target);
        }

        for terminal in terminals_remaining.into_iter() {
            let is_incoming_port = if let Some(Component::Incoming(_name, _typ)) = &self.component(terminal.clone()) {
                true
            } else {
                false
            };

            let is_outgoing_port = if let Some(Component::Outgoing(_name, _typ)) = &self.component(terminal.clone()) {
                true
            } else {
                false
            };

            let is_local = !terminal.contains(".");

            if !is_local && is_incoming_port {
                errors.push(CircuitError::NoDrivers(terminal.to_string()));
            } else if is_local && is_outgoing_port {
                errors.push(CircuitError::NoDrivers(terminal.to_string()));
            }
        }
        errors
    }

    pub fn is_port(&self) -> bool {
        match self {
            Component::Incoming(_name, _typ) => true,
            Component::Outgoing(_name, _typ) => true,
            _ => false,
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
            Component::Reg(_name, _typ, _value) => vec![],
        }
    }

    fn walk_rec(&self, path: Path) -> Vec<(Path, &Component)> {
        let mut results = vec![(path.clone(), self)];
        for child in self.children() {
            results.extend(child.walk_rec(path.join(child.name().into())));
        }
        results
    }

    fn modules_rec(&self, current_path: Path) -> Vec<Path> {
        let mut results = vec![current_path.clone()];
        for child in self.children() {
            if let Component::Mod(name, _children, _wires) = child {
                results.extend(child.modules_rec(current_path.join(name.clone().into())));
            }
        }
        results
    }

    pub fn wires(&self) -> Vec<Wire> {
        match self {
            Component::Top(_children, wires) => wires.clone(),
            Component::Mod(_name, _children, wires) => wires.clone(),
            _ => vec![],
        }
    }

    fn wires_rec(&self, path: Path) -> Vec<Wire> {
        let mut results: Vec<Wire> = self.wires().into_iter().map(|wire| wire.rebase(path.clone())).collect();
        for child in self.children() {
            results.extend(child.wires_rec(path.join(child.name().into())));
        }
        results
    }

    fn terminals_rec(&self, path: Path) -> Vec<Path> {
        let mut results = vec![];
        for child in self.children() {
            match child {
                Component::Top(_children, _wires) => panic!("No component should contain a Top."),
                Component::Node(name, _typ) => results.push(path.join(name.clone().into())),
                Component::Reg(name, _typ, _reset) => {
                    results.push(path.join(format!("{name}.set").into()));
                    results.push(path.join(name.clone().into()));
                },
                Component::Incoming(name, _typ) => results.push(path.join(name.clone().into())),
                Component::Outgoing(name, _typ) => results.push(path.join(name.clone().into())),
                Component::Mod(name, _children, _wires) => results.extend(child.terminals_rec(path.join(name.clone().into()))),
                Component::Ext(name, _children) => results.extend(child.terminals_rec(path.join(name.clone().into()))),
            }
        }
        results
    }

    fn port_paths(&self) -> Vec<Path> {
        let mut results = vec![];
        for child in self.children() {
            match child {
                Component::Incoming(name, _typ) => results.push(name.to_string().into()),
                Component::Outgoing(name, _typ) => results.push(name.to_string().into()),
                _ => (),
            }
        }
        results
    }

    fn visible_terminals(&self) -> Vec<Path> {
        let mut results = vec![];
        for child in self.children() {
            match child {
                Component::Top(_children, _wires) => panic!("No component should contain a Top."),
                Component::Node(name, _typ) => results.push(name.to_string().into()),
                Component::Incoming(name, _typ) => results.push(name.to_string().into()),
                Component::Outgoing(name, _typ) => results.push(name.to_string().into()),
                Component::Mod(name, _children, _wires) => {
                    let mod_path: Path = name.to_string().into();
                    for path in child.port_paths() {
                        results.push(mod_path.join(path));
                    }
                },
                _ => (),
            }
        }
        results
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CircuitError {
    ExtHasNonPort(Name),
    DuplicateComponent(Name),
    MultipleDrivers(Name),
    NoDrivers(Name),
    WrongWireType(Name, WireType),
    IncomingPortDriven(Name),
    NoSuchComponent(Name),
}
