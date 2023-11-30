use super::*;

use anyhow::anyhow;

pub type Name = String;

#[derive(Debug, Clone)]
pub struct Package(pub Vec<Decl>);

impl Package {
    fn typedef(&self, name: &str) -> Option<Arc<TypeDef>> {
        for decl in &self.0 {
            if let Decl::TypeDef(typedef) = &decl {
                if typedef.name == name {
                    return Some(typedef.clone());
                }
            }
        }
        None
    }
}

#[derive(Debug, Clone)]
pub enum Decl {
    Mod(Arc<Component>),
    TypeDef(Arc<TypeDef>),
}

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
    Mod(Name, Vec<Component>, Vec<Wire>),
    Ext(Name, Vec<Component>),
    Incoming(Name, Type),
    Outgoing(Name, Type),
    Node(Name, Type),
    Reg(Name, Type, Value),
}

#[derive(Debug, Clone)]
pub struct Circuit(Arc<Package>);

impl std::ops::Deref for Circuit {
    type Target = Component;
    fn deref(&self) -> &Component {
        let Package(decls) = &*self.0;
        for decl in decls {
            if let Decl::Mod(m) = decl {
                return m;
            }
        }
        panic!("No top")
    }
}

impl Circuit {
    pub fn new(package: Package) -> Circuit {
        let mut circuit = Circuit(Arc::new(package));
        circuit.resolve_references();
        circuit
    }

    pub fn top(&self) -> &Component {
        let Package(decls) = &*self.0;
        for decl in decls {
            if let Decl::Mod(m) = decl {
                return m;
            }
        }
        panic!("No top")
    }

    pub fn root(&self) -> Path {
        self.top().name().into()
    }

    pub fn modules(&self) -> Vec<Path> {
        let path: Path = self.top().name().into();
        let mut results = vec![path.clone()];
        for child in self.children() {
            if let Component::Mod(name, _children, _wires) = child {
                results.extend(child.modules_rec(path.join(name.clone().into())));
            }
        }
        results
    }

    pub fn component(&self, path: Path) -> Option<&Component> {
        let root: Path = self.top().name().into();
        if path == root {
            return Some(&self.top());
        }
        let root_prefix = format!("{root}.");
        if !path.starts_with(&root_prefix) {
            eprintln!("{path} does not start with {root_prefix})");
        }
        assert!(path.starts_with(&root_prefix));
        let path: Path = path[root_prefix.len()..].into();
        self.top().component(path)
    }

    fn resolve_references(&mut self) {
        let mut wires = self.top().wires_rec(self.top().name().into());
        for Wire(_target, expr, _wiretype) in &mut wires {
            let func = |e: &mut Expr| {
                if let Expr::Lit(Value::Enum(r, _name)) = e {
                    let typedef = self.0.typedef(r.name()).unwrap();
                    r.resolve_to(typedef).unwrap();
                }
            };
            expr.with_subexprs_mut(&func);
        }
    }

    pub fn wires(&self) -> Vec<Wire> {
        self.top().wires_rec(self.top().name().into())
    }

    fn walk(&self) -> Vec<(Path, &Component)> {
        self.top().walk_rec(self.top().name().into())
    }

    pub fn exts(&self) -> Vec<Path> {
        let mut results = vec![];
        for (path, component) in self.walk() {
            if let Component::Ext(_name, _children) = component {
                results.push(path);
            }
        }
        results
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
        self.top().terminals_rec(self.top().name().into())
    }

    pub fn reset_for_reg(&self, path: Path) -> Option<Value> {
        if let Some(Component::Reg(_name, _typ, reset)) = self.component(path) {
            Some(reset.clone())
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

    pub fn context(&self) -> anyhow::Result<Context<Path, Type>> {
        let mut ctx = vec![];
        for path in self.visible_terminals() {
            let component = self.component(path.clone()).unwrap();
            let typ = component.type_of().ok_or_else(|| anyhow!("Unknown component: {path} is not visible in {}", self.name()))?;
            ctx.push((path, typ));
        }
        Ok(Context::from(ctx))
    }

    fn type_of(&self) -> Option<Type> {
        match self {
            Component::Mod(_name, _children, _wires) => None,
            Component::Ext(_name, _children) => None,
            Component::Node(_name, typ) => Some(typ.clone()),
            Component::Outgoing(_name, typ) => Some(typ.clone()),
            Component::Incoming(_name, typ) => Some(typ.clone()),
            Component::Reg(_name, typ, _reset) => Some(typ.clone()),
        }
    }

    fn check(&self) -> Result<(), Vec<CircuitError>> {
        let mut errors = vec![];

        match self {
            Component::Mod(_name, _children, _wires) => {
                errors.extend(self.check_typecheck());
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

    fn check_typecheck(&self) -> Vec<CircuitError> {
        let ctx = match self.context() {
            Ok(ctx) => ctx,
            Err(e) => return vec![CircuitError::Unknown(format!("{e:?}"))],
        };

        let mut errors = vec![];

        for Wire(target, expr, _wiretype) in &self.wires() {
            let target_typ = if let Some(typ) = ctx.lookup(target) {
                typ
            } else {
                errors.push(CircuitError::Unknown(format!("Target {target} in {} has unknown type", self.name())));
                continue;
            };

            match expr.typecheck(&target_typ, ctx.clone()) {
                Err(e) => errors.push(CircuitError::TypeError(format!("{e:?}"))),
                Ok(()) => (),
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

    pub fn children(&self) -> Vec<&Component> {
        match self {
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
                Component::Node(name, _typ) => results.push(name.to_string().into()),
                Component::Incoming(name, _typ) => results.push(name.to_string().into()),
                Component::Outgoing(name, _typ) => results.push(name.to_string().into()),
                Component::Reg(name, _typ, _reset) => results.push(name.to_string().into()),
                Component::Mod(name, _children, _wires) => {
                    let mod_path: Path = name.to_string().into();
                    for path in child.port_paths() {
                        results.push(mod_path.join(path));
                    }
                },
                Component::Ext(name, _children) => {
                    let ext_path: Path = name.to_string().into();
                    for path in child.port_paths() {
                        results.push(ext_path.join(path));
                    }
                },
            }
        }
        results
    }

    pub fn is_mod(&self) -> bool {
        match self {
            Component::Mod(_name, _children, _wires) => true,
            _ => false
        }
    }

    pub fn is_port(&self) -> bool {
        match self {
            Component::Incoming(_name, _typ) => true,
            Component::Outgoing(_name, _typ) => true,
            _ => false
        }
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
    TypeError(String),
    Unknown(String),
}
