use super::*;
use std::collections::BTreeSet;
use std::sync::Arc;
use std::sync::Mutex;

use crate::reference::Reference;

use anyhow::anyhow;

pub type Name = String;

#[derive(Debug, Clone)]
pub struct Package(Vec<Decl>);

impl Package {
    pub fn new(decls: Vec<Decl>) -> Package {
        let package = Package(decls);
        package
    }

    pub fn moddefs(&self) -> Vec<Arc<Component>> {
        let mut results = vec![];
        for decl in &self.0 {
            if let Decl::ModDef(moddef) = &decl {
                results.push(moddef.clone());
            }
        }
        results
    }

    pub fn moddef(&self, name: &str) -> Option<Arc<Component>> {
        for decl in &self.0 {
            if let Decl::ModDef(moddef) = &decl {
                if moddef.name() == name {
                    return Some(moddef.clone());
                }
            }
        }
        None
    }

    pub fn extdef(&self, name: &str) -> Option<Arc<Component>> {
        for decl in &self.0 {
            if let Decl::ExtDef(extdef) = &decl {
                if extdef.name() == name {
                    return Some(extdef.clone());
                }
            }
        }
        None
    }

    pub fn typedef(&self, name: &str) -> Option<Arc<TypeDef>> {
        for decl in &self.0 {
            if let Decl::TypeDef(typedef) = &decl {
                if typedef.name == name {
                    return Some(typedef.clone());
                }
            }
        }
        None
    }

    pub fn resolve_references(&self) -> Result<(), Vec<CircuitError>> {
        let mut errors = vec![];
        self.resolve_references_component_types();

        // resolve references in ModInsts and ExtInsts
        for moddef in self.moddefs() {
            for child in moddef.children() {
                if let Component::ExtInst(_loc, name, reference) = &*child {
                    if let Some(extdef) = self.extdef(reference.name()) {
                        reference.resolve_to(extdef).unwrap();
                    } else {
                        errors.push(CircuitError::Unknown(format!("Undefined reference to ext: {name}")));
                    }
                } else if let Component::ModInst(_loc, name, reference) = &*child {
                    if let Some(moddef) = self.moddef(reference.name()) {
                        reference.resolve_to(moddef).unwrap();
                    } else {
                        errors.push(CircuitError::Unknown(format!("Undefined reference to mod {name}")));
                    }
                }
            }
        }

        let errors_mutex = Arc::new(Mutex::new(errors));

        // resolve references in Exprs in Wires
        for moddef in self.moddefs() {
            for Wire(_loc, _target, expr, _wiretype) in moddef.wires() {
                let func = |e: &Expr| {
                    if let Expr::Lit(_loc, Value::Enum(r, name)) = e {
                        if let Some(typedef) = self.typedef(r.name()) {
                            r.resolve_to(typedef).unwrap();
                        } else {
                            let mut errors = errors_mutex.lock().unwrap();
                            errors.push(CircuitError::Unknown(format!("Undefined reference to mod {name}")));
                        }
                    }
                };
                expr.with_subexprs(&func);
            }
        }

        let errors = errors_mutex.lock().unwrap();
        if errors.len() > 0 {
            Err(errors.to_vec())
        } else {
            Ok(())
        }
    }

    fn resolve_references_component_types(&self) {
        for moddef in self.moddefs() {
            for component in moddef.children() {
                match &*component {
                    Component::Mod(_loc, _name, _children, _wires, _whens) => (),
                    Component::ModInst(_loc, _name, _moddef) => (),
                    Component::Ext(_loc, _name, _children) => (),
                    Component::ExtInst(_loc, _name, _defname) => (),
                    Component::Node(_loc, _name, typ) => self.resolve_references_type(typ),
                    Component::Outgoing(_loc, _name, typ) => self.resolve_references_type(typ),
                    Component::Incoming(_loc, _name, typ) => self.resolve_references_type(typ),
                    Component::Reg(_loc, _name, typ, _reset) => self.resolve_references_type(typ),
                }
            }
        }
    }

    fn resolve_references_type(&self, typ: &Type) {
        match typ {
            Type::Word(_width) => (),
            Type::Vec(typ, _len) => self.resolve_references_type(typ),
            Type::TypeDef(typedef) => {
                if let Some(resolved_typedef) = self.typedef(typedef.name()) {
                    typedef.resolve_to(resolved_typedef).unwrap();
                } else {
                    panic!("No definition for typedef {}", typedef.name())
                }
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum Decl {
    ModDef(Arc<Component>),
    ExtDef(Arc<Component>),
    TypeDef(Arc<TypeDef>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WireType {
    Direct,
    Latch,
    Proc,
}

#[derive(Debug, Clone)]
pub struct Wire(pub Loc, pub Path, pub Expr, pub WireType);

#[derive(Debug, Clone)]
pub struct When(pub Expr, pub Vec<Wire>);


impl Wire {
    pub fn new(loc: Loc, target: Path, expr: Expr, typ: WireType) -> Wire {
        Wire(loc, target, expr, typ)
    }

    pub fn rebase(self, base: Path) -> Wire {
        let Wire(loc, target, expr, typ) = self;
        Wire(loc, base.join(target), expr.rebase(base), typ)
    }
}

#[derive(Debug, Clone)]
pub enum Component {
    Mod(Loc, Name, Vec<Arc<Component>>, Vec<Wire>, Vec<When>),
    ModInst(Loc, Name, Reference<Component>),
    Ext(Loc, Name, Vec<Arc<Component>>),
    ExtInst(Loc, Name, Reference<Component>),
    Incoming(Loc, Name, Type),
    Outgoing(Loc, Name, Type),
    Node(Loc, Name, Type),
    Reg(Loc, Name, Type, Expr),
}

impl HasLoc for Wire {
    fn loc(&self) -> Loc {
        let Wire(loc, _target, _expr, _wire_type) = self;
        loc.clone()
    }
}

impl HasLoc for Component {
    fn loc(&self) -> Loc {
        match self {
            Component::Mod(loc, _name, _children, _wires, _whens) => loc.clone(),
            Component::ModInst(loc, _name, _moddef) => loc.clone(),
            Component::Ext(loc, _name, _children) => loc.clone(),
            Component::ExtInst(loc, _name, _extdef) => loc.clone(),
            Component::Incoming(loc, _name, _typ) => loc.clone(),
            Component::Outgoing(loc, _name, _typ) => loc.clone(),
            Component::Node(loc, _name, _typ) => loc.clone(),
            Component::Reg(loc, _name, _typ, _expr) => loc.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Circuit(Arc<Package>, Arc<Component>);

impl Circuit {
    pub fn new(package: Package, top: &str) -> Circuit {
        let top = package.moddef(top).expect(&format!("No such mod definition: {top}"));
        let circuit = Circuit(Arc::new(package), top);
        circuit
    }

    fn package(&self) -> &Package {
        &self.0
    }

    fn top(&self) -> Arc<Component> {
        self.1.clone()
    }

    pub fn root(&self) -> Path {
        self.top().name().into()
    }

    pub fn component(&self, path: Path) -> Option<Arc<Component>> {
        let root: Path = self.top().name().into();
        if path == root {
            return Some(self.top());
        }
        let root_prefix = format!("{root}.");
        if !path.starts_with(&root_prefix) {
            eprintln!("{path} does not start with {root_prefix})");
        }
        assert!(path.starts_with(&root_prefix));
        let path: Path = path[root_prefix.len()..].into();
        self.component_from(self.top(), path)
    }

    fn component_from(&self, component: Arc<Component>, path: Path) -> Option<Arc<Component>> {
        let mut result: Arc<Component> = component;
        for part in path.split(".") {
            if let Some(child) = result.child(part) {
                if let Component::ExtInst(_loc, _name, extdef) = &*child {
                    result = extdef.get().unwrap().clone();
                } else if let Component::ModInst(_loc, _name, moddef) = &*child {
                    result = moddef.get().unwrap().clone();
                } else {
                    result = child.clone();
                }
            }
        }
        Some(result)
    }


    pub fn wires(&self) -> Vec<Wire> {
        let mut results = vec![];
        for (path, component) in self.walk_instances() {
            for Wire(_loc, target, expr, wiretype) in component.wires() {
                results.push(Wire(_loc, path.clone().join(target), expr.rebase(path.clone()), wiretype));
            }
        }
        results
    }

    fn walk_instances(&self) -> Vec<(Path, Arc<Component>)> {
        self.walk_instances_rec(self.top(), self.top().name().into())
    }

    fn walk_instances_rec(&self, component: Arc<Component>, path: Path) -> Vec<(Path, Arc<Component>)> {
        let mut results = vec![(path.clone(), component.clone())];
        for child in component.children() {
            if let Component::ModInst(_loc, name, reference) = &*child {
                let package = self.package();
                if let Some(moddef) = package.moddef(reference.name()) {
                    results.extend(self.walk_instances_rec(moddef.clone(), path.join(child.name().into())));
                } else {
                    panic!("Undefined reference to ext: {name}")
                }
            } else if let Component::ExtInst(_loc, name, reference) = &*child {
                let package = self.package();
                if let Some(extdef) = package.extdef(reference.name()) {
                    results.extend(self.walk_instances_rec(extdef.clone(), path.join(child.name().into())));
                } else {
                    panic!("Undefined reference to ext: {name}")
                }
            } else {
                results.extend(self.walk_instances_rec(child.clone(), path.join(child.name().into())));
            }
        }
        results
    }

    pub fn exts(&self) -> Vec<Path> {
        let mut results = vec![];
        for (path, component) in self.walk_instances() {
            if let Component::Ext(_loc, _name, _children) = &*component {
                results.push(path);
            }
        }
        results
    }

    pub fn regs(&self) -> Vec<Path> {
        let mut results = vec![];
        for (path, component) in self.walk_instances() {
            if let Component::Reg(_loc, _name, _typ, _reset) = &*component {
                results.push(path);
            }
        }
        results
    }

    pub fn terminals(&self) -> Vec<Path> {
        self.top().terminals_rec(self.top().name().into())
    }

    pub fn reset_for_reg(&self, path: Path) -> Option<Expr> {
        if let Component::Reg(_loc, _name, _typ, reset) = &*self.component(path)? {
            Some(reset.clone())
        } else {
            None
        }
    }

    pub fn check(&self) -> Result<(), Vec<(Path, CircuitError)>> {
        let mut errors: Vec<(Path, CircuitError)> = vec![];
        for moddef in self.package().moddefs() {
            if let Err(component_errors) = self.check_component(moddef.clone()) {
                for component_error in component_errors {
                    errors.push((moddef.name().into(), component_error));
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    pub fn context_for(&self, component: Arc<Component>) -> anyhow::Result<Context<Path, Type>> {
        let mut ctx = vec![];
        for (path, target) in self.visible_paths(component.clone()) {
//            let target = self.component_from(component.clone(), path.clone()).unwrap();
            let typ = self.type_of(target).ok_or_else(|| anyhow!("Unknown component: {path} is not visible in {}", self.top().name()))?;
            ctx.push((path, typ));
        }
        Ok(Context::from(ctx))
    }

    fn check_component(&self, component: Arc<Component>) -> Result<(), Vec<CircuitError>> {
        let mut errors = vec![];

        match &*component {
            Component::Mod(_loc, _name, _children, _wires, _whens) => {
                errors.extend(self.check_typecheck(component.clone()));
                errors.extend(self.check_wires_no_such_component(component.clone()));
                errors.extend(self.check_children_duplicate_names(component.clone()));
                errors.extend(self.check_wires_duplicate_targets(component.clone()));
                errors.extend(self.check_missing_drivers(component.clone()));
                errors.extend(self.check_wires_wiretype(component.clone()));
                errors.extend(self.check_incoming_port_driven(component.clone()));
            },
            Component::Ext(_loc, _name, children) => {
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

    fn check_children_duplicate_names(&self, component: Arc<Component>) -> Vec<CircuitError> {
        let mut errors = vec![];
        let mut seen = BTreeSet::new();
        for child in &component.children() {
            if !seen.contains(child.name()) {
                seen.insert(child.name());
            } else {
                errors.push(CircuitError::DuplicateComponent(child.name().to_string()));
            }
        }
        errors

    }

    fn check_wires_duplicate_targets(&self, component: Arc<Component>) -> Vec<CircuitError> {
        let mut errors = vec![];
        let mut seen = BTreeSet::new();
        for Wire(_loc, target, _expr, _typ) in &component.wires() {
            if !seen.contains(target) {
                seen.insert(target);
            } else {
                errors.push(CircuitError::MultipleDrivers(target.to_string()));
            }
        }
        errors
    }

    fn check_typecheck(&self, component: Arc<Component>) -> Vec<CircuitError> {
        let ctx = match self.context_for(component.clone()) {
            Ok(ctx) => ctx,
            Err(e) => return vec![CircuitError::Unknown(format!("{e:?}"))],
        };

        let mut errors = vec![];

        for Wire(loc, target, expr, _wiretype) in &component.wires() {
            let target_typ = if let Some(typ) = ctx.lookup(target) {
                typ
            } else {
                errors.push(CircuitError::NoSuchComponent(loc.clone(), target.to_string()));
                continue;
            };

            match expr.typecheck(&target_typ, ctx.clone()) {
                Err(e) => errors.push(CircuitError::TypeError(e)),
                Ok(()) => (),
            }

        }
        errors
    }

    fn check_wires_no_such_component(&self, component: Arc<Component>) -> Vec<CircuitError> {
        let mut errors = vec![];

        for Wire(loc, target, _expr, _wiretype) in &component.wires() {
            if self.component_from(component.clone(), target.clone()).is_none() {
                errors.push(CircuitError::NoSuchComponent(loc.clone(), target.to_string()));
            }
        }
        errors
    }

    fn check_wires_wiretype(&self, component: Arc<Component>) -> Vec<CircuitError> {
        let mut errors = vec![];

        for Wire(loc, target, _expr, wiretype) in &component.wires() {
            if let Some(component) = self.component_from(component.clone(), target.clone()) {
                match (&*component, wiretype) {
                    (Component::Reg(_loc, name, _typ, _reset), WireType::Direct) =>
                        errors.push(CircuitError::WrongWireType(loc.clone(), name.clone(), WireType::Direct)),
                    (Component::Node(_loc, name, _typ), WireType::Latch) =>
                        errors.push(CircuitError::WrongWireType(loc.clone(), name.clone(), WireType::Latch)),
                    (Component::Outgoing(_loc, name, _typ), WireType::Latch) =>
                        errors.push(CircuitError::WrongWireType(loc.clone(), name.clone(), WireType::Latch)),
                    (_, _) => (),
                }
            }
        }
        errors
    }

    fn check_incoming_port_driven(&self, component: Arc<Component>) -> Vec<CircuitError> {
        let mut errors = vec![];

        for Wire(_loc, target, _expr, _wiretype) in &component.wires() {
            if let Some(component) = self.component_from(component.clone(), target.clone()) {
                let is_local = !target.contains(".");
                if is_local {
                    match &*component {
                        Component::Incoming(_loc, name, _typ) =>
                            errors.push(CircuitError::IncomingPortDriven(name.clone())),
                        _ => (),
                    }
                }
            }
        }
        errors
    }

    fn check_missing_drivers(&self, component: Arc<Component>) -> Vec<CircuitError> {
        let mut errors = vec![];
        let mut terminals_remaining: Vec<(Path, Arc<Component>)> = self.visible_paths(component.clone());

        for Wire(_loc, target, _expr, _typ) in &component.wires() {
            terminals_remaining = terminals_remaining.into_iter().filter(|(path, _component)| path != target).collect();
        }

        for (path, component) in terminals_remaining.into_iter() {
            let mut is_incoming_port = false;
            let mut is_outgoing_port = false;

            if let Component::Incoming(_loc, _name, _typ) = &*component {
                is_incoming_port = true;
            } else if let Component::Incoming(_loc, _name, _typ) = &*component {
                is_outgoing_port = true;
            }

            let is_local = !path.contains(".");

            if !is_local && is_incoming_port {
                errors.push(CircuitError::NoDrivers(component.clone()));
            } else if is_local && is_outgoing_port {
                errors.push(CircuitError::NoDrivers(component.clone()));
            }
        }
        errors
    }

    fn visible_paths(&self, component: Arc<Component>) -> Vec<(Path, Arc<Component>)> {
        let mut results = vec![];
        for child in component.children() {
            match &*child {
                Component::Node(_loc, name, _typ) => results.push((name.to_string().into(), child.clone())),
                Component::Incoming(_loc, name, _typ) => results.push((name.to_string().into(), child.clone())),
                Component::Outgoing(_loc, name, _typ) => results.push((name.to_string().into(), child.clone())),
                Component::Reg(_loc, name, _typ, _reset) => results.push((name.to_string().into(), child.clone())),
                Component::Mod(_loc, name, _children, _wires, _whens) => {
                    let mod_path: Path = name.to_string().into();
                    for (path, component) in child.port_paths() {
                        results.push((mod_path.join(path), component.clone()));
                    }
                },
                Component::ModInst(_loc, name, defname) => {
                    let mod_path: Path = name.to_string().into();
                    if let Some(moddef) = defname.get() {
                        for (path, _component) in moddef.port_paths() {
                            results.push((mod_path.join(path), child.clone()));
                        }
                    } else {
                        panic!("Module {name} hasn't been resolved.")
                    }
                },
                Component::Ext(_loc, name, _children) => {
                    let ext_path: Path = name.to_string().into();
                    for (path, component) in child.port_paths() {
                        results.push((ext_path.join(path), component.clone()));
                    }
                },
                Component::ExtInst(_loc, name, defname) => {
                    let ext_path: Path = name.to_string().into();
                    if let Some(extdef) = defname.get() {
                        for (path, component) in extdef.port_paths() {
                            results.push((ext_path.join(path), component.clone()));
                        }
                    } else {
                        panic!("External module {name} hasn't been resolved.")
                    }
                },
            }
        }
        results
    }

    fn type_of(&self, component: Arc<Component>) -> Option<Type> {
        match &*component {
            Component::Mod(_loc, _name, _children, _wires, _whens) => None,
            Component::ModInst(_loc, _name, _defname) => None,
            Component::Ext(_loc, _name, _children) => None,
            Component::ExtInst(_loc, _name, _defname) => None,
            Component::Node(_loc, _name, typ) => Some(typ.clone()),
            Component::Outgoing(_loc, _name, typ) => Some(typ.clone()),
            Component::Incoming(_loc, _name, typ) => Some(typ.clone()),
            Component::Reg(_loc, _name, typ, _reset) => Some(typ.clone()),
        }
    }
}

impl Component {
    pub fn name(&self) -> &str {
        match self {
            Component::Mod(_loc, name, _children, _wires, _whens) => name.as_str(),
            Component::ModInst(_loc, name, _defname) => name.as_str(),
            Component::Ext(_loc, name, _children) => name.as_str(),
            Component::ExtInst(_loc, name, _defname) => name.as_str(),
            Component::Incoming(_loc, name, _typ) => name.as_str(),
            Component::Outgoing(_loc, name, _typ) => name.as_str(),
            Component::Node(_loc, name, _typ) => name.as_str(),
            Component::Reg(_loc, name, _typ, _value) => name.as_str(),
        }
    }

    fn child(&self, name: &str) -> Option<Arc<Component>> {
        for child in self.children() {
            if child.name() == name {
                return Some(child);
            }
        }
        None
    }

    pub fn children(&self) -> Vec<Arc<Component>> {
        match self {
            Component::Mod(_loc, _name, children, _wires, _whens) => children.iter().cloned().collect(),
            Component::ModInst(_loc, _name, _defname) => vec![],
            Component::Ext(_loc, _name, children) => children.iter().cloned().collect(),
            Component::ExtInst(_loc, _name, _defname) => vec![],
            Component::Incoming(_loc, _name, _typ) => vec![],
            Component::Outgoing(_loc, _name, _typ) => vec![],
            Component::Node(_loc, _name, _typ) => vec![],
            Component::Reg(_loc, _name, _typ, _value) => vec![],
        }
    }

    fn wires(&self) -> Vec<Wire> {
        match self {
            Component::Mod(_loc, _name, _children, wires, _whens) => wires.clone(),
            _ => vec![],
        }
    }

    fn terminals_rec(&self, path: Path) -> Vec<Path> {
        let mut results = vec![];
        for child in self.children() {
            match &*child {
                Component::Node(_loc, name, _typ) => results.push(path.join(name.clone().into())),
                Component::Reg(_loc, name, _typ, _reset) => {
                    results.push(path.join(format!("{name}.set").into()));
                    results.push(path.join(name.clone().into()));
                },
                Component::Incoming(_loc, name, _typ) => results.push(path.join(name.clone().into())),
                Component::Outgoing(_loc, name, _typ) => results.push(path.join(name.clone().into())),
                Component::Mod(_loc, name, _children, _wires, _whens) => results.extend(child.terminals_rec(path.join(name.clone().into()))),
                Component::ModInst(_loc, name, defname) => {
                    if let Some(moddef) = defname.get() {
                        results.extend(moddef.terminals_rec(path.join(name.clone().into())))
                    } else {
                        panic!("Module {name} hasn't been resolved.")
                    }
                }
                Component::Ext(_loc, name, _children) => results.extend(child.terminals_rec(path.join(name.clone().into()))),
                Component::ExtInst(_loc, name, defname) => {
                    if let Some(extdef) = defname.get() {
                        results.extend(extdef.terminals_rec(path.join(name.clone().into())))
                    } else {
                        panic!("External module {name} hasn't been resolved.")
                    }
                }
            }
        }
        results
    }

    fn port_paths(&self) -> Vec<(Path, Arc<Component>)> {
        let mut results = vec![];
        for child in self.children() {
            match &*child {
                Component::Incoming(_loc, name, _typ) => results.push((name.to_string().into(), child.clone())),
                Component::Outgoing(_loc, name, _typ) => results.push((name.to_string().into(), child.clone())),
                _ => (),
            }
        }
        results
    }

    pub fn is_mod(&self) -> bool {
        match self {
            Component::Mod(_loc, _name, _children, _wires, _whens) => true,
            _ => false
        }
    }

    pub fn is_port(&self) -> bool {
        match self {
            Component::Incoming(_loc, _name, _typ) => true,
            Component::Outgoing(_loc, _name, _typ) => true,
            _ => false
        }
    }
}

#[derive(Debug, Clone)]
pub enum CircuitError {
    ExtHasNonPort(Name),
    DuplicateComponent(Name),
    MultipleDrivers(Name),
    NoDrivers(Arc<Component>),
    WrongWireType(Loc, Name, WireType),
    IncomingPortDriven(Name),
    NoSuchComponent(Loc, String),
    TypeError(TypeError),
    Many(Vec<CircuitError>),
    ParseError(Loc, String),
    Unknown(String),
}

impl std::fmt::Display for CircuitError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            CircuitError::ExtHasNonPort(name) => write!(f, "Ext declares a component other than an incoming or outgoing: {name}"),
            CircuitError::DuplicateComponent(name) => write!(f, "Duplicate component: {name}"),
            CircuitError::MultipleDrivers(name) => write!(f, "Component has multiple drivers: {name}."),
            CircuitError::NoDrivers(component) => write!(f, "Component is not driven: {}", component.name()),
            CircuitError::WrongWireType(_loc, name, wire_type) => {
                let symbol = match wire_type {
                    WireType::Direct => ":=",
                    WireType::Latch => "<=",
                    WireType::Proc => "<=!",
                };
                write!(f, "Wrong wire type: {name} does not support {symbol}")
            },
            CircuitError::IncomingPortDriven(name) => write!(f, "Incoming port is being driven from inside a mod, but shouldn't be: {name}"),
            CircuitError::NoSuchComponent(_loc, s) => write!(f, "No such component: {s}"),
            CircuitError::TypeError(type_error) => write!(f, "Type Error: {type_error}"),
            CircuitError::Many(errors) => write!(f, "{}", errors.iter().map(|s| s.to_string()).collect::<Vec<_>>().join("\n")),
            CircuitError::ParseError(_loc, _error) => write!(f, "{self:?}"),
            CircuitError::Unknown(_message) => write!(f, "{self:?}"),
        }
    }
}

impl HasLoc for CircuitError {
    fn loc(&self) -> Loc {
        match self {
            CircuitError::ExtHasNonPort(_name) => Loc::unknown(),
            CircuitError::DuplicateComponent(_name) => Loc::unknown(),
            CircuitError::MultipleDrivers(_name) => Loc::unknown(),
            CircuitError::NoDrivers(_name) => Loc::unknown(),
            CircuitError::WrongWireType(loc, _name, _wire_type) => loc.clone(),
            CircuitError::IncomingPortDriven(_name) => Loc::unknown(),
            CircuitError::NoSuchComponent(loc, _name) => loc.clone(),
            CircuitError::TypeError(type_error) => type_error.loc(),
            CircuitError::Many(_errors) => Loc::unknown(),
            CircuitError::ParseError(loc, _error) => loc.clone(),
            CircuitError::Unknown(_string) => Loc::unknown(),
        }
    }
}
