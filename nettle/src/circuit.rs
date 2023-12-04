use super::*;
use std::collections::BTreeSet;
use std::sync::Arc;

use crate::reference::Reference;

use anyhow::anyhow;

pub type Name = String;

#[derive(Debug, Clone)]
pub struct Package(pub Vec<Decl>);

impl Package {
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
pub struct Wire(pub Path, pub Expr, pub WireType);

#[derive(Debug, Clone)]
pub struct When(pub Expr, pub Vec<Wire>);


impl Wire {
    pub fn new(target: Path, expr: Expr, typ: WireType) -> Wire {
        Wire(target, expr, typ)
    }

    pub fn rebase(self, base: Path) -> Wire {
        let Wire(target, expr, typ) = self;
        Wire(base.join(target), expr.rebase(base), typ)
    }
}

#[derive(Debug, Clone)]
pub enum Component {
    Mod(Name, Vec<Arc<Component>>, Vec<Wire>, Vec<When>),
    ModInst(Name, Reference<Component>),
    Ext(Name, Vec<Arc<Component>>),
    ExtInst(Name, Reference<Component>),
    Incoming(Name, Type),
    Outgoing(Name, Type),
    Node(Name, Type),
    Reg(Name, Type, Expr),
}

#[derive(Debug, Clone)]
pub struct Circuit(Arc<Package>);

impl std::ops::Deref for Circuit {
    type Target = Component;
    fn deref(&self) -> &Component {
        let Package(decls) = &*self.0;
        for decl in decls {
            if let Decl::ModDef(m) = decl {
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

    fn package(&self) -> &Package {
        &self.0
    }

    pub fn top(&self) -> Arc<Component> {
        let Package(decls) = self.package();
        for decl in decls {
            if let Decl::ModDef(m) = decl {
                return m.clone();
            }
        }
        panic!("No top")
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
                if let Component::ExtInst(_name, extdef) = &*child {
                    result = extdef.get().unwrap().clone();
                } else if let Component::ModInst(_name, moddef) = &*child {
                    result = moddef.get().unwrap().clone();
                } else {
                    result = child.clone();
                }
            }
        }
        Some(result)
    }

    fn resolve_references(&mut self) {
        self.resolve_references_component_types();

        // resolve references in ModInsts and ExtInsts
        for (_path, component) in self.walk() {
            for child in component.children() {
                if let Component::ExtInst(name, reference) = &*child {
                    let package = self.package();
                    if let Some(extdef) = package.extdef(reference.name()) {
                        reference.resolve_to(extdef).unwrap();
                    } else {
                        panic!("Undefined reference to ext: {name}")
                    }
                } else if let Component::ModInst(name, reference) = &*child {
                    let package = self.package();
                    if let Some(moddef) = package.moddef(reference.name()) {
                        reference.resolve_to(moddef).unwrap();
                    } else {
                        panic!("Undefined reference to mod: {name}")
                    }
                }
            }
        }

        // resolve references in Exprs in Wires
        for moddef in self.package().moddefs() {
            for Wire(_target, expr, _wiretype) in moddef.wires() {
                let func = |e: &Expr| {
                    if let Expr::Lit(_loc, Value::Enum(r, _name)) = e {
                        let typedef = self.0.typedef(r.name()).unwrap();
                        r.resolve_to(typedef).unwrap();
                    }
                };
                expr.with_subexprs(&func);
            }
        }
    }

    fn resolve_references_component_types(&self) {
        for moddef in self.package().moddefs() {
            for component in moddef.children() {
                match &*component {
                    Component::Mod(_name, _children, _wires, _whens) => (),
                    Component::ModInst(_name, _moddef) => (),
                    Component::Ext(_name, _children) => (),
                    Component::ExtInst(_name, _defname) => (),
                    Component::Node(_name, typ) => self.resolve_references_type(typ),
                    Component::Outgoing(_name, typ) => self.resolve_references_type(typ),
                    Component::Incoming(_name, typ) => self.resolve_references_type(typ),
                    Component::Reg(_name, typ, _reset) => self.resolve_references_type(typ),
                }
            }
        }
    }

    fn resolve_references_type(&self, typ: &Type) {
        match typ {
            Type::Word(_width) => (),
            Type::Vec(typ, _len) => self.resolve_references_type(typ),
            Type::TypeDef(typedef) => {
                if let Some(resolved_typedef) = self.package().typedef(typedef.name()) {
                    typedef.resolve_to(resolved_typedef).unwrap();
                } else {
                    panic!("No definition for typedef {}", typedef.name())
                }
            },
        }
    }

    pub fn wires(&self) -> Vec<Wire> {
        let mut results = vec![];
        for (path, component) in self.walk_instances() {
            for Wire(target, expr, wiretype) in component.wires() {
                results.push(Wire(path.clone().join(target), expr.rebase(path.clone()), wiretype));
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
            if let Component::ModInst(name, reference) = &*child {
                let package = self.package();
                if let Some(moddef) = package.moddef(reference.name()) {
                    results.extend(self.walk_instances_rec(moddef.clone(), path.join(child.name().into())));
                } else {
                    panic!("Undefined reference to ext: {name}")
                }
            } else if let Component::ExtInst(name, reference) = &*child {
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

    fn walk(&self) -> Vec<(Path, Arc<Component>)> {
        self.walk_rec(self.top(), self.top().name().into())
    }

    fn walk_rec(&self, component: Arc<Component>, path: Path) -> Vec<(Path, Arc<Component>)> {
        let mut results = vec![(path.clone(), component.clone())];
        for child in component.children() {
            results.extend(self.walk_rec(child.clone(), path.join(child.name().into())));
        }
        results
    }

    pub fn exts(&self) -> Vec<Path> {
        let mut results = vec![];
        for (path, component) in self.walk_instances() {
            if let Component::Ext(_name, _children) = &*component {
                results.push(path);
            }
        }
        results
    }

    pub fn regs(&self) -> Vec<Path> {
        let mut results = vec![];
        for (path, component) in self.walk_instances() {
            if let Component::Reg(_name, _typ, _reset) = &*component {
                results.push(path);
            }
        }
        results
    }

    pub fn terminals(&self) -> Vec<Path> {
        self.top().terminals_rec(self.top().name().into())
    }

    pub fn reset_for_reg(&self, path: Path) -> Option<Expr> {
        if let Component::Reg(_name, _typ, reset) = &*self.component(path)? {
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
        for path in self.visible_paths(component.clone()) {
            let target = self.component_from(component.clone(), path.clone()).unwrap();
            let typ = self.type_of(target).ok_or_else(|| anyhow!("Unknown component: {path} is not visible in {}", self.name()))?;
            ctx.push((path, typ));
        }
        Ok(Context::from(ctx))
    }

    fn check_component(&self, component: Arc<Component>) -> Result<(), Vec<CircuitError>> {
        let mut errors = vec![];

        match &*component {
            Component::Mod(_name, _children, _wires, _whens) => {
                errors.extend(self.check_typecheck(component.clone()));
                errors.extend(self.check_wires_no_such_component(component.clone()));
                errors.extend(self.check_children_duplicate_names(component.clone()));
                errors.extend(self.check_wires_duplicate_targets(component.clone()));
                errors.extend(self.check_missing_drivers(component.clone()));
                errors.extend(self.check_wires_wiretype(component.clone()));
                errors.extend(self.check_incoming_port_driven(component.clone()));
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
        for Wire(target, _expr, _typ) in &component.wires() {
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

        for Wire(target, expr, _wiretype) in &component.wires() {
            let target_typ = if let Some(typ) = ctx.lookup(target) {
                typ
            } else {
                errors.push(CircuitError::Unknown(format!("Target {target} in {} has unknown type", component.name())));
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

        for Wire(target, _expr, _wiretype) in &component.wires() {
            if self.component_from(component.clone(), target.clone()).is_none() {
                errors.push(CircuitError::NoSuchComponent(target.to_string()));
            }
        }
        errors
    }

    fn check_wires_wiretype(&self, component: Arc<Component>) -> Vec<CircuitError> {
        let mut errors = vec![];

        for Wire(target, _expr, wiretype) in &component.wires() {
            if let Some(component) = self.component_from(component.clone(), target.clone()) {
                match (&*component, wiretype) {
                    (Component::Reg(name, _typ, _reset), WireType::Direct) =>
                        errors.push(CircuitError::WrongWireType(name.clone(), WireType::Direct)),
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

    fn check_incoming_port_driven(&self, component: Arc<Component>) -> Vec<CircuitError> {
        let mut errors = vec![];

        for Wire(target, _expr, _wiretype) in &component.wires() {
            if let Some(component) = self.component_from(component.clone(), target.clone()) {
                let is_local = !target.contains(".");
                if is_local {
                    match &*component {
                        Component::Incoming(name, _typ) =>
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
        let mut terminals_remaining: BTreeSet<Path> = self.visible_paths(component.clone()).into_iter().collect();

        for Wire(target, _expr, _typ) in &component.wires() {
            terminals_remaining.remove(target);
        }

        for terminal in terminals_remaining.into_iter() {
            let mut is_incoming_port = false;
            let mut is_outgoing_port = false;

            if let Some(component) = self.component_from(component.clone(), terminal.clone()) {
                if let Component::Incoming(_name, _typ) = &*component {
                    is_incoming_port = true;
                } else if let Component::Incoming(_name, _typ) = &*component {
                    is_outgoing_port = true;
                }
            }

            let is_local = !terminal.contains(".");

            if !is_local && is_incoming_port {
                errors.push(CircuitError::NoDrivers(terminal.to_string()));
            } else if is_local && is_outgoing_port {
                errors.push(CircuitError::NoDrivers(terminal.to_string()));
            }
        }
        errors
    }

    fn visible_paths(&self, component: Arc<Component>) -> Vec<Path> {
        let mut results = vec![];
        for child in component.children() {
            match &*child {
                Component::Node(name, _typ) => results.push(name.to_string().into()),
                Component::Incoming(name, _typ) => results.push(name.to_string().into()),
                Component::Outgoing(name, _typ) => results.push(name.to_string().into()),
                Component::Reg(name, _typ, _reset) => results.push(name.to_string().into()),
                Component::Mod(name, _children, _wires, _whens) => {
                    let mod_path: Path = name.to_string().into();
                    for path in child.port_paths() {
                        results.push(mod_path.join(path));
                    }
                },
                Component::ModInst(name, defname) => {
                    let mod_path: Path = name.to_string().into();
                    if let Some(moddef) = defname.get() {
                        for path in moddef.port_paths() {
                            results.push(mod_path.join(path));
                        }
                    } else {
                        panic!("External module {name} hasn't been resolved.")
                    }
                },
                Component::Ext(name, _children) => {
                    let ext_path: Path = name.to_string().into();
                    for path in child.port_paths() {
                        results.push(ext_path.join(path));
                    }
                },
                Component::ExtInst(name, defname) => {
                    let ext_path: Path = name.to_string().into();
                    if let Some(extdef) = defname.get() {
                        for path in extdef.port_paths() {
                            results.push(ext_path.join(path));
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
            Component::Mod(_name, _children, _wires, _whens) => None,
            Component::ModInst(_name, _defname) => None,
            Component::Ext(_name, _children) => None,
            Component::ExtInst(_name, _defname) => None,
            Component::Node(_name, typ) => Some(typ.clone()),
            Component::Outgoing(_name, typ) => Some(typ.clone()),
            Component::Incoming(_name, typ) => Some(typ.clone()),
            Component::Reg(_name, typ, _reset) => Some(typ.clone()),
        }
    }
}

impl Component {
    pub fn name(&self) -> &str {
        match self {
            Component::Mod(name, _children, _wires, _whens) => name.as_str(),
            Component::ModInst(name, _defname) => name.as_str(),
            Component::Ext(name, _children) => name.as_str(),
            Component::ExtInst(name, _defname) => name.as_str(),
            Component::Incoming(name, _typ) => name.as_str(),
            Component::Outgoing(name, _typ) => name.as_str(),
            Component::Node(name, _typ) => name.as_str(),
            Component::Reg(name, _typ, _value) => name.as_str(),
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
            Component::Mod(_name, children, _wires, _whens) => children.iter().cloned().collect(),
            Component::ModInst(_name, _defname) => vec![],
            Component::Ext(_name, children) => children.iter().cloned().collect(),
            Component::ExtInst(_name, _defname) => vec![],
            Component::Incoming(_name, _typ) => vec![],
            Component::Outgoing(_name, _typ) => vec![],
            Component::Node(_name, _typ) => vec![],
            Component::Reg(_name, _typ, _value) => vec![],
        }
    }

    fn wires(&self) -> Vec<Wire> {
        match self {
            Component::Mod(_name, _children, wires, _whens) => wires.clone(),
            _ => vec![],
        }
    }

    fn terminals_rec(&self, path: Path) -> Vec<Path> {
        let mut results = vec![];
        for child in self.children() {
            match &*child {
                Component::Node(name, _typ) => results.push(path.join(name.clone().into())),
                Component::Reg(name, _typ, _reset) => {
                    results.push(path.join(format!("{name}.set").into()));
                    results.push(path.join(name.clone().into()));
                },
                Component::Incoming(name, _typ) => results.push(path.join(name.clone().into())),
                Component::Outgoing(name, _typ) => results.push(path.join(name.clone().into())),
                Component::Mod(name, _children, _wires, _whens) => results.extend(child.terminals_rec(path.join(name.clone().into()))),
                Component::ModInst(name, defname) => {
                    if let Some(moddef) = defname.get() {
                        results.extend(moddef.terminals_rec(path.join(name.clone().into())))
                    } else {
                        panic!("Module {name} hasn't been resolved.")
                    }
                }
                Component::Ext(name, _children) => results.extend(child.terminals_rec(path.join(name.clone().into()))),
                Component::ExtInst(name, defname) => {
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

    fn port_paths(&self) -> Vec<Path> {
        let mut results = vec![];
        for child in self.children() {
            match &*child {
                Component::Incoming(name, _typ) => results.push(name.to_string().into()),
                Component::Outgoing(name, _typ) => results.push(name.to_string().into()),
                _ => (),
            }
        }
        results
    }

    pub fn is_mod(&self) -> bool {
        match self {
            Component::Mod(_name, _children, _wires, _whens) => true,
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

#[derive(Debug, Clone)]
pub enum CircuitError {
    ExtHasNonPort(Name),
    DuplicateComponent(Name),
    MultipleDrivers(Name),
    NoDrivers(Name),
    WrongWireType(Name, WireType),
    IncomingPortDriven(Name),
    NoSuchComponent(Name),
    TypeError(TypeError),
    Unknown(String),
}

impl HasLoc for CircuitError {
    fn loc(&self) -> Loc {
        match self {
            CircuitError::ExtHasNonPort(_name) => Loc::unknown(),
            CircuitError::DuplicateComponent(_name) => Loc::unknown(),
            CircuitError::MultipleDrivers(_name) => Loc::unknown(),
            CircuitError::NoDrivers(_name) => Loc::unknown(),
            CircuitError::WrongWireType(_name, WireType) => Loc::unknown(),
            CircuitError::IncomingPortDriven(_name) => Loc::unknown(),
            CircuitError::NoSuchComponent(_name) => Loc::unknown(),
            CircuitError::TypeError(type_error) => type_error.loc(),
            CircuitError::Unknown(_string) => Loc::unknown(),
        }
    }
}
