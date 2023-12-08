mod check;

pub use check::CircuitError;

use super::*;
use std::collections::BTreeSet;
use std::sync::Arc;
use std::sync::Mutex;

use crate::reference::Reference;

use anyhow::anyhow;

type Name = String;

/// A Package is a parsed Nettle file.
/// It consists of a number of top-level declarations.
///
/// After it is constructed, you need to call [`Package::resolve_references`]
/// to ensure all variables have valid referents.
/// After that, you also need to call [`Package::check`] to do typechecking of expressions
/// and connection checking of components.
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
            } else if let Decl::ExtDef(moddef) = &decl {
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
            } else if let Decl::ExtDef(moddef) = &decl {
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

    /// Iterate over all declarations and resolve references.
    ///
    /// Submodule instances (eg, `mod foo of Foo`) will resolve the module definition (eg, `Foo`).
    ///
    /// For each wire, the expression is resolved.
    /// This will find any references to typedefs and resolve those (eg, `AluOp::Add`).
    pub fn resolve_references(&self) -> Result<(), Vec<CircuitError>> {
        let mut errors = vec![];
        self.resolve_references_component_types();

        // resolve references in ModInsts and ExtInsts
        for moddef in self.moddefs() {
            for child in moddef.children() {
                if let Component::ExtInst(loc, name, reference) = &*child {
                    if let Some(extdef) = self.extdef(reference.name()) {
                        reference.resolve_to(extdef).unwrap();
                    } else {
                        errors.push(CircuitError::Unknown(Some(loc.clone()), format!("Undefined reference to ext: {name}")));
                    }
                } else if let Component::ModInst(loc, name, reference) = &*child {
                    if let Some(moddef) = self.moddef(reference.name()) {
                        reference.resolve_to(moddef).unwrap();
                    } else {
                        errors.push(CircuitError::Unknown(Some(loc.clone()), format!("Undefined reference to mod {name}")));
                    }
                }
            }
        }

        let errors_mutex = Arc::new(Mutex::new(errors));

        // resolve references in Exprs in Wires
        for moddef in self.moddefs() {
            for Wire(_loc, _target, expr, _wiretype) in moddef.wires() {
                let func = |e: &Expr| {
                    if let Expr::Lit(loc, Value::Enum(r, name)) = e {
                        if let Some(typedef) = self.typedef(r.name()) {
                            r.resolve_to(typedef).unwrap();
                        } else {
                            let mut errors = errors_mutex.lock().unwrap();
                            errors.push(CircuitError::Unknown(Some(loc.clone()), format!("Undefined reference to mod {name}")));
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

    /// Look at all components in scope, work out their type, and build a [`context::Context`] to assist in typechecking.
    pub fn context_for(&self, component: Arc<Component>) -> anyhow::Result<Context<Path, Type>> {
        // TODO Remove this anyhow
        let mut ctx = vec![];
        for (path, target) in self.visible_paths(component.clone()) {
//            let target = self.component_from(component.clone(), path.clone()).unwrap();
            let typ = self.type_of(target).ok_or_else(|| anyhow!("Unknown component: {path} is not visible in {}", component.name()))?;
            ctx.push((path, typ));
        }
        Ok(Context::from(ctx))
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
                        for (path, component) in moddef.port_paths() {
                            results.push((mod_path.join(path), component.clone()));
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

}

/// A top-level declaration in a [`Package`].
#[derive(Debug, Clone)]
pub enum Decl {
    ModDef(Arc<Component>),
    ExtDef(Arc<Component>),
    TypeDef(Arc<TypeDef>),
}

/// The different kinds of [`Wire`]s in Nettle.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WireType {
    /// Direct wire. Written `:=` in the syntax. Connects one terminal to another.
    Direct,
    /// Latched wire. Written `<=` in the syntax. Connects one terminal to the data pin of a register.
    Latch,
    /// Procedural. Written `<=!` in the syntax. Connects one terminal to the data pin of a register.
    Proc,
}

/// [`Wire`]s drive the value of port, node, or register.
#[derive(Debug, Clone)]
pub struct Wire(pub Loc, pub Path, pub Expr, pub WireType);

#[derive(Debug, Clone)]
pub struct When(pub Expr, pub Vec<Wire>);

impl Wire {
    pub fn new(loc: Loc, target: Path, expr: Expr, typ: WireType) -> Wire {
        Wire(loc, target, expr, typ)
    }

    pub fn rebase(self, base: Path) -> Wire {
        // TODO REMOVE THIS.
        let Wire(loc, target, expr, typ) = self;
        Wire(loc, base.join(target), expr.rebase(base), typ)
    }
}

/// A [`Component`] is a declaration that lives inside of a `mod` or `ext` definiton.
#[derive(Debug, Clone)]
pub enum Component {
    Mod(Loc, Name, Vec<Arc<Component>>, Vec<Wire>, Vec<When>),
    ModInst(Loc, Name, Reference<Component>),
    Ext(Loc, Name, Vec<Arc<Component>>),
    ExtInst(Loc, Name, Reference<Component>), // TODO GET RID OF THIS
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

/// A [`Circuit`] is a module instance.
/// It allows you to walk the instance, following references to definitions.
#[derive(Debug, Clone)]
pub struct Circuit(Arc<Package>, Arc<Component>);

impl Circuit {
    pub fn new(package: Package, top: &str) -> Circuit {
        let top = package.moddef(top).expect(&format!("No such mod definition: {top}"));
        let circuit = Circuit(Arc::new(package), top);
        circuit
    }

    pub fn package(&self) -> &Package {
        &self.0
    }

    /// The module definition for this [`Circuit`].
    pub fn top(&self) -> Arc<Component> {
        self.1.clone()
    }

    pub fn root(&self) -> Path {
        // TODO REMOVE THIS
        self.top().name().into()
    }

    /// Dot into the given path.
    /// Follow [`Component::ModInst`]s to their definitions.
    pub fn component(&self, path: Path) -> Option<Arc<Component>> {
        let root: Path = self.top().name().into();
        if path == root {
            return Some(self.top());
        }
        // TODO GET RID OF THIS
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

    /// Walk the instance's module hierarchy, returning all [`Wire`]s.
    pub fn wires(&self) -> Vec<Wire> {
        let mut results = vec![];
        for (path, component) in self.walk_instances() {
            for Wire(_loc, target, expr, wiretype) in component.wires() {
                // TODO _loc should be loc?
                results.push(Wire(_loc, path.clone().join(target), expr.rebase(path.clone()), wiretype));
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

    /// Walk the instance's module hierarchy, returning all [`Wire`]s.
    pub fn regs(&self) -> Vec<Path> {
        let mut results = vec![];
        for (path, component) in self.walk_instances() {
            if let Component::Reg(_loc, _name, _typ, _reset) = &*component {
                results.push(path);
            }
        }
        results
    }

    /// Walk the instance's module hierarchy, returning all [`Path`]s for everything.
    pub fn terminals(&self) -> Vec<Path> {
        self.top().terminals_rec(self.top().name().into())
    }

    /// Given a [`Path`], if it is a [`Component::Reg`], return its reset value.
    pub fn reset_for_reg(&self, path: Path) -> Option<Expr> {
        if let Component::Reg(_loc, _name, _typ, reset) = &*self.component(path)? {
            Some(reset.clone())
        } else {
            None
        }
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
            Component::Mod(_loc, _name, _children, wires, _whens) => {
                wires.clone()
            }
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
