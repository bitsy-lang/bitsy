use super::*;

use std::collections::BTreeMap;
use std::sync::Arc;
use std::sync::Mutex;

use anyhow::anyhow;

pub type Name = String;

/// A Package is a parsed Bitsy file.
/// It consists of a number of top-level declarations.
///
/// After it is constructed, you need to call [`Package::resolve_references`]
/// to ensure all variables have valid referents.
/// After that, you also need to call [`Package::check`] to do typechecking of expressions
/// and connection checking of components.
#[derive(Debug, Clone)]
pub struct Package {
    decls: Vec<Decl>,
    user_types: BTreeMap<String, Arc<Type>>,
}

impl Package {
    pub fn new(decls: Vec<Decl>) -> Result<Package, Vec<CircuitError>> {
        let mut user_types: BTreeMap<String, Arc<Type>> = BTreeMap::new();
        for decl in &decls {
            if let Decl::TypeDef(typedef) = decl {
                user_types.insert(typedef.name.clone(), Arc::new(Type::Enum(typedef.clone())));
            }
        }

        let package = Package {
            decls,
            user_types,
        };
        package.resolve_references()?;
        package.check()?;
        Ok(package)
    }

    pub fn top(&self, top_name: &str) -> Result<Circuit, CircuitError>  {
        if let Some(top) = self.moddef(top_name) {
            Ok(Circuit(self.clone(), top))
        } else {
            Err(CircuitError::Unknown(None, format!("No such mod definition: {top_name}")))
        }
    }

    pub fn moddefs(&self) -> Vec<Arc<Component>> {
        let mut results = vec![];
        for decl in &self.decls {
            if let Decl::ModDef(moddef) = &decl {
                results.push(moddef.clone());
            } else if let Decl::ExtDef(moddef) = &decl {
                results.push(moddef.clone());
            }
        }
        results
    }

    pub fn moddef(&self, name: &str) -> Option<Arc<Component>> {
        for decl in &self.decls {
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
        for decl in &self.decls {
            if let Decl::ExtDef(extdef) = &decl {
                if extdef.name() == name {
                    return Some(extdef.clone());
                }
            }
        }
        None
    }

    pub fn typedef(&self, name: &str) -> Option<Arc<TypeDef>> {
        for decl in &self.decls {
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
    fn resolve_references(&self) -> Result<(), Vec<CircuitError>> {
        let mut errors = vec![];
        self.resolve_references_component_types();

        // resolve references in ModInsts and resets
        for moddef in self.moddefs() {
            for child in moddef.children() {
                if let Component::ModInst(loc, name, reference) = &*child {
                    if let Some(moddef) = self.moddef(reference.name()) {
                        reference.resolve_to(moddef).unwrap();
                    } else {
                        errors.push(CircuitError::Unknown(Some(loc.clone()), format!("Undefined reference to mod {name}")));
                    }
                } else if let Component::Reg(_loc, _name, _typ, Some(reset)) = &*child {
                    errors.extend(self.resolve_references_expr(reset.clone()));
                }
            }
        }

        // resolve references in Exprs in Wires
        for moddef in self.moddefs() {
            for Wire(_loc, _target, expr, _wiretype) in moddef.wires() {
                errors.extend(self.resolve_references_expr(expr));
            }
        }

        if errors.len() > 0 {
            Err(errors.to_vec())
        } else {
            Ok(())
        }
    }

    fn resolve_references_expr(&self, expr: Arc<Expr>) -> Vec<CircuitError> {
        let errors_mutex = Arc::new(Mutex::new(vec![]));
        let mut func = |e: &Expr| {
            if let Expr::Enum(loc, _typ, r, name) = e {
                if let Some(typ) = self.user_types.get(r.name()) {
                    r.resolve_to(typ.clone()).unwrap();
                } else {
                    let mut errors = errors_mutex.lock().unwrap();
                    errors.push(CircuitError::Unknown(Some(loc.clone()), format!("Undefined reference to mod {name}")));
                }
            }
        };
        expr.with_subexprs(&mut func);
        let errors = errors_mutex.lock().unwrap();
        errors.clone()
    }

    fn resolve_references_component_types(&self) {
        for moddef in self.moddefs() {
            for component in moddef.children() {
                match &*component {
                    Component::Mod(_loc, _name, _children, _wires, _whens) => (),
                    Component::ModInst(_loc, _name, _moddef) => (),
                    Component::Ext(_loc, _name, _children) => (),
                    Component::Node(_loc, _name, typ) => self.resolve_references_type(typ.clone()),
                    Component::Outgoing(_loc, _name, typ) => self.resolve_references_type(typ.clone()),
                    Component::Incoming(_loc, _name, typ) => self.resolve_references_type(typ.clone()),
                    Component::Reg(_loc, _name, typ, _reset) => self.resolve_references_type(typ.clone()),
                }
            }
        }
    }

    fn resolve_references_type(&self, typ: Arc<Type>) {
        match &*typ {
            Type::Word(_width) => (),
            Type::Enum(_typedef) => (),
            Type::Valid(typ) => self.resolve_references_type(typ.clone()),
            Type::Vec(typ, _len) => self.resolve_references_type(typ.clone()),
            Type::TypeDef(r) => {
                let typ = self.user_types.get(r.name()).unwrap().clone();
                r.resolve_to(typ).unwrap();
            },
        }
    }

    /// Look at all components in scope, work out their type, and build a [`context::Context`] to assist in typechecking.
    pub fn context_for(&self, component: Arc<Component>) -> anyhow::Result<Context<Path, Arc<Type>>> {
        // TODO Remove this anyhow
        let mut ctx = vec![];
        for (path, target) in self.visible_paths(component.clone()) {
//            let target = self.component_from(component.clone(), path.clone()).unwrap();
            let typ = self.type_of(target).ok_or_else(|| anyhow!("Unknown component: {path} is not visible in {}", component.name()))?;
            ctx.push((path, typ));
        }
        Ok(Context::from(ctx))
    }

    pub(crate) fn visible_paths(&self, component: Arc<Component>) -> Vec<(Path, Arc<Component>)> {
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
            }
        }
        results
    }

    pub fn type_of(&self, component: Arc<Component>) -> Option<Arc<Type>> {
        let typ = match &*component {
            Component::Mod(_loc, _name, _children, _wires, _whens) => None,
            Component::ModInst(_loc, _name, _defname) => None,
            Component::Ext(_loc, _name, _children) => None,
            Component::Node(_loc, _name, typ) => Some(typ.clone()),
            Component::Outgoing(_loc, _name, typ) => Some(typ.clone()),
            Component::Incoming(_loc, _name, typ) => Some(typ.clone()),
            Component::Reg(_loc, _name, typ, _reset) => Some(typ.clone()),
        };
        if let Some(mut typ) = typ {
            while let Type::TypeDef(r) = &*typ {
                typ = r.get().unwrap();
            }
            Some(typ)
        } else {
            None
        }
    }

    pub(crate) fn component_from(&self, component: Arc<Component>, path: Path) -> Option<Arc<Component>> {
        let mut result: Arc<Component> = component;
        for part in path.split(".") {
            if let Some(child) = result.child(part) {
                if let Component::ModInst(_loc, _name, moddef) = &*child {
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

/// The different kinds of [`Wire`]s in Bitsy.
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
pub struct Wire(pub Loc, pub Path, pub Arc<Expr>, pub WireType);

#[derive(Debug, Clone)]
pub struct When(pub Arc<Expr>, pub Vec<Wire>);

impl Wire {
    pub fn new(loc: Loc, target: Path, expr: Arc<Expr>, typ: WireType) -> Wire {
        Wire(loc, target, expr, typ)
    }

    pub fn rebase(self, base: Path) -> Wire {
        // TODO REMOVE THIS.
        let Wire(loc, target, expr, typ) = self;
        Wire(loc, base.join(target), expr.rebase(base), typ)
    }
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
            Component::Incoming(loc, _name, _typ) => loc.clone(),
            Component::Outgoing(loc, _name, _typ) => loc.clone(),
            Component::Node(loc, _name, _typ) => loc.clone(),
            Component::Reg(loc, _name, _typ, _expr) => loc.clone(),
        }
    }
}
