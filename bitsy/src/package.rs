use super::*;
mod resolve;

use once_cell::sync::OnceCell;

use std::collections::BTreeMap;
use std::sync::Arc;

pub use ast::WireType;

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
    items: Vec<Item>,
}

impl Package {
    pub fn from(ast: &ast::Package) -> Result<Package, Vec<BitsyError>> {
        let items = resolve::resolve(ast);
        let package = Package {
            items,
        };
        package.check()?;
        Ok(package)
    }

    pub fn top(&self, top_name: &str) -> Result<Circuit, BitsyError>  {
        if let Some(top) = self.moddef(top_name) {
            Ok(Circuit(self.clone(), top))
        } else {
            Err(BitsyError::Unknown(None, format!("No such mod definition: {top_name}")))
        }
    }

    pub fn moddefs(&self) -> Vec<Arc<Component>> {
        let mut results = vec![];
        for items in &self.items {
            if let Item::ModDef(moddef) = &items {
                results.push(moddef.clone());
            } else if let Item::ExtDef(moddef) = &items {
                results.push(moddef.clone());
            }
        }
        results
    }

    pub fn moddef(&self, name: &str) -> Option<Arc<Component>> {
        for item in &self.items {
            if let Item::ModDef(moddef) = &item {
                if moddef.name() == name {
                    return Some(moddef.clone());
                }
            } else if let Item::ExtDef(moddef) = &item {
                if moddef.name() == name {
                    return Some(moddef.clone());
                }
            }
        }
        None
    }

    pub fn extdef(&self, name: &str) -> Option<Arc<Component>> {
        for item in &self.items {
            if let Item::ExtDef(extdef) = &item {
                if extdef.name() == name {
                    return Some(extdef.clone());
                }
            }
        }
        None
    }

    pub fn typedef(&self, name: &str) -> Option<Arc<EnumTypeDef>> {
        for item in &self.items {
            if let Item::EnumTypeDef(typedef) = &item {
                if typedef.name == name {
                    return Some(typedef.clone());
                }
            }
        }
        None
    }

    /// Look at all components in scope, work out their type, and build a [`context::Context`] to assist in typechecking.
    pub fn context_for(&self, component: Arc<Component>) -> Context<Path, Type> {
        let mut ctx = vec![];
        for (path, target) in self.visible_paths(component.clone()) {
            let typ = self.type_of(target).unwrap();
            ctx.push((path, typ));
        }
        Context::from(ctx)
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
                Component::ModInst(_loc, name, moddef) => {
                    let mod_path: Path = name.to_string().into();
                    for (path, component) in moddef.port_paths() {
                        results.push((mod_path.join(path), component.clone()));
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

    pub fn type_of(&self, component: Arc<Component>) -> Option<Type> {
        match &*component {
            Component::Mod(_loc, _name, _children, _wires, _whens) => None,
            Component::ModInst(_loc, _name, _defname) => None,
            Component::Ext(_loc, _name, _children) => None,
            Component::Node(_loc, _name, typ) => Some(typ.clone()),
            Component::Outgoing(_loc, _name, typ) => Some(typ.clone()),
            Component::Incoming(_loc, _name, typ) => Some(typ.clone()),
            Component::Reg(_loc, _name, typ, _reset) => Some(typ.clone()),
        }
    }

    pub(crate) fn component_from(&self, component: Arc<Component>, path: Path) -> Option<Arc<Component>> {
        let mut result: Arc<Component> = component;
        for part in path.split(".") {
            if let Some(child) = result.child(part) {
                if let Component::ModInst(_loc, _name, moddef) = &*child {
                    result = moddef.clone();
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
pub enum Item {
    ModDef(Arc<Component>),
    ExtDef(Arc<Component>),
    EnumTypeDef(Arc<EnumTypeDef>),
    StructTypeDef(Arc<StructTypeDef>),
}

impl Item {
    pub fn name(&self) -> &str {
        match self {
            Item::ModDef(component) => component.name(),
            Item::ExtDef(component) => component.name(),
            Item::EnumTypeDef(typedef) => &typedef.name,
            Item::StructTypeDef(typedef) => &typedef.name,
        }
    }

    pub fn is_moddef(&self) -> bool {
        match self {
            Item::ModDef(_component) => true,
            Item::ExtDef(_component) => true,
            _ => false,
        }
    }

    pub fn is_typedef(&self) -> bool {
        match self {
            Item::EnumTypeDef(_typedef) => true,
            Item::StructTypeDef(_typedef) => true,
            _ => false,
        }
    }

    pub fn as_type(&self) -> Option<Type> {
        match self {
            Item::EnumTypeDef(typedef) => Some(Type::Enum(typedef.clone())),
            Item::StructTypeDef(typedef) => Some(Type::Struct(typedef.clone())),
            _ => None,
        }
    }

    pub fn as_component(&self) -> Option<Arc<Component>> {
        match self {
            Item::ModDef(component) => Some(component.clone()),
            Item::ExtDef(component) => Some(component.clone()),
            _ => None,
        }
    }
}

/// [`Wire`]s drive the value of port, node, or register.
#[derive(Debug, Clone)]
pub struct Wire(pub Loc, pub Path, pub Arc<Expr>, pub WireType);

#[derive(Debug, Clone)]
pub struct When(pub Arc<Expr>, pub Vec<Wire>);

impl Wire {
    pub fn new(loc: Loc, target: Path, expr: Arc<Expr>, typ: WireType) -> Wire {
        // TODO REMOVE THIS.
        Wire(loc, target, expr, typ)
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
