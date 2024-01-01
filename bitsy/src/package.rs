use super::*;

use std::sync::Arc;

pub use ast::Ident;
pub use ast::WireType;

pub type Name = String;

/// A Package is a parsed Bitsy file.
/// It consists of a number of top-level [`Item`]s.
#[derive(Debug, Clone)]
pub struct Package {
    items: Vec<Item>,
    idents: Vec<Ident>,
}

impl Package {
    pub fn from(ast: &ast::Package) -> Result<Package, Vec<BitsyError>> {
        let namespace = resolve::resolve(ast)?;
        let items = namespace.items().into_iter().map(|(_name, item)| item).collect();
        let idents = namespace.idents();

        let package = Package {
            items,
            idents,
        };

        package.check()?;
        Ok(package)
    }

    pub fn idents(&self) -> Vec<Ident> {
        self.idents.clone()
    }

    pub fn top(&self, top_name: &str) -> Result<Circuit, BitsyError>  {
        if let Some(top) = self.moddef(top_name) {
            Ok(Circuit(self.clone(), top))
        } else {
            Err(BitsyError::Unknown(None, format!("No such mod definition: {top_name}")))
        }
    }

    pub fn items(&self) -> Vec<Item> {
        let mut results = vec![];
        for item in &self.items {
            results.push(item.clone())
        }
        results
    }

    pub fn item(&self, name: &str) -> Option<Item> {
        for item in &self.items {
            if name == item.name() {
                return Some(item.clone());
            }
        }
        None
    }

    pub fn moddefs(&self) -> Vec<Arc<Component>> {
        let mut results = vec![];
        for item in &self.items {
            if let Item::ModDef(moddef) = &item {
                results.push(moddef.clone());
            } else if let Item::ExtDef(moddef) = &item {
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

    pub fn fndef(&self, name: &str) -> Option<Arc<FnDef>> {
        for item in &self.items {
            if let Item::FnDef(fndef) = &item {
                if fndef.name == name {
                    return Some(fndef.clone());
                }
            }
        }
        None
    }

    pub fn fndefs(&self) -> Vec<Arc<FnDef>> {
        let mut result = vec![];
        for item in &self.items {
            if let Item::FnDef(fndef) = &item {
                result.push(fndef.clone());
            }
        }
        result
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
                Component::Node(_span, name, _typ) => results.push((name.to_string().into(), child.clone())),
                Component::Incoming(_span, name, _typ) => results.push((name.to_string().into(), child.clone())),
                Component::Outgoing(_span, name, _typ) => results.push((name.to_string().into(), child.clone())),
                Component::Reg(_span, name, _typ, _reset) => results.push((name.to_string().into(), child.clone())),
                Component::Mod(_span, name, _children, _wires, _whens) => {
                    let mod_path: Path = name.to_string().into();
                    for (path, component) in child.port_paths() {
                        results.push((mod_path.join(path), component.clone()));
                    }
                },
                Component::ModInst(_span, name, moddef) => {
                    let mod_path: Path = name.to_string().into();
                    for (path, component) in moddef.port_paths() {
                        results.push((mod_path.join(path), component.clone()));
                    }
                },
                Component::Ext(_span, name, _children) => {
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
            Component::Mod(_span, _name, _children, _wires, _whens) => None,
            Component::ModInst(_span, _name, _defname) => None,
            Component::Ext(_span, _name, _children) => None,
            Component::Node(_span, _name, typ) => Some(typ.clone()),
            Component::Outgoing(_span, _name, typ) => Some(typ.clone()),
            Component::Incoming(_span, _name, typ) => Some(typ.clone()),
            Component::Reg(_span, _name, typ, _reset) => Some(typ.clone()),
        }
    }

    pub(crate) fn component_from(&self, component: Arc<Component>, path: Path) -> Option<Arc<Component>> {
        let mut result: Arc<Component> = component;
        for part in path.split(".") {
            if let Component::ModInst(_span, _name, moddef) = &*result {
                result = moddef.clone();
            }

            if let Some(child) = result.child(part) {
                result = child.clone();
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
    AltTypeDef(Arc<AltTypeDef>),
    FnDef(Arc<FnDef>),
}

impl Item {
    pub fn name(&self) -> &str {
        match self {
            Item::ModDef(component) => component.name(),
            Item::ExtDef(component) => component.name(),
            Item::EnumTypeDef(typedef) => &typedef.name,
            Item::StructTypeDef(typedef) => &typedef.name,
            Item::AltTypeDef(typedef) => &typedef.name,
            Item::FnDef(typedef) => &typedef.name,
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
            Item::AltTypeDef(_typedef) => true,
            _ => false,
        }
    }

    pub fn is_fndef(&self) -> bool {
        match self {
            Item::FnDef(_fndef) => true,
            _ => false,
        }
    }

    pub fn as_component(&self) -> Option<Arc<Component>> {
        match self {
            Item::ModDef(component) => Some(component.clone()),
            Item::ExtDef(component) => Some(component.clone()),
            _ => None,
        }
    }

    pub fn as_fndef(&self) -> Option<Arc<FnDef>> {
        match self {
            Item::FnDef(fndef) => Some(fndef.clone()),
            _ => None,
        }
    }
}

impl HasSpan for Item {
    fn span(&self) -> Span {
        match self {
            Item::ModDef(component) => component.span(),
            Item::ExtDef(component) => component.span(),
            Item::EnumTypeDef(typedef) => typedef.span.clone(),
            Item::StructTypeDef(typedef) => typedef.span.clone(),
            Item::AltTypeDef(typedef) => typedef.span.clone(),
            Item::FnDef(typedef) => typedef.span.clone(),
        }
    }
}

/// [`Wire`]s drive the value of port, node, or register.
#[derive(Debug, Clone)]
pub struct Wire(pub Span, pub Path, pub Arc<Expr>, pub WireType);

#[derive(Debug, Clone)]
pub struct When(pub Arc<Expr>, pub Vec<Wire>);

impl Wire {
    pub fn new(span: Span, target: Path, expr: Arc<Expr>, typ: WireType) -> Wire {
        // TODO REMOVE THIS.
        Wire(span, target, expr, typ)
    }
}

impl HasSpan for Wire {
    fn span(&self) -> Span {
        let Wire(span, _target, _expr, _wire_type) = self;
        span.clone()
    }
}

impl HasSpan for Component {
    fn span(&self) -> Span {
        match self {
            Component::Mod(span, _name, _children, _wires, _whens) => span.clone(),
            Component::ModInst(span, _name, _moddef) => span.clone(),
            Component::Ext(span, _name, _children) => span.clone(),
            Component::Incoming(span, _name, _typ) => span.clone(),
            Component::Outgoing(span, _name, _typ) => span.clone(),
            Component::Node(span, _name, _typ) => span.clone(),
            Component::Reg(span, _name, _typ, _expr) => span.clone(),
        }
    }
}
