use super::*;
use std::sync::Arc;

/// A [`Component`] is a declaration that lives inside of a `mod` or `ext` definiton.
#[derive(Debug, Clone)]
pub enum Component {
    Mod(Loc, Name, Vec<Arc<Component>>, Vec<Wire>, Vec<When>),
    ModInst(Loc, Name, Arc<Component>),
    Ext(Loc, Name, Vec<Arc<Component>>),
    Incoming(Loc, Name, Type),
    Outgoing(Loc, Name, Type),
    Node(Loc, Name, Type),
    Reg(Loc, Name, Type, Option<Arc<Expr>>),
}

impl Component {
    pub fn name(&self) -> &str {
        match self {
            Component::Mod(_loc, name, _children, _wires, _whens) => name.as_str(),
            Component::ModInst(_loc, name, _defname) => name.as_str(),
            Component::Ext(_loc, name, _children) => name.as_str(),
            Component::Incoming(_loc, name, _typ) => name.as_str(),
            Component::Outgoing(_loc, name, _typ) => name.as_str(),
            Component::Node(_loc, name, _typ) => name.as_str(),
            Component::Reg(_loc, name, _typ, _value) => name.as_str(),
        }
    }

    pub fn child(&self, name: &str) -> Option<Arc<Component>> {
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
            Component::Incoming(_loc, _name, _typ) => vec![],
            Component::Outgoing(_loc, _name, _typ) => vec![],
            Component::Node(_loc, _name, _typ) => vec![],
            Component::Reg(_loc, _name, _typ, _value) => vec![],
        }
    }

    pub(crate) fn wires(&self) -> Vec<Wire> {
        match self {
            Component::Mod(_loc, _name, _children, wires, _whens) => {
                wires.clone()
            }
            _ => vec![],
        }
    }

    pub(crate) fn terminals_rec(&self, path: Path) -> Vec<Path> {
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
                Component::ModInst(_loc, name, moddef) => {
                    results.extend(moddef.terminals_rec(path.join(name.clone().into())))
                }
                Component::Ext(_loc, name, _children) => results.extend(child.terminals_rec(path.join(name.clone().into()))),
            }
        }
        results
    }

    pub fn port_paths(&self) -> Vec<(Path, Arc<Component>)> {
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

    pub fn is_incoming_port(&self) -> bool {
        match self {
            Component::Incoming(_loc, _name, _typ) => true,
            _ => false
        }
    }

    pub fn is_outgoing_port(&self) -> bool {
        match self {
            Component::Outgoing(_loc, _name, _typ) => true,
            _ => false
        }
    }

    pub fn type_of(&self) -> Option<Type> {
        match self {
            Component::Node(_loc,_name, typ) => Some(typ.clone()),
            Component::Reg(_loc,_name, typ, _reset) => Some(typ.clone()),
            Component::Incoming(_loc,_name, typ) => Some(typ.clone()),
            Component::Outgoing(_loc,_name, typ) => Some(typ.clone()),
            Component::Mod(_loc,_name, _children, _wires, _whens) => None,
            Component::ModInst(_loc,_name, _defname) => None,
            Component::Ext(_loc,_name, _children) => None,
        }
    }

    pub fn reset(&self) -> Option<Arc<Expr>> {
        match self {
            Component::Reg(_loc, _name, _typ, reset) => reset.clone(),
            _ => None
        }
    }

    pub fn submods(&self) -> Vec<Arc<Component>> {
        let mut results = vec![];
        for child in self.children() {
            if let Component::Mod(_loc,_name, _children, _wires, _whens) = &*child {
                results.push(child.clone());
            }
        }
        results
    }
}
