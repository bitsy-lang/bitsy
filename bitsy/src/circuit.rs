mod check;
mod mlir;

use super::*;
use std::collections::BTreeSet;
use std::sync::Arc;

/// A [`Circuit`] is a module instance.
/// It allows you to walk the instance, following references to definitions.
#[derive(Debug, Clone)]
pub struct Circuit(pub(crate) Package, pub(crate) Arc<Component>);

impl Circuit {
    pub fn package(&self) -> &Package {
        &self.0
    }

    /// The module definition for this [`Circuit`].
    pub fn top(&self) -> Arc<Component> {
        self.1.clone()
    }

    /// Dot into the given path.
    /// Follow [`Component::ModInst`]s to their definitions.
    pub fn component(&self, path: Path) -> Option<Arc<Component>> {
        if path == "top".into() {
            return Some(self.top());
        }
        // TODO GET RID OF THIS
        let root_prefix = format!("top.");
        if !path.starts_with(&root_prefix) {
            eprintln!("{path} does not start with top");
        }
        assert!(path.starts_with(&root_prefix));
        let path: Path = path[root_prefix.len()..].into();
        self.component_from(self.top(), path)
    }

    fn component_from(&self, component: Arc<Component>, path: Path) -> Option<Arc<Component>> {
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
        self.top().terminals_rec("top".into())
    }

    /// Given a [`Path`], if it is a [`Component::Reg`], return its reset value.
    pub fn reset_for_reg(&self, path: Path) -> Option<Arc<Expr>> {
        if let Component::Reg(_loc, _name, _typ, reset) = &*self.component(path)? {
            reset.clone()
        } else {
            None
        }
    }

    fn walk_instances(&self) -> Vec<(Path, Arc<Component>)> {
        self.walk_instances_rec(self.top(), "top".into())
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
            }  else {
                results.extend(self.walk_instances_rec(child.clone(), path.join(child.name().into())));
            }
        }
        results
    }

}
