use super::*;

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

fn resolve_type(typ: &ast::Type, ctx: Context<String, Arc<Type>>) -> Arc<Type> {
    match typ {
        ast::Type::Word(n) => Type::word(*n),
        ast::Type::Vec(t, n) => Type::vec(resolve_type(t, ctx), *n),
        ast::Type::Valid(t) => Type::valid(resolve_type(t, ctx)),
        ast::Type::TypeRef(r) => ctx.lookup(r).unwrap().clone(),
    }
}

fn resolve_struct_typedef(typedef: &ast::StructTypeDef, ctx: Context<String, Arc<Type>>) -> Arc<StructTypeDef> {
    let mut fields: BTreeMap<String, Arc<Type>> = BTreeMap::new();
    for (name, typ) in &typedef.fields {
        fields.insert(name.to_string(), resolve_type(typ, ctx.clone()));
    }

    let package_typedef = Arc::new(StructTypeDef {
        name: typedef.name.to_string(),
        fields: fields.into_iter().collect(),
    });
    package_typedef
}

fn resolve_enum_typedef(typedef: &ast::EnumTypeDef) -> Arc<EnumTypeDef> {
    let package_typedef = Arc::new(EnumTypeDef {
        name: typedef.name.to_string(),
        values: typedef.values.clone(),
    });
    package_typedef
}

fn resolve_decls(decls: &[&ast::Decl], ctx: Context<String, Arc<Type>>, mod_ctx: Context<String, Arc<Component>>) -> (Vec<Arc<Component>>, Vec<Wire>, Vec<When>) {
    let mut children = vec![];
    let mut wires = vec![];
    let mut whens = vec![];

    for decl in decls {
        match decl {
            ast::Decl::Mod(loc, name, decls) => {
                let (inner_children, wires, whens) = resolve_decls(&decls.iter().collect::<Vec<_>>(), ctx.clone(), mod_ctx.clone());
                let child = Component::Mod(loc.clone(), name.clone(), inner_children, wires, whens);
                children.push(Arc::new(child));
            },
            ast::Decl::ModInst(loc, name, moddef_name) => {
                let child = Component::ModInst(loc.clone(), name.clone(), mod_ctx.lookup(moddef_name).unwrap());
                children.push(Arc::new(child));
            },
            ast::Decl::Incoming(loc, name, typ) => {
                let child = Component::Incoming(loc.clone(), name.clone(), resolve_type(typ, ctx.clone()));
                children.push(Arc::new(child));
            },
            ast::Decl::Outgoing(loc, name, typ) => {
                let child = Component::Outgoing(loc.clone(), name.clone(), resolve_type(typ, ctx.clone()));
                children.push(Arc::new(child));
            },
            ast::Decl::Node(loc, name, typ) => {
                let child = Component::Node(loc.clone(), name.clone(), resolve_type(typ, ctx.clone()));
                children.push(Arc::new(child));
            },
            ast::Decl::Reg(loc, name, typ, reset) => {
                let child = Component::Reg(
                    loc.clone(),
                    name.clone(),
                    resolve_type(typ, ctx.clone()),
                    reset.clone().map(|e| resolve_expr(&e, ctx.clone())),
                );
                children.push(Arc::new(child));
            },
            ast::Decl::Wire(loc, ast::Wire(_loc, target, expr, wire_type)) => {
                let wire = Wire(
                    loc.clone(),
                    target_to_path(target),
                    resolve_expr(expr, ctx.clone()),
                    wire_type.clone(),
                );
                wires.push(wire);
            },
            ast::Decl::When(_loc, ast::When(cond, wires)) => {
                let mut package_wires = vec![];
                let package_cond = resolve_expr(&*cond, ctx.clone());

                for ast::Wire(loc, target, expr, wire_type) in wires {
                    let package_wire = Wire(
                        loc.clone(),
                        target_to_path(target),
                        resolve_expr(expr, ctx.clone()),
                        wire_type.clone(),
                    );
                    package_wires.push(package_wire);
                }
                let package_when = When(package_cond, package_wires);
                whens.push(package_when);
            },
        }
    }

    (children, wires, whens)
}

fn resolve_moddef(moddef: &ast::ModDef, ctx: Context<String, Arc<Type>>, mod_ctx: Context<String, Arc<Component>>) -> Arc<Component> {
    let ast::ModDef(loc, name, decls) = moddef;
    let decls_slice: &[&ast::Decl] = &decls.iter().collect::<Vec<_>>();
    let (children, wires, whens) = resolve_decls(decls_slice, ctx, mod_ctx);
    Arc::new(Component::Mod(loc.clone(), name.clone(), children, wires, whens))
}

fn resolve_extmoddef(moddef: &ast::ModDef, ctx: Context<String, Arc<Type>>, mod_ctx: Context<String, Arc<Component>>) -> Arc<Component> {
    let ast::ModDef(loc, name, decls) = moddef;
    let decls_slice: &[&ast::Decl] = &decls.iter().collect::<Vec<_>>();
    let (children, wires, whens) = resolve_decls(decls_slice, ctx, mod_ctx);
    assert!(wires.is_empty());
    assert!(whens.is_empty());
    Arc::new(Component::Ext(loc.clone(), name.clone(), children))
}

fn resolve_expr(expr: &ast::Expr, ctx: Context<String, Arc<Type>>) -> Arc<Expr> {
    Arc::new(match expr {
        ast::Expr::Ref(loc, target) => Expr::Reference(loc.clone(), OnceCell::new(), target_to_path(target)),
        ast::Expr::Word(loc, w, n) => Expr::Word(loc.clone(), OnceCell::new(), *w, *n),
        ast::Expr::Enum(loc, typ, value) => Expr::Enum(loc.clone(), OnceCell::new(), resolve_type(typ, ctx.clone()), value.clone()),
        ast::Expr::Struct(loc, fields) => {
            let package_fields = fields
                .into_iter()
                .map(|(name, expr)| {
                    (name.to_string(), resolve_expr(expr, ctx.clone()))
                })
                .collect();
            Expr::Struct(loc.clone(), OnceCell::new(), package_fields)
        },
        ast::Expr::Vec(loc, es) => {
            let package_es = es
                .into_iter()
                .map(|expr| {
                    resolve_expr(expr, ctx.clone())
                })
                .collect();
            Expr::Vec(loc.clone(), OnceCell::new(), package_es)
        },
        ast::Expr::Call(loc, func, es) => {
            let package_es = es
                .into_iter()
                .map(|expr| {
                    resolve_expr(expr, ctx.clone())
                })
                .collect();
            match func.as_str() {
                "cat" => Expr::Cat(loc.clone(), OnceCell::new(), package_es),
                "mux" => Expr::Mux(loc.clone(), OnceCell::new(), package_es[0].clone(), package_es[1].clone(), package_es[2].clone()),
                "sext" => Expr::Sext(loc.clone(), OnceCell::new(), package_es[0].clone()),
                "word" => Expr::Sext(loc.clone(), OnceCell::new(), package_es[0].clone()),
                _ => panic!("Unknown call: {func}"), // TODO Expr::Call(loc.clone(), OnceCell::new(), func.clone(), package_es),
            }
        },
        ast::Expr::Let(loc, x, e, b) => {
            let package_e = resolve_expr(e, ctx.clone());
            let package_b = resolve_expr(b, ctx.clone());
            Expr::Let(loc.clone(), OnceCell::new(), x.clone(), package_e, package_b)
        },
        ast::Expr::UnOp(loc, op, e1) => Expr::UnOp(loc.clone(), OnceCell::new(), *op, resolve_expr(&e1, ctx.clone())),
        ast::Expr::BinOp(loc, op, e1, e2) => Expr::BinOp(loc.clone(), OnceCell::new(), *op, resolve_expr(&e1, ctx.clone()), resolve_expr(&e2, ctx.clone())),
        ast::Expr::If(loc, c, e1, e2) => {
            let package_c  = resolve_expr(c, ctx.clone());
            let package_e1 = resolve_expr(e1, ctx.clone());
            let package_e2 = resolve_expr(e2, ctx.clone());
            Expr::If(loc.clone(), OnceCell::new(), package_c, package_e1, package_e2)
        },
        ast::Expr::Match(loc, e, arms) => {
            let package_e  = resolve_expr(e, ctx.clone());
            let package_arms = arms
                .into_iter()
                .map(|ast::MatchArm(pat, expr)| {
                    let package_expr = resolve_expr(expr, ctx.clone());
                    MatchArm(pat.clone(), package_expr)
                })
                .collect();
            Expr::Match(loc.clone(), OnceCell::new(), package_e, package_arms)
        },
        ast::Expr::IdxField(loc, e, field) => Expr::IdxField(loc.clone(), OnceCell::new(), resolve_expr(&e, ctx.clone()), field.clone()),
        ast::Expr::Idx(loc, e, i) => Expr::Idx(loc.clone(), OnceCell::new(), resolve_expr(&e, ctx.clone()), *i),
        ast::Expr::IdxRange(loc, e, j, i) => Expr::IdxRange(loc.clone(), OnceCell::new(), resolve_expr(&e, ctx.clone()), *j, *i),
        ast::Expr::Hole(loc, name) => Expr::Hole(loc.clone(), OnceCell::new(), name.clone()),
    })
}

fn target_to_path(target: &ast::Target) -> Path {
    match target {
        ast::Target::Local(path) => path.to_string().into(),
        ast::Target::Nonlocal(path, port) => format!("{path}.{port}").into(),
    }
}

impl Package {
    pub fn from(ast: &ast::Package) -> Result<Package, Vec<BitsyError>> {
        let mut user_types = BTreeMap::new();
        let mut moddefs: BTreeMap<String, Arc<Component>> = BTreeMap::new();
        let mut items = vec![];
        for item in &ast.items {
            match item {
                ast::Item::ModDef(moddef) => {
                    let ctx = Context::from(user_types.clone().into_iter().collect());
                    let mod_ctx = Context::from(moddefs.clone().into_iter().collect());
                    let moddef = resolve_moddef(moddef, ctx, mod_ctx);
                    items.push(Item::ModDef(moddef.clone()));
                    moddefs.insert(moddef.name().to_string(), moddef);
                },
                ast::Item::ExtDef(moddef) => {
                    let ctx = Context::from(user_types.clone().into_iter().collect());
                    let mod_ctx = Context::from(moddefs.clone().into_iter().collect());
                    let moddef = resolve_extmoddef(moddef, ctx, mod_ctx);
                    items.push(Item::ExtDef(moddef.clone()));
                    moddefs.insert(moddef.name().to_string(), moddef);
                },
                ast::Item::EnumTypeDef(typedef) => {
                    let typedef = resolve_enum_typedef(typedef);
                    user_types.insert(typedef.name.to_string(), Arc::new(Type::Enum(typedef)));
                },
                ast::Item::StructTypeDef(typedef) => {
                    let ctx = Context::from(user_types.clone().into_iter().collect());
                    let typedef = resolve_struct_typedef(typedef, ctx);
                    user_types.insert(typedef.name.to_string(), Arc::new(Type::Struct(typedef)));
                },
            }
        }

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

    /*
    /// Iterate over all declarations and resolve references.
    ///
    /// Submodule instances (eg, `mod foo of Foo`) will resolve the module definition (eg, `Foo`).
    ///
    /// For each wire, the expression is resolved.
    /// This will find any references to typedefs and resolve those (eg, `AluOp::Add`).
    fn resolve_references(&self) -> Result<(), Vec<BitsyError>> {
        let mut errors = vec![];
        self.resolve_references_component_types();

        // resolve references in ModInsts and resets
        for moddef in self.moddefs() {
            for child in moddef.children() {
                if let Component::ModInst(loc, name, reference) = &*child {
                    if let Some(moddef) = self.moddef(reference.name()) {
                        reference.resolve_to(moddef).unwrap();
                    } else {
                        errors.push(BitsyError::Unknown(Some(loc.clone()), format!("Undefined reference to mod {name}")));
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

    fn resolve_references_expr(&self, expr: Arc<Expr>) -> Vec<BitsyError> {
        let errors_mutex = Arc::new(Mutex::new(vec![]));
        let mut func = |e: &Expr| {
            if let Expr::Enum(loc, _typ, r, name) = e {
                if let Some(typ) = self.user_types.get(r.name()) {
                    r.resolve_to(typ.clone()).unwrap();
                } else {
                    let mut errors = errors_mutex.lock().unwrap();
                    errors.push(BitsyError::Unknown(Some(loc.clone()), format!("Undefined reference to mod {name}")));
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
            Type::Struct(typedef) => {
                for (_name, typ) in &typedef.fields {
                    self.resolve_references_type(typ.clone());
                }
            },
            Type::TypeRef(r) => {
                let typ = self.user_types.get(r.name()).unwrap().clone();
                r.resolve_to(typ).unwrap();
            },
        }
    }
    */

    /// Look at all components in scope, work out their type, and build a [`context::Context`] to assist in typechecking.
    pub fn context_for(&self, component: Arc<Component>) -> Context<Path, Arc<Type>> {
        // TODO Remove this anyhow
        let mut ctx = vec![];
        for (path, target) in self.visible_paths(component.clone()) {
//            let target = self.component_from(component.clone(), path.clone()).unwrap();
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

    pub fn type_of(&self, component: Arc<Component>) -> Option<Arc<Type>> {
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
