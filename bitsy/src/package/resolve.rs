use std::collections::BTreeSet;
use super::*;

pub fn resolve(package: &ast::Package) -> Vec<Item> {
    let mut items: BTreeMap<String, Item> = BTreeMap::new();
    for item in order_items(package) {
        let item = resolve_item(item, &items);
        items.insert(item.name().to_string(), item);
    }

    items.into_iter().map(|(_name, item)| item).collect()
}

fn order_items(package: &ast::Package) -> Vec<&ast::Item> {
    use petgraph::graph::{DiGraph, NodeIndex};
    use petgraph::algo::toposort;

    let mut items: BTreeMap<String, (NodeIndex, &ast::Item)> = BTreeMap::new();
    let mut name_by_node: BTreeMap<NodeIndex, String> = BTreeMap::new();
    let mut graph = DiGraph::new();

    for item in &package.items {
        let node = graph.add_node(item.name().to_string());
        items.insert(item.name().to_string(), (node, item));
        name_by_node.insert(node, item.name().to_string());
    }

    for item in &package.items {
        let (node, _item) = items[item.name()];
        items.insert(item.name().to_string(), (node, item));

        for item_dependency in item_dependencies(item) {
            let (dependency, _item) = items[&item_dependency];
            graph.add_edge(node, dependency, ());
        }
    }

    let mut sorted: Vec<NodeIndex> = toposort(&graph, None).unwrap();
    sorted.reverse();

    let mut results = vec![];
    for node in sorted {
        let name = &name_by_node[&node];
        let (_node, item) = items[name];
        results.push(item);
    }
    results
}

fn resolve_item(item: &ast::Item, items: &BTreeMap<String, Item>) -> Item {
    let user_types: Vec<(String, Type)> = items
        .clone()
        .into_iter()
        .filter(|(_name, item)| item.is_typedef())
        .map(|(name, item)| (name, item.as_type().unwrap()))
        .collect();
    let moddefs: Vec<(String, Arc<Component>)> = items
        .clone()
        .into_iter()
        .filter(|(_name, item)| item.is_moddef())
        .map(|(name, item)| (name, item.as_component().unwrap()))
        .collect();

    match item {
        ast::Item::ModDef(moddef) => {
            let ctx = Context::from(user_types.clone().into_iter().collect());
            let mod_ctx = Context::from(moddefs.clone().into_iter().collect());
            let moddef = resolve_moddef(moddef, ctx, mod_ctx);
            Item::ModDef(moddef.clone())
        },
        ast::Item::ExtDef(moddef) => {
            let ctx = Context::from(user_types.clone().into_iter().collect());
            let mod_ctx = Context::from(moddefs.clone().into_iter().collect());
            let moddef = resolve_extmoddef(moddef, ctx, mod_ctx);
            Item::ExtDef(moddef.clone())
        },
        ast::Item::EnumTypeDef(typedef) => {
            let typedef = resolve_enum_typedef(typedef);
            Item::EnumTypeDef(typedef)
        },
        ast::Item::StructTypeDef(typedef) => {
            let ctx = Context::from(user_types.clone().into_iter().collect());
            let typedef = resolve_struct_typedef(typedef, ctx);
            Item::StructTypeDef(typedef)
        },
    }
}

fn item_dependencies(item: &ast::Item) -> BTreeSet<String> {
    match item {
        ast::Item::ModDef(moddef) => moddef_dependencies(moddef),
        ast::Item::ExtDef(moddef) => moddef_dependencies(moddef),
        ast::Item::EnumTypeDef(_typedef) => BTreeSet::new(),
        ast::Item::StructTypeDef(typedef) => structtypedef_dependencies(typedef),
    }
}

fn moddef_dependencies(moddef: &ast::ModDef) -> BTreeSet<String> {
    let mut results = vec![];
    let ast::ModDef(_loc, _name, decls) = moddef;
    for decl in decls {
        results.extend(decl_dependencies(decl).into_iter());
    }
    results.into_iter().collect()
}

fn decl_dependencies(decl: &ast::Decl) -> BTreeSet<String> {
    let mut results = vec![];
    match decl {
        ast::Decl::Mod(_loc, _name, decls) => {
            for decl in decls {
                results.extend(decl_dependencies(decl).into_iter());
            }
        },
        ast::Decl::ModInst(_loc, _name, moddef_name) => results.push(moddef_name.clone()),
        ast::Decl::Incoming(_loc, _name, typ) => results.extend(type_dependencies(typ)),
        ast::Decl::Outgoing(_loc, _name, typ) => results.extend(type_dependencies(typ)),
        ast::Decl::Node(_loc, _name, typ) => results.extend(type_dependencies(typ)),
        ast::Decl::Reg(_loc, _name, typ, reset) => {
            results.extend(type_dependencies(typ).into_iter());
            if let Some(expr) = reset {
                results.extend(expr_dependencies(expr).into_iter());
            }
        },
        ast::Decl::Wire(_loc, wire) => results.extend(wire_dependencies(wire).into_iter()),
        ast::Decl::When(_loc, ast::When(cond, wires)) => {
            results.extend(expr_dependencies(cond).into_iter());
            for wire in wires {
                results.extend(wire_dependencies(wire).into_iter());
            }
        },
    }
    results.into_iter().collect()
}

fn wire_dependencies(wire: &ast::Wire) -> BTreeSet<String> {
    let ast::Wire(_loc2, _target, expr, _wire_type) = wire;
    expr_dependencies(expr)
}

fn structtypedef_dependencies(typedef: &ast::StructTypeDef) -> BTreeSet<String> {
    typedef.fields.iter().map(|(_name, typ)| type_dependencies(typ)).flatten().collect()
}

fn type_dependencies(typ: &ast::Type) -> BTreeSet<String> {
    match typ {
        ast::Type::Word(_n) => BTreeSet::new(),
        ast::Type::Vec(t, _n) => type_dependencies(t),
        ast::Type::Valid(t) => type_dependencies(t),
        ast::Type::TypeRef(r) => vec![r.clone()].into_iter().collect(),
    }
}

fn expr_dependencies(expr: &ast::Expr) -> BTreeSet<String> {
    match expr {
        ast::Expr::Ref(_loc, _target) => BTreeSet::new(),
        ast::Expr::Word(_loc, _w, _v) => BTreeSet::new(),
        ast::Expr::Enum(_loc, typ, _value) => type_dependencies(typ),
        ast::Expr::Struct(_loc, _fields) => BTreeSet::new(),
        ast::Expr::Vec(_loc, es) => {
            let mut results = BTreeSet::new();
            for e in es {
                results.extend(expr_dependencies(e).into_iter());
            }
            results
        },
        ast::Expr::Call(_loc, _func, es) => {
            let mut results = BTreeSet::new();
            for e in es {
                results.extend(expr_dependencies(e).into_iter());
            }
            results
        },
        ast::Expr::Let(_loc, _x, e, b) => {
            let mut results = BTreeSet::new();
            for e in &[e, b] {
                results.extend(expr_dependencies(e).into_iter());
            }
            results
        },
        ast::Expr::UnOp(_loc, _op, e1) => expr_dependencies(e1),
        ast::Expr::BinOp(_loc, _op, e1, e2) => {
            let mut results = BTreeSet::new();
            for e in &[e1, e2] {
                results.extend(expr_dependencies(e).into_iter());
            }
            results
        },
        ast::Expr::If(_loc, c, e1, e2) => {
            let mut results = BTreeSet::new();
            for e in &[c, e1, e2] {
                results.extend(expr_dependencies(e).into_iter());
            }
            results
        },
        ast::Expr::Match(_loc, e, arms) => {
            let mut results = BTreeSet::new();
            results.extend(expr_dependencies(e).into_iter());
            for ast::MatchArm(_pat, e) in arms {
                results.extend(expr_dependencies(e).into_iter());
            }
            results
        },
        ast::Expr::IdxField(_loc, e, _field) => expr_dependencies(e),
        ast::Expr::Idx(_loc, e, _i) => expr_dependencies(e),
        ast::Expr::IdxRange(_loc, e, _j, _i) => expr_dependencies(e),
        ast::Expr::Hole(_loc, _name) => BTreeSet::new(),
    }
}

fn resolve_type(typ: &ast::Type, ctx: Context<String, Type>) -> Type {
    match typ {
        ast::Type::Word(n) => Type::word(*n),
        ast::Type::Vec(t, n) => Type::vec(resolve_type(t, ctx), *n),
        ast::Type::Valid(t) => Type::valid(resolve_type(t, ctx)),
        ast::Type::TypeRef(r) => ctx.lookup(r).expect(&format!("Couldn't find {r}")).clone(),
    }
}

fn resolve_struct_typedef(typedef: &ast::StructTypeDef, ctx: Context<String, Type>) -> Arc<StructTypeDef> {
    let mut fields: BTreeMap<String, Type> = BTreeMap::new();
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

fn resolve_decls(decls: &[&ast::Decl], ctx: Context<String, Type>, mod_ctx: Context<String, Arc<Component>>) -> (Vec<Arc<Component>>, Vec<Wire>, Vec<When>) {
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

fn resolve_moddef(moddef: &ast::ModDef, ctx: Context<String, Type>, mod_ctx: Context<String, Arc<Component>>) -> Arc<Component> {
    let ast::ModDef(loc, name, decls) = moddef;
    let decls_slice: &[&ast::Decl] = &decls.iter().collect::<Vec<_>>();
    let (children, wires, whens) = resolve_decls(decls_slice, ctx, mod_ctx);
    Arc::new(Component::Mod(loc.clone(), name.clone(), children, wires, whens))
}

fn resolve_extmoddef(moddef: &ast::ModDef, ctx: Context<String, Type>, mod_ctx: Context<String, Arc<Component>>) -> Arc<Component> {
    let ast::ModDef(loc, name, decls) = moddef;
    let decls_slice: &[&ast::Decl] = &decls.iter().collect::<Vec<_>>();
    let (children, wires, whens) = resolve_decls(decls_slice, ctx, mod_ctx);
    assert!(wires.is_empty());
    assert!(whens.is_empty());
    Arc::new(Component::Ext(loc.clone(), name.clone(), children))
}

fn resolve_expr(expr: &ast::Expr, ctx: Context<String, Type>) -> Arc<Expr> {
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
                "word" => Expr::ToWord(loc.clone(), OnceCell::new(), package_es[0].clone()),
                "@Valid" => Expr::Ctor(loc.clone(), OnceCell::new(), "Valid".to_string(), package_es),
                "@Invalid" => Expr::Ctor(loc.clone(), OnceCell::new(), "Invalid".to_string(), vec![]),
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
