use std::collections::BTreeSet;
use std::collections::BTreeMap;
use super::*;

struct Namespace(BTreeMap<String, Item>);

pub fn resolve(package: &ast::Package) -> Result<Vec<Item>, Vec<BitsyError>> {
    let mut namespace = Namespace::new();

    for item in order_items(package)? {
        let item = namespace.resolve_item(item)?;
        namespace.add(item.name(), item.clone());
    }

    Ok(namespace.items().into_iter().map(|(_name, item)| item).collect())
}

impl Namespace {
    fn new() -> Namespace {
        Namespace(BTreeMap::new())
    }

    fn items(&self) -> Vec<(String, Item)> {
        self.0.clone().into_iter().collect()
    }

    fn add(&mut self, name: &str, item: Item) {
        let prev = self.0.insert(name.to_string(), item);
        assert!(prev.is_none());
    }

    fn item(&self, name: &str) -> Option<Item> {
        self.0.get(name).cloned()
    }

    fn moddef(&self, name: &str) -> Option<Arc<Component>> {
        let item = self.item(name);
        if let Some(Item::ModDef(component)) = &item {
            Some(component.clone())
        } else if let Some(Item::ExtDef(component)) = &item {
            Some(component.clone())
        } else {
            None
        }
    }

    fn fndef(&self, name: &str) -> Option<Arc<FnDef>> {
        let item = self.item(name);
        if let Some(Item::FnDef(fndef)) = &item {
            Some(fndef.clone())
        } else {
            None
        }
    }

    fn resolve_item(&self, item: &ast::Item) -> Result<Item, Vec<BitsyError>> {
        Ok(match item {
            ast::Item::ModDef(moddef) => Item::ModDef(self.resolve_moddef(moddef)?),
            ast::Item::ExtDef(moddef) => Item::ExtDef(self.resolve_extmoddef(moddef)?),
            ast::Item::EnumTypeDef(typedef) => Item::EnumTypeDef(self.resolve_enum_typedef(typedef)?),
            ast::Item::StructTypeDef(typedef) => Item::StructTypeDef(self.resolve_struct_typedef(typedef)?),
            ast::Item::AltTypeDef(typedef) => Item::AltTypeDef(self.resolve_alt_typedef(typedef)?),
            ast::Item::FnDef(fndef) => Item::FnDef(self.resolve_fndef(fndef)?),
        })
    }

    fn resolve_moddef(&self, moddef: &ast::ModDef) -> Result<Arc<Component>, Vec<BitsyError>> {
        let ast::ModDef(loc, name, decls) = moddef;
        let decls_slice: &[&ast::Decl] = &decls.iter().collect::<Vec<_>>();
        let (children, wires, whens) = self.resolve_decls(decls_slice)?;
        Ok(Arc::new(Component::Mod(loc.clone(), name.to_string(), children, wires, whens)))
    }

    fn resolve_extmoddef(&self, moddef: &ast::ModDef) -> Result<Arc<Component>, Vec<BitsyError>> {
        let ast::ModDef(loc, name, decls) = moddef;
        let decls_slice: &[&ast::Decl] = &decls.iter().collect::<Vec<_>>();
        let (children, wires, whens) = self.resolve_decls(decls_slice)?;
        assert!(wires.is_empty());
        assert!(whens.is_empty());
        Ok(Arc::new(Component::Ext(loc.clone(), name.to_string(), children)))
    }

    fn resolve_enum_typedef(&self, typedef: &ast::EnumTypeDef) -> Result<Arc<EnumTypeDef>, Vec<BitsyError>> {
        let package_typedef = Arc::new(EnumTypeDef {
            name: typedef.name.to_string(),
            values: typedef.values.iter().map(|(name, val)| (name.to_string(), val.clone())).collect(),
        });
        Ok(package_typedef)
    }

    fn resolve_struct_typedef(&self, typedef: &ast::StructTypeDef) -> Result<Arc<StructTypeDef>, Vec<BitsyError>> {
        let mut fields: BTreeMap<String, Type> = BTreeMap::new();
        for (name, typ) in &typedef.fields {
            fields.insert(name.to_string(), self.resolve_type(typ)?);
        }

        let package_typedef = Arc::new(StructTypeDef {
            name: typedef.name.to_string(),
            fields: fields.into_iter().collect(),
        });
        Ok(package_typedef)
    }

    fn resolve_alt_typedef(&self, typedef: &ast::AltTypeDef) -> Result<Arc<AltTypeDef>, Vec<BitsyError>> {
        let mut alts: BTreeMap<String, Vec<Type>> = BTreeMap::new();
        for (name, typs) in &typedef.alts {
            let mut alt_types = vec![];
            for typ in typs {
                alt_types.push(self.resolve_type(typ)?);
            }
            alts.insert(name.to_string(), alt_types);
        }

        let package_typedef = Arc::new(AltTypeDef {
            name: typedef.name.to_string(),
            alts: alts.into_iter().collect(),
        });
        Ok(package_typedef)
    }

    fn resolve_fndef(&self, fndef: &ast::FnDef) -> Result<Arc<FnDef>, Vec<BitsyError>> {
        let mut args: BTreeMap<String, Type> = BTreeMap::new();
        for (name, typ) in &fndef.args {
            args.insert(name.to_string(), self.resolve_type(typ)?);
        }

        let package_typedef = Arc::new(FnDef {
            name: fndef.name.to_string(),
            args: args.into_iter().collect(),
            ret: self.resolve_type(&fndef.ret)?,
            body: self.resolve_expr(&fndef.body, Context::empty())?,
        });
        Ok(package_typedef)
    }

    fn resolve_type(&self, typ: &ast::Type) -> Result<Type, Vec<BitsyError>> {
        Ok(match typ {
            ast::Type::Word(n) => Type::word(*n),
            ast::Type::Vec(t, n) => Type::vec(self.resolve_type(t)?, *n),
            ast::Type::Valid(t) => Type::valid(self.resolve_type(t)?),
            ast::Type::TypeRef(r) => match self.item(r.as_str()) {
                Some(Item::EnumTypeDef(typedef)) => Type::Enum(typedef.clone()),
                Some(Item::StructTypeDef(typedef)) => Type::Struct(typedef.clone()),
                Some(Item::AltTypeDef(typedef)) => Type::Alt(typedef.clone()),
                Some(_) => return Err(vec![BitsyError::Unknown(None, format!("Not a type definition: {r}"))]),
                None => return Err(vec![BitsyError::Unknown(None, format!("Type definition not found: {r}"))]),
            },
        })
    }

    fn resolve_decls(&self, decls: &[&ast::Decl]) -> Result<(Vec<Arc<Component>>, Vec<Wire>, Vec<When>), Vec<BitsyError>> {
        let mut children = vec![];
        let mut wires = vec![];
        let mut whens = vec![];

        for decl in decls {
            match decl {
                ast::Decl::Mod(loc, name, decls) => {
                    let (inner_children, wires, whens) = self.resolve_decls(&decls.iter().collect::<Vec<_>>())?;
                    let child = Component::Mod(loc.clone(), name.to_string(), inner_children, wires, whens);
                    children.push(Arc::new(child));
                },
                ast::Decl::ModInst(loc, name, moddef_name) => {
                    let moddef = self.moddef(moddef_name.as_str()).unwrap();
                    let child = Component::ModInst(loc.clone(), name.to_string(), moddef);
                    children.push(Arc::new(child));
                },
                ast::Decl::Incoming(loc, name, typ) => {
                    let child = Component::Incoming(loc.clone(), name.to_string(), self.resolve_type(typ)?);
                    children.push(Arc::new(child));
                },
                ast::Decl::Outgoing(loc, name, typ) => {
                    let child = Component::Outgoing(loc.clone(), name.to_string(), self.resolve_type(typ)?);
                    children.push(Arc::new(child));
                },
                ast::Decl::Node(loc, name, typ) => {
                    let child = Component::Node(loc.clone(), name.to_string(), self.resolve_type(typ)?);
                    children.push(Arc::new(child));
                },
                ast::Decl::Reg(loc, name, typ, reset) => {
                    let reset_e = if let Some(e) = reset {
                        Some(self.resolve_expr(&e, Context::empty())?)
                    } else {
                        None
                    };
                    let child = Component::Reg(
                        loc.clone(),
                        name.to_string(),
                        self.resolve_type(typ)?,
                        reset_e,
                    );
                    children.push(Arc::new(child));
                },
                ast::Decl::Wire(loc, ast::Wire(_loc, target, expr, wire_type)) => {
                    let wire = Wire(
                        loc.clone(),
                        target_to_path(target),
                        self.resolve_expr(expr, Context::empty())?,
                        wire_type.clone(),
                    );
                    wires.push(wire);
                },
                ast::Decl::When(_loc, ast::When(cond, wires)) => {
                    let mut package_wires = vec![];
                    let package_cond = self.resolve_expr(&*cond, Context::empty())?;

                    for ast::Wire(loc, target, expr, wire_type) in wires {
                        let package_wire = Wire(
                            loc.clone(),
                            target_to_path(target),
                            self.resolve_expr(expr, Context::empty())?,
                            wire_type.clone(),
                        );
                        package_wires.push(package_wire);
                    }
                    let package_when = When(package_cond, package_wires);
                    whens.push(package_when);
                },
            }
        }

        Ok((children, wires, whens))
    }

    fn resolve_expr(&self, expr: &ast::Expr, ctx: Context<String, Type>) -> Result<Arc<Expr>, Vec<BitsyError>> {
        Ok(Arc::new(match expr {
            ast::Expr::Ident(loc, id) => Expr::Reference(loc.clone(), OnceCell::new(), id.to_string().into()),
            ast::Expr::Dot(loc, e, x) => {
                if let ast::Expr::Ident(_loc, id) = &**e {
                    Expr::Reference(loc.clone(), OnceCell::new(), format!("{id}.{x}").into())
                } else {
                    panic!()
                }
            },
            ast::Expr::Word(loc, w, n) => Expr::Word(loc.clone(), OnceCell::new(), *w, *n),
            ast::Expr::Enum(loc, typ, value) => {
                Expr::Enum(loc.clone(), OnceCell::new(), self.resolve_type(typ)?, value.clone())
            },
            ast::Expr::Struct(loc, fields) => {
                let mut package_fields: Vec<(String, Arc<Expr>)> = vec![];
                for (name, expr) in fields {
                    package_fields.push((name.to_string(), self.resolve_expr(expr, ctx.clone())?));
                }
                Expr::Struct(loc.clone(), OnceCell::new(), package_fields)
            },
            ast::Expr::Vec(loc, es) => {
                let mut package_es: Vec<Arc<Expr>> = vec![];
                for expr in es {
                    package_es.push(self.resolve_expr(expr, ctx.clone())?);
                }
                Expr::Vec(loc.clone(), OnceCell::new(), package_es)
            },
            ast::Expr::Call(loc, func, es) => {
                let mut package_es: Vec<Arc<Expr>> = vec![];
                for expr in es {
                    package_es.push(self.resolve_expr(expr, ctx.clone())?);
                }
                match func.as_str() {
                    "cat" => Expr::Cat(loc.clone(), OnceCell::new(), package_es),
                    "mux" => Expr::Mux(
                        loc.clone(),
                        OnceCell::new(),
                        package_es[0].clone(),
                        package_es[1].clone(),
                        package_es[2].clone(),
                    ),
                    "sext" => Expr::Sext(loc.clone(), OnceCell::new(), package_es[0].clone()),
                    "zext" => Expr::Zext(loc.clone(), OnceCell::new(), package_es[0].clone()),
                    "trycast" => Expr::TryCast(loc.clone(), OnceCell::new(), package_es[0].clone()),
                    "word" => Expr::ToWord(loc.clone(), OnceCell::new(), package_es[0].clone()),
                    "@Valid" => Expr::Ctor(loc.clone(), OnceCell::new(), "Valid".to_string(), package_es),
                    "@Invalid" => Expr::Ctor(loc.clone(), OnceCell::new(), "Invalid".to_string(), vec![]),
                    fnname => {
                        let func = fnname.to_string();
                        if fnname.starts_with("@") {
                            let mut package_es: Vec<Arc<Expr>> = vec![];
                            for expr in es {
                                package_es.push(self.resolve_expr(expr, ctx.clone())?);
                            }
                            Expr::Ctor(loc.clone(), OnceCell::new(), fnname[1..].to_string(), package_es)
                        } else if let Some(fndef) = self.fndef(&func) {
                            let mut package_es: Vec<Arc<Expr>> = vec![];
                            for expr in es {
                                package_es.push(self.resolve_expr(expr, ctx.clone())?);
                            }
                            Expr::Call(loc.clone(), OnceCell::new(), fndef, package_es)
                        } else {
                            panic!("Unknown call: {func}")
                        }
                    },
                }
            },
            ast::Expr::Let(loc, x, type_ascription, e, b) => {
                let package_e = self.resolve_expr(e, ctx.clone())?;
                let package_b = self.resolve_expr(b, ctx)?;
                let package_ascription = if let Some(typ) = type_ascription {
                    Some(self.resolve_type(&typ)?)
                } else {
                    None
                };
                Expr::Let(loc.clone(), OnceCell::new(), x.to_string(), package_ascription, package_e, package_b)
            },
            ast::Expr::UnOp(loc, op, e1) => {
                Expr::UnOp(loc.clone(), OnceCell::new(), *op, self.resolve_expr(&e1, ctx)?)
            },
            ast::Expr::BinOp(loc, op, e1, e2) => Expr::BinOp(
                loc.clone(),
                OnceCell::new(),
                *op,
                self.resolve_expr(&e1, ctx.clone())?,
                self.resolve_expr(&e2, ctx)?,
            ),
            ast::Expr::If(loc, c, e1, e2) => {
                let package_c = self.resolve_expr(c, ctx.clone())?;
                let package_e1 = self.resolve_expr(e1, ctx.clone())?;
                let package_e2 = self.resolve_expr(e2, ctx)?;
                Expr::If(loc.clone(), OnceCell::new(), package_c, package_e1, package_e2)
            },
            ast::Expr::Match(loc, e, arms) => {
                let package_e = self.resolve_expr(e, ctx.clone())?;
                let mut package_arms: Vec<MatchArm> = vec![];
                for ast::MatchArm(pat, expr) in arms {
                    let package_expr = self.resolve_expr(expr, ctx.clone())?;
                    package_arms.push(MatchArm(pat.clone(), package_expr))
                }
                Expr::Match(loc.clone(), OnceCell::new(), package_e, package_arms)
            },
            ast::Expr::IdxField(loc, e, field) => Expr::IdxField(
                loc.clone(),
                OnceCell::new(),
                self.resolve_expr(&e, ctx)?,
                field.to_string(),
            ),
            ast::Expr::Idx(loc, e, i) => {
                Expr::Idx(loc.clone(), OnceCell::new(), self.resolve_expr(&e, ctx)?, *i)
            },
            ast::Expr::IdxRange(loc, e, j, i) => {
                Expr::IdxRange(loc.clone(), OnceCell::new(), self.resolve_expr(&e, ctx)?, *j, *i)
            },
            ast::Expr::Hole(loc, name) => {
                Expr::Hole(loc.clone(), OnceCell::new(), name.clone().map(|name| name.to_string()))
            },
        }))
    }

}

fn item_dependencies(item: &ast::Item) -> BTreeSet<String> {
    match item {
        ast::Item::ModDef(moddef) => moddef_dependencies(moddef),
        ast::Item::ExtDef(moddef) => moddef_dependencies(moddef),
        ast::Item::EnumTypeDef(_typedef) => BTreeSet::new(),
        ast::Item::StructTypeDef(typedef) => structtypedef_dependencies(typedef),
        ast::Item::AltTypeDef(typedef) => altypedef_dependencies(typedef),
        ast::Item::FnDef(typedef) => fndef_dependencies(typedef),
    }
}

fn moddef_dependencies(moddef: &ast::ModDef) -> BTreeSet<String> {
    let mut results = vec![];
    let component_names = moddef_component_names(moddef);
    let ast::ModDef(_loc, _name, decls) = moddef;
    for decl in decls {
        results.extend(decl_dependencies(decl, &component_names).into_iter());
    }
    results.into_iter().collect()
}

fn moddef_component_names(moddef: &ast::ModDef) -> BTreeSet<String> {
    let mut result = BTreeSet::new();
    let ast::ModDef(_loc, _name, decls) = moddef;
    for decl in decls {
        match decl {
            ast::Decl::Mod(_loc, name, _decls) => {
                result.insert(name.to_string());
            },
            ast::Decl::ModInst(_loc, name, _moddef_name) => {
                result.insert(name.to_string());
            },
            ast::Decl::Incoming(_loc, name, _typ) => {
                result.insert(name.to_string());
            },
            ast::Decl::Outgoing(_loc, name, _typ) => {
                result.insert(name.to_string());
            },
            ast::Decl::Node(_loc, name, _typ) => {
                result.insert(name.to_string());
            },
            ast::Decl::Reg(_loc, name, _typ, _reset) => {
                result.insert(name.to_string());
            },
            ast::Decl::Wire(_loc, _wire) => (),
            ast::Decl::When(_loc, _when) => (),
        }
    }
    result
}

fn moddef_component_names_anonymous(decls: &[ast::Decl]) -> BTreeSet<String> {
    let mut result = BTreeSet::new();
    for decl in decls {
        match decl {
            ast::Decl::Mod(_loc, name, _decls) => {
                result.insert(name.to_string());
            },
            ast::Decl::ModInst(_loc, name, _moddef_name) => {
                result.insert(name.to_string());
            },
            ast::Decl::Incoming(_loc, name, _typ) => {
                result.insert(name.to_string());
            },
            ast::Decl::Outgoing(_loc, name, _typ) => {
                result.insert(name.to_string());
            },
            ast::Decl::Node(_loc, name, _typ) => {
                result.insert(name.to_string());
            },
            ast::Decl::Reg(_loc, name, _typ, _reset) => {
                result.insert(name.to_string());
            },
            ast::Decl::Wire(_loc, _wire) => (),
            ast::Decl::When(_loc, _when) => (),
        }
    }
    result
}

fn decl_dependencies(decl: &ast::Decl, component_names: &BTreeSet<String>) -> BTreeSet<String> {
    let mut results = vec![];
    match decl {
        ast::Decl::Mod(_loc, _name, decls) => {
            let component_names = moddef_component_names_anonymous(&*decls);
            for decl in decls {
                results.extend(decl_dependencies(decl, &component_names).into_iter());
            }
        },
        ast::Decl::ModInst(_loc, _name, moddef_name) => results.push(moddef_name.to_string()),
        ast::Decl::Incoming(_loc, _name, typ) => results.extend(type_dependencies(typ)),
        ast::Decl::Outgoing(_loc, _name, typ) => results.extend(type_dependencies(typ)),
        ast::Decl::Node(_loc, _name, typ) => results.extend(type_dependencies(typ)),
        ast::Decl::Reg(_loc, _name, typ, reset) => {
            results.extend(type_dependencies(typ).into_iter());
            if let Some(expr) = reset {
                results.extend(expr_dependencies(expr, component_names).into_iter());
            }
        },
        ast::Decl::Wire(_loc, wire) => {
            let ast::Wire(_loc2, _target, expr, _wire_type) = wire;
            results.extend(expr_dependencies(expr, component_names).into_iter())
        },
        ast::Decl::When(_loc, ast::When(cond, wires)) => {
            results.extend(expr_dependencies(cond, component_names).into_iter());
            for ast::Wire(_loc2, _target, expr, _wire_type) in wires {
                results.extend(expr_dependencies(expr, component_names).into_iter())
            }
        },
    }
    results.into_iter().collect()
}

fn structtypedef_dependencies(typedef: &ast::StructTypeDef) -> BTreeSet<String> {
    typedef.fields.iter().map(|(_name, typ)| type_dependencies(typ)).flatten().collect()
}

fn altypedef_dependencies(typedef: &ast::AltTypeDef) -> BTreeSet<String> {
    let mut deps = vec![];
    for (_name, typs) in &typedef.alts {
        for typ in typs {
            deps.extend(type_dependencies(typ).into_iter())
        }
    }
    deps.into_iter().collect()
}

fn fndef_dependencies(typedef: &ast::FnDef) -> BTreeSet<String> {
    let mut result = type_dependencies(&typedef.ret);
    let mut arguments = BTreeSet::new();
    for (name, typ) in &typedef.args {
        arguments.insert(name.to_string());
        result.extend(type_dependencies(typ).into_iter());
    }

    result.extend(expr_dependencies(&typedef.body, &arguments).into_iter());
    result
}

fn type_dependencies(typ: &ast::Type) -> BTreeSet<String> {
    match typ {
        ast::Type::Word(_n) => BTreeSet::new(),
        ast::Type::Vec(t, _n) => type_dependencies(t),
        ast::Type::Valid(t) => type_dependencies(t),
        ast::Type::TypeRef(r) => vec![r.to_string()].into_iter().collect(),
    }
}

fn expr_dependencies(expr: &ast::Expr, shadowed: &BTreeSet<String>) -> BTreeSet<String> {
    match expr {
        ast::Expr::Ident(_loc, ident) => {
            if shadowed.contains(&ident.to_string()) {
                BTreeSet::new()
            } else {
                vec![ident.to_string()].into_iter().collect()
            }
        },
        ast::Expr::Dot(_loc, e, _x) => expr_dependencies(e, shadowed),
        ast::Expr::Word(_loc, _w, _v) => BTreeSet::new(),
        ast::Expr::Enum(_loc, typ, _value) => type_dependencies(typ),
        ast::Expr::Struct(_loc, _fields) => BTreeSet::new(),
        ast::Expr::Vec(_loc, es) => {
            let mut results = BTreeSet::new();
            for e in es {
                results.extend(expr_dependencies(e, shadowed).into_iter());
            }
            results
        },
        ast::Expr::Call(_loc, func, es) => {
            let mut results = BTreeSet::new();
            #[rustfmt::skip]
            const SPECIALS: &[&str] = &[
                "cat",
                "mux",
                "sext",
                "zext",
                "trycast",
                "word",
                "@Valid",
                "@Invalid",
            ];

            if !SPECIALS.contains(&func.as_str()) && !func.as_str().starts_with("@") {
                results.insert(func.to_string());
            }
            for e in es {
                results.extend(expr_dependencies(e, shadowed).into_iter());
            }
            results
        },
        ast::Expr::Let(_loc, x, type_ascription, e, b) => {
            let mut new_shadowed = shadowed.clone();
            new_shadowed.insert(x.to_string());

            let mut results = BTreeSet::new();
            if let Some(typ) = type_ascription {
                results.extend(type_dependencies(typ).into_iter());
            }
            results.extend(expr_dependencies(e, shadowed).into_iter());
            results.extend(expr_dependencies(b, &new_shadowed).into_iter());
            results
        },
        ast::Expr::UnOp(_loc, _op, e1) => expr_dependencies(e1, shadowed),
        ast::Expr::BinOp(_loc, _op, e1, e2) => {
            let mut results = BTreeSet::new();
            for e in &[e1, e2] {
                results.extend(expr_dependencies(e, shadowed).into_iter());
            }
            results
        },
        ast::Expr::If(_loc, c, e1, e2) => {
            let mut results = BTreeSet::new();
            for e in &[c, e1, e2] {
                results.extend(expr_dependencies(e, shadowed).into_iter());
            }
            results
        },
        ast::Expr::Match(_loc, e, arms) => {
            let mut results = BTreeSet::new();
            results.extend(expr_dependencies(e, shadowed).into_iter());
            for ast::MatchArm(pat, e) in arms {
                let mut new_shadowed = shadowed.clone();
                new_shadowed.extend(pat.bound_vars().into_iter());
                results.extend(expr_dependencies(e, &new_shadowed).into_iter());
            }
            results
        },
        ast::Expr::IdxField(_loc, e, _field) => expr_dependencies(e, shadowed),
        ast::Expr::Idx(_loc, e, _i) => expr_dependencies(e, shadowed),
        ast::Expr::IdxRange(_loc, e, _j, _i) => expr_dependencies(e, shadowed),
        ast::Expr::Hole(_loc, _name) => BTreeSet::new(),
    }
}

fn target_to_path(target: &ast::Target) -> Path {
    match target {
        ast::Target::Local(id) => id.as_str().into(),
        ast::Target::Nonlocal(id1, id2) => format!("{}.{}", id1.as_str(), id2.as_str()).into(),
    }
}

fn order_items(package: &ast::Package) -> Result<Vec<&ast::Item>, Vec<BitsyError>> {
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

    let mut errors = vec![];

    for item in &package.items {
        let (node, _item) = items[item.name()];
        items.insert(item.name().to_string(), (node, item));

        for item_dependency in item_dependencies(item) {
            if let Some((dependency, _item)) = items.get(&item_dependency) {
                graph.add_edge(node, *dependency, ());
            } else {
                errors.push(BitsyError::Unknown(None, format!("{item_dependency} not found")));
            }
        }
    }

    if !errors.is_empty() {
        return Err(errors);
    }

    let mut sorted: Vec<NodeIndex> = toposort(&graph, None).unwrap();
    sorted.reverse();

    let mut results = vec![];
    for node in sorted {
        let name = &name_by_node[&node];
        let (_node, item) = items[name];
        results.push(item);
    }
    Ok(results)
}
