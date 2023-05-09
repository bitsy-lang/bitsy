use crate::Bitsy;
use crate::*;
use crate::common::*;

/*
impl Bitsy {
    pub fn flatten(&mut self) {
        let mut gensym_id = 0;
        let mut expr_wire_indexes: Vec<usize> = vec![];
        let mut wires_to_add: Vec<Wire> = vec![];
        let mut components_to_add: Vec<Component> = vec![];

        for (i, wire) in self.wires.iter().enumerate() {
            if let Wire::Expr(visibility, sink, expr) = wire {
                expr_wire_indexes.push(i);
                let (new_components, new_wires, source) = self.flatten_expr(
                    expr.clone(),
                    &mut gensym_id,
                );
                components_to_add.extend_from_slice(&new_components);
                wires_to_add.extend_from_slice(&new_wires);
                wires_to_add.push(Wire::Simple(*visibility, sink.clone(), source));
            }
        }

        for i in expr_wire_indexes.into_iter().rev() {
            self.wires.remove(i);
        }

        self.components.extend_from_slice(&components_to_add);
        self.wires.extend_from_slice(&wires_to_add);
    }

    fn flatten_expr(
        &self,
        expr: Arc<Expr>,
        gensym_id: &mut usize,
    ) -> (
        Vec<Component>,
        Vec<Wire>,
        Terminal,
    ) {
        match &*expr {
            Expr::Term(terminal) => {
                (
                    vec![],
                    vec![],
                    terminal.clone(),
                )
            },
            Expr::Lit(v) => {
                let name = format!("__gen_{gensym_id}");
                *gensym_id += 1;
                let gate = Component::Const(name, Visibility::Private, v.clone());
                (
                    vec![gate.clone()],
                    vec![],
                    gate.port("val"),
                )
            },
            Expr::Add(op0, op1) => {
                let (new_components0, new_wires0, op_terminal0) = self.flatten_expr(op0.clone(), gensym_id);
                let (new_components1, new_wires1, op_terminal1) = self.flatten_expr(op1.clone(), gensym_id);

                let name = format!("__gen_{gensym_id}");
                *gensym_id += 1;
                let gate = Component::Gate(name, Visibility::Private, GateComponent { gate_name: "Add".to_string() });
                (
                    vec![gate.clone()],
                    vec![
                        Wire::Simple(Visibility::Private, gate.port("in0"), op_terminal0),
                        Wire::Simple(Visibility::Private, gate.port("in1"), op_terminal1),
                    ],
                    gate.port("out"),
                )
            },
            Expr::Mul(op0, op1) => {
                let (new_components0, new_wires0, op_terminal0) = self.flatten_expr(op0.clone(), gensym_id);
                let (new_components1, new_wires1, op_terminal1) = self.flatten_expr(op1.clone(), gensym_id);

                let name = format!("__gen_{gensym_id}");
                *gensym_id += 1;
                let gate = Component::Gate(name, Visibility::Private, GateComponent { gate_name: "Mul".to_string() });
                (
                    vec![gate.clone()],
                    vec![
                        Wire::Simple(Visibility::Private, gate.port("in0"), op_terminal0),
                        Wire::Simple(Visibility::Private, gate.port("in1"), op_terminal1),
                    ],
                    gate.port("out"),
                )
            },
        }
    }
}
*/
/*
impl Bitsy {
    pub fn flatten_exprs(&self) -> Bitsy {
        let mut decls = vec![];

        for decl in &self.decls {
            match decl {
                Decl::ModDef(mod_def) => {
                    let mut flattened_mod_def: ModDef = ModDef::clone(mod_def);
                    flattened_mod_def.flatten_exprs();
                    decls.push(Decl::ModDef(Arc::new(flattened_mod_def)));
                },
                Decl::EnumDef(enum_def) => {
                    decls.push(Decl::EnumDef(Arc::clone(enum_def)));
                },
                Decl::StructDef(struct_def) => {
                    decls.push(Decl::StructDef(Arc::clone(struct_def)));
                },
            }
        }
        Bitsy {
            decls,
        }
    }
}
*/
