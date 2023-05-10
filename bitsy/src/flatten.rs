use crate::{Module, GateComponent};
use crate::*;
use crate::common::*;

impl Module {
    pub fn flatten(&mut self) {
        let mut gensym_id = 0;
        let mut expr_wire_indexes: Vec<usize> = vec![];
        let mut wires_to_add: Vec<Wire> = vec![];
        let mut components_to_add: Vec<Component> = vec![];

        for (i, wire) in self.wires.iter().enumerate() {
            let Wire(visibility, sink, expr) = wire;
            expr_wire_indexes.push(i);
            let (new_components, new_wires, source) = self.flatten_expr(expr, &mut gensym_id);
            components_to_add.extend_from_slice(&new_components);
            wires_to_add.extend_from_slice(&new_wires);
            wires_to_add.push(Wire(*visibility, sink.clone(), Box::new(Expr::Var(source.name()))));
        }

        for i in expr_wire_indexes.into_iter().rev() {
            self.wires.remove(i);
        }

        self.components.extend_from_slice(&components_to_add);
        self.wires.extend_from_slice(&wires_to_add);
    }

    fn flatten_expr(
        &self,
        expr: &Expr,
        gensym_id: &mut usize,
    ) -> (
        Vec<Component>,
        Vec<Wire>,
        Terminal,
    ) {
        match &*expr {
            Expr::Var(name) => {
                let parts = name.split(".").collect::<Vec<_>>();
                assert_eq!(parts.len(), 2);
                let component_name = parts[0];
                let terminal_name = parts[1];
                let component: Component = self.component(name).expect("No such component").clone();
                (
                    vec![],
                    vec![],
                    component.port(terminal_name),
                )
            },
            Expr::Lit(v) => {
                let name = format!("__gen_{gensym_id}");
                *gensym_id += 1;
                let gate = Component::Const(name, Visibility::Private, v.clone(), todo!());
                (
                    vec![gate.clone()],
                    vec![],
                    gate.port("val"),
                )
            },
            Expr::Add(op0, op1) => {
                let (new_components0, new_wires0, op_terminal0) = self.flatten_expr(&op0, gensym_id);
                let (new_components1, new_wires1, op_terminal1) = self.flatten_expr(&op1, gensym_id);

                let name = format!("__gen_{gensym_id}");
                *gensym_id += 1;
                let gate = Component::Gate(name, Visibility::Private, GateComponent { gate: Arc::new(Gate("Add".to_string())) });
                (
                    vec![gate.clone()],
                    vec![
                        Wire(Visibility::Private, gate.port("in0"), op_terminal0.to_expr()),
                        Wire(Visibility::Private, gate.port("in1"), op_terminal1.to_expr()),
                    ],
                    gate.port("out"),
                )
            },
            Expr::Mul(op0, op1) => {
                todo!()
                /*
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
                */
            },
            _ => todo!(),
        }
    }
}

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
