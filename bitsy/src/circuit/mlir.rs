use super::*;
use std::sync::Arc;
use std::collections::BTreeMap;

impl Package {
    pub fn emit_mlir(&self) {
        for moddef in self.moddefs() {
            self.emit_mlir_moddef(moddef);
        }
    }

    fn emit_mlir_moddef(&self, moddef: Arc<Component>) {
        let mut ports: Vec<(bool, String, Arc<Type>)> = vec![];
        let mut output_ports: Vec<String> = vec![];
        let mut output_port_ssas: BTreeMap<String, String> = BTreeMap::new();
        let mut output_port_types: Vec<Arc<Type>> = vec![];

        for (_path, port) in moddef.port_paths() {
            let name = port.name().to_string();
            let typ = self.type_of(port.clone()).unwrap();
            ports.push((port.is_incoming_port(), name.clone(), typ.clone()));
            if port.is_outgoing_port() {
                output_ports.push(name);
                output_port_types.push(typ);
            }
        }

        let ctx = self.context_for(moddef.clone()).unwrap();

        println!("hw.module @{}(", moddef.name());
        self.emit_mlir_moddef_portlist(&ports);
        println!(") {{");

        for (i, Wire(_loc, target, expr, wire_type)) in moddef.wires().iter().enumerate() {
            match wire_type {
                WireType::Direct => {
                    let ssa = expr.emit_mlir(i, ctx.clone());
                    let target_string = target.to_string();
                    if output_ports.contains(&target_string) {
                        output_port_ssas.insert(target_string, ssa);
                    }
                },
                WireType::Latch => {
                    let next_ssa = expr.emit_mlir(i, ctx.clone());
                    let target_string = target.to_string();
                    let reg = moddef.child(target).unwrap();
                    let typ = reg.type_of().unwrap();
                    let reset = reg.reset().unwrap();
                    let reset_ssa = reset.emit_mlir(i, ctx.clone());
                    println!("    %{target_string} = seq.firreg {next_ssa} clock %_clock reset sync %_reset, {reset_ssa} : {}", type_to_mlir(typ));
                },
                _ => panic!(),
            }
        }

        let output_port_ssas: Vec<&str> = output_ports.iter().map(|output_port| {
            output_port_ssas[output_port].as_str()
        }).collect();
        let output_port_types: Vec<String> = output_port_types.iter().map(|typ| {
            type_to_mlir(typ.clone())
        }).collect();
        println!("    hw.output {} : {}", output_port_ssas.join(","), output_port_types.join(","));
        println!("}}");
    }

    fn emit_mlir_moddef_portlist(&self, ports: &[(bool, String, Arc<Type>)]) {
        println!("    in %_clock : !seq.clock,");
        println!("    in %_reset : i1,");
        for (i, (is_input, name, typ)) in ports.iter().enumerate() {
            let typ_name = type_to_mlir(typ.clone());
            if *is_input {
                print!("    in %{name} : {typ_name}");
            } else {
                print!("    out {name} : {typ_name}");
            }
            if i + 1 < ports.len() {
                println!(",");
            } else {
                println!();
            }
        }
    }
}

impl Expr {
    fn emit_mlir(&self, prefix: usize, ctx: Context<Path, Arc<Type>>) -> String {
        let (name, op) = match self {
            Expr::Reference(_loc, typ, name) => {
                let typ: Arc<Type> = typ.get().unwrap().clone();
                let typ_name = type_to_mlir(typ);
                (format!("%tmp_{prefix}_reference"), format!("comb.add %{name} : {typ_name}"))
            },
            Expr::Word(_loc, typ, _w, n) => {
                let typ: Arc<Type> = typ.get().unwrap().clone();
                let (v_str, v_typ) = (format!("{n}"), type_to_mlir(typ));
                (format!("%tmp_{prefix}_lit"), format!("hw.constant {v_str} : {v_typ}"))
            },
            Expr::BinOp(_loc, typ, BinOp::Add, e1, e2) => {
                let typ = typ.get().unwrap().clone();
                let typ_name = type_to_mlir(typ);
                let e1_ssa = e1.emit_mlir(prefix, ctx.clone());
                let e2_ssa = e2.emit_mlir(prefix, ctx.clone());
                (format!("%tmp_{prefix}_add"), format!("comb.add {e1_ssa}, {e2_ssa} : {typ_name}"))
            },
            _ => panic!(),
        };

        println!("    {name} = {op}");
        name.to_string()
    }
}

fn type_to_mlir(typ: Arc<Type>) -> String {
    match &*typ {
        Type::Word(n) => format!("i{n}"),
        _ => panic!(),
    }
}
