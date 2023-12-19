use super::*;
use std::sync::Arc;
use std::collections::BTreeMap;

impl Package {
    pub fn emit_mlir(&self) {
        for moddef in self.moddefs() {
            if let Component::Mod(_loc, _name, _children, _wires, _whens) = &*moddef {
                self.emit_mlir_moddef(moddef);
            }
        }
    }

    fn emit_mlir_moddef(&self, moddef: Arc<Component>) {
        let mut ports: Vec<(bool, String, Type)> = vec![];
        let mut output_ports: Vec<String> = vec![];
        let mut output_port_ssas: BTreeMap<String, String> = BTreeMap::new();
        let mut output_port_types: Vec<Type> = vec![];

        for (_path, port) in moddef.port_paths() {
            let name = port.name().to_string();
            let typ = self.type_of(port.clone()).unwrap();
            ports.push((port.is_incoming_port(), name.clone(), typ.clone()));
            if port.is_outgoing_port() {
                output_ports.push(name);
                output_port_types.push(typ);
            }
        }

        let ctx = self.context_for(moddef.clone());

        println!("hw.module @{}(", moddef.name());
        self.emit_mlir_moddef_portlist(&ports);
        println!(") {{");

        for (i, Wire(_loc, target, expr, wire_type)) in moddef.wires().iter().enumerate() {
            match wire_type {
                WireType::Direct => {
                    let ssa = expr.emit_mlir(format!("$comb{i}"), ctx.clone());
                    let target_string = target.to_string();
                    if output_ports.contains(&target_string) {
                        output_port_ssas.insert(target_string, ssa);
                    } else if let Some(Component::Node(_loc, name, typ)) = moddef.child(target).as_ref().map(|arc| &**arc) {
                        let type_name = type_to_mlir(typ.clone());
                        println!("    %{name} = comb.add {ssa} : {type_name}");
                    }
                },
                WireType::Latch => {
                    let next_ssa = expr.emit_mlir(format!("$comb{i}"), ctx.clone());
                    let target_string = target.to_string();
                    let reg = moddef.child(target).unwrap();
                    let typ = reg.type_of().unwrap();
                    let reset = reg.reset().unwrap();
                    let reset_ssa = reset.emit_mlir(format!("$reset{i}"), ctx.clone());
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
        if output_port_ssas.len() > 0 {
            println!("    hw.output {} : {}", output_port_ssas.join(","), output_port_types.join(","));
        }
        println!("}}");
    }

    fn emit_mlir_moddef_portlist(&self, ports: &[(bool, String, Type)]) {
        println!("    in %_clock : !seq.clock,");
        print!("    in %_reset : i1");
        if ports.len() > 0 {
            println!(",");
        } else {
            println!();
        }

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
    fn emit_mlir(&self, prefix: String, ctx: Context<Path, Type>) -> String {
        let typ: Type = self.type_of();
        let type_name = type_to_mlir(typ.clone());

        match self {
            Expr::Reference(_loc, _typ, name) => {
                format!("%{name}")
            },
            Expr::Word(_loc, _typ, _w, n) => {
                let name = format!("%{prefix}_word");
                println!("    {name} = hw.constant {n} : {type_name}");
                name
            },
            Expr::Enum(_loc, typ, _typedef, valname) => {
                let name = format!("%{prefix}_enum");
                let typedef = if let Type::Enum(typedef) = typ.get().unwrap() {
                    typedef
                } else {
                    panic!();
                };
                let v = typedef.value_of(&*valname).unwrap();
                println!("    {name} = hw.constant {v} : {type_name}");
                name
            },
            Expr::ToWord(_loc, _typ, e1) => {
                let name = format!("%{prefix}_toword");
                let e1_ssa = e1.emit_mlir(format!("{prefix}_e1"), ctx.clone());
                println!("    {name} = comb.add {e1_ssa} : {type_name}");
                name
            },
            Expr::UnOp(_loc, _typ, UnOp::Not, e1) => {
                let name = format!("%{prefix}_not");
                let e1_ssa = e1.emit_mlir(format!("{prefix}_not_e1"), ctx.clone());
                // %c-1_i8 = hw.constant -1 : i8
                // %0 = comb.xor bin %a, %c-1_i8 : i8
                println!("    %{prefix}_not_negone = hw.constant -1 : {type_name}");
                println!("    {name} = comb.xor {e1_ssa}, %{prefix}_not_negone : {type_name}");
                name
            },
            Expr::BinOp(_loc, _typ, BinOp::Add, e1, e2) => {
                let name = format!("%{prefix}_add");
                let e1_ssa = e1.emit_mlir(format!("{prefix}_add_e1"), ctx.clone());
                let e2_ssa = e2.emit_mlir(format!("{prefix}_add_e2"), ctx.clone());
                println!("    {name} = comb.add {e1_ssa}, {e2_ssa} : {type_name}");
                name
            },
            Expr::BinOp(_loc, _typ, BinOp::Sub, e1, e2) => {
                let name = format!("%{prefix}_sub");
                let e1_ssa = e1.emit_mlir(format!("{prefix}_sub_e1"), ctx.clone());
                let e2_ssa = e2.emit_mlir(format!("{prefix}_sub_e2"), ctx.clone());
                println!("    {name} = comb.sub {e1_ssa}, {e2_ssa} : {type_name}");
                name
            },
            Expr::BinOp(_loc, _typ, BinOp::And, e1, e2) => {
                let name = format!("%{prefix}_and");
                let e1_ssa = e1.emit_mlir(format!("{prefix}_and_e1"), ctx.clone());
                let e2_ssa = e2.emit_mlir(format!("{prefix}_and_e2"), ctx.clone());
                println!("    {name} = comb.and {e1_ssa}, {e2_ssa} : {type_name}");
                name
            },
            Expr::BinOp(_loc, _typ, BinOp::Or, e1, e2) => {
                let name = format!("%{prefix}_or");
                let e1_ssa = e1.emit_mlir(format!("{prefix}_or_e1"), ctx.clone());
                let e2_ssa = e2.emit_mlir(format!("{prefix}_or_e2"), ctx.clone());
                println!("    {name} = comb.or {e1_ssa}, {e2_ssa} : {type_name}");
                name
            },
            Expr::BinOp(_loc, _typ, BinOp::Xor, e1, e2) => {
                let name = format!("%{prefix}_xor");
                let e1_ssa = e1.emit_mlir(format!("{prefix}_or_e1"), ctx.clone());
                let e2_ssa = e2.emit_mlir(format!("{prefix}_or_e2"), ctx.clone());
                println!("    {name} = comb.xor {e1_ssa}, {e2_ssa} : {type_name}");
                name
            },
            Expr::BinOp(_loc, _typ, BinOp::Eq, e1, e2) => {
                let name = format!("%{prefix}_eq");
                let e1_type_name = type_to_mlir(e1.type_of());
                let e1_ssa = e1.emit_mlir(format!("{prefix}_eq_e1"), ctx.clone());
                let e2_ssa = e2.emit_mlir(format!("{prefix}_eq_e2"), ctx.clone());
                // %0 = comb.icmp bin eq %a, %b : i8
                println!("    {name} = comb.icmp bin eq {e1_ssa}, {e2_ssa} : {e1_type_name}");
                name
            },
            Expr::BinOp(_loc, _typ, BinOp::Lt, e1, e2) => {
                let name = format!("%{prefix}_eq");
                let e1_type_name = type_to_mlir(e1.type_of());
                let e1_ssa = e1.emit_mlir(format!("{prefix}_lt_e1"), ctx.clone());
                let e2_ssa = e2.emit_mlir(format!("{prefix}_lt_e2"), ctx.clone());
                // %0 = comb.icmp bin ult %a, %b : i8
                println!("    {name} = comb.icmp bin ult {e1_ssa}, {e2_ssa} : {e1_type_name}");
                name
            },
            Expr::If(_loc, _typ, cond, e1, e2) => {
                let name = format!("%{prefix}_if");
                let cond_ssa = cond.emit_mlir(format!("{prefix}_if_cond"), ctx.clone());
                let e1_ssa   =   e1.emit_mlir(format!("{prefix}_if_e1"),   ctx.clone());
                let e2_ssa   =   e2.emit_mlir(format!("{prefix}_if_e2"),   ctx.clone());
                // %0 = comb.mux bin %in, %a, %b : i8
                println!("    {name} = comb.mux bin {cond_ssa}, {e1_ssa}, {e2_ssa} : {type_name}");
                name
            },
            Expr::Mux(_loc, _typ, cond, e1, e2) => {
                let name = format!("%{prefix}_mux");
                let cond_ssa = cond.emit_mlir(format!("{prefix}_mux_cond"), ctx.clone());
                let e1_ssa   =   e1.emit_mlir(format!("{prefix}_mux_e1"),   ctx.clone());
                let e2_ssa   =   e2.emit_mlir(format!("{prefix}_mux_e2"),   ctx.clone());
                // %0 = comb.mux bin %in, %a, %b : i8
                println!("    {name} = comb.mux bin {cond_ssa}, {e1_ssa}, {e2_ssa} : {type_name}");
                name
            },
            Expr::Cat(_loc, _typ, es) => {
            let name = format!("%{prefix}_cat");
                let mut es_ssas = vec![];
                let mut es_typenames = vec![];
                for (i, e) in es.iter().enumerate() {
                    let ssa = e.emit_mlir(format!("{prefix}_cat_e{i}"),ctx.clone());
                    let width = e.type_of().bitwidth();
                    let type_name = format!("i{width}");
                    es_ssas.push(ssa);
                    es_typenames.push(type_name);
                }

                println!("    {name} = comb.concat {} : {}", es_ssas.join(", "), es_typenames.join(", "));
                name
            },
            Expr::Sext(_loc, _typ, e1) => {
                let name = format!("%{prefix}_sext");
                match (typ, e1.type_of()) {
                    (Type::Word(outer_width), Type::Word(inner_width)) => {
                        assert!(outer_width >= inner_width);
                        let extension_width = outer_width - inner_width;
                        let e1_ssa = e1.emit_mlir(format!("{prefix}_sext_e1"),   ctx.clone());
                        // %c0_i7 = hw.constant 0 : i7
                        // %0 = comb.concat %c0_i7, %a : i7, i1
                        println!("    %{prefix}_sext_zero = hw.constant 0 : i{extension_width}");
                        println!("    {name} = comb.concat %{prefix}_sext_zero, {e1_ssa} : i{extension_width}, i{inner_width}");
                        name
                    },
                    _ => panic!(),
                }
            },
            /*
            Expr::Let(loc, _typ, name, e, b) => {
            let name = format!("%{prefix}_let");
                let e_ssa = e.emit_mlir(format!("{prefix}_let_{name}"), ctx.clone());
                let b_ssa = b.emit_mlir(format!("{prefix}_let_{name}"), new_ctx);

                println!("comb.add %{b_ssa} : {type_name}");
                name
            },
            */
            Expr::Idx(_loc, _typ, e1, i) => {
                let name = format!("%{prefix}_idx");
                let e1_type_name = type_to_mlir(e1.type_of());
                let e1_ssa = e1.emit_mlir(format!("{prefix}_idx_e1"), ctx.clone());
                // %0 = comb.extract %b from 0 : (i8) -> i1
                println!("    {name} = comb.extract {e1_ssa} from {i} : ({e1_type_name}) -> i1");
                name
            },
            Expr::IdxRange(_loc, _typ, e1, j, i) => {
                let name = format!("%{prefix}_idxrange");
                let e1_type_name = type_to_mlir(e1.type_of());
                let width = j - i;
                let e1_ssa = e1.emit_mlir(format!("{prefix}_idxrange_e1"), ctx.clone());
                // %0 = comb.extract %b from 0 : (i8) -> i3
                println!("    {name} = comb.extract {e1_ssa} from {i} : ({e1_type_name}) -> i{width}");
                name
            }
            _ => panic!("Can't lower expression {self:?}"),
        }
    }
}

fn type_to_mlir(typ: Type) -> String {
    match typ {
        Type::Word(n) => format!("i{n}"),
        Type::Struct(typedef) => {
            let typedef = typedef;
            let n = typedef.bitwidth();
            format!("i{n}")
        },
        Type::Enum(typedef) => {
            let n = typedef.bitwidth();
            format!("i{n}")
        }
        _ => panic!("Can't lower type to MLIR directly"),
    }
}
