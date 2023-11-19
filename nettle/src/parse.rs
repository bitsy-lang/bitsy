use super::*;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(grammar);

#[derive(Debug)]
pub struct Mod(String, Vec<ModDecl>);

#[derive(Debug)]
pub enum ModDecl {
    Node(String, Type),
    Reg(String, Type, Value),
    Mod(Mod),
    Wire(Path, Expr),
    Ext(String, Vec<(String, Type)>),
}

#[derive(Debug)]
struct Ext(String, Vec<String>);

fn mod_to_circuit(m: Mod) -> CircuitNode {
    let Mod(name, decls) = m;
    let mut module = Circuit::new(&name);

    for decl in decls {
        match decl {
            ModDecl::Node(name, typ) => module = module.node(&name, typ),
            ModDecl::Reg(name, typ, reset) => module = module.reg(&name, typ, reset),
            ModDecl::Mod(submodule) => {
                let name = submodule.0.to_string();
                module = module.instantiate(&name, &mod_to_circuit(submodule))
            },
            ModDecl::Wire(terminal, expr) => module = module.wire(&terminal, &expr),
            ModDecl::Ext(name, ports) => {
                let ports: Vec<(&str, Type)> = ports.iter().map(|(name, typ)| (name.as_str(), *typ)).collect();
                module = module.ext(&name, &ports)
            },
        }
    }
    module
}

pub fn parse_top(circuit: &str) -> Circuit {
    let m: Mod = grammar::TopParser::new().parse(circuit).unwrap();
    mod_to_circuit(m).build()
}

impl From<&str> for Expr {
    fn from(expr: &str) -> Expr {
        *grammar::ExprParser::new().parse(expr).unwrap()
    }
}

pub fn parse_testbench(testbench: &str) -> Testbench {
    grammar::TestbenchParser::new().parse(testbench).unwrap()
}
