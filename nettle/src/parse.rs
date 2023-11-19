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

fn mod_to_module(m: Mod) -> ModuleDef {
    let Mod(name, decls) = m;
    let mut module = Module::new(&name);

    for decl in decls {
        match decl {
            ModDecl::Node(name, _typ) => module = module.node(&name),
            ModDecl::Reg(name, _typ, reset) => module = module.reg(&name, reset),
            ModDecl::Mod(submodule) => {
                let name = submodule.0.to_string();
                module = module.instantiate(&name, &mod_to_module(submodule))
            },
            ModDecl::Wire(terminal, expr) => module = module.wire(&terminal, &expr),
            ModDecl::Ext(name, ports) => {
                let ports: Vec<_> = ports.iter().map(|(name, _typ)| name.as_str()).collect();
                module = module.ext(&name, &ports)
            },
        }
    }
    module
}

pub fn parse_top(circuit: &str) -> Module {
    let m: Mod = grammar::TopParser::new().parse(circuit).unwrap();
    mod_to_module(m).build()
}

impl From<&str> for Expr {
    fn from(expr: &str) -> Expr {
        *grammar::ExprParser::new().parse(expr).unwrap()
    }
}

pub fn parse_testbench(testbench: &str) -> Testbench {
    grammar::TestbenchParser::new().parse(testbench).unwrap()
}
