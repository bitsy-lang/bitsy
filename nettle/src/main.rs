#![allow(unused, dead_code)]

use std::sync::Arc;
use std::io::Write;


#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub parser);

pub mod ast;
pub mod sim;
pub mod pretty;
pub mod wavedump;

fn main() {
    let argv: Vec<String> = std::env::args().collect();
    let default = "Top.net".to_string();
    let filename = argv.get(1).unwrap_or(&default);

    let parser = parser::NettleParser::new();
    let file = std::fs::read_to_string(filename).unwrap();
    let circuit = Arc::new(parser.parse(&file).unwrap());

    let mut simulator = sim::Simulator::new(circuit.clone());

    let top_in_signal = simulator.signal_by_path("top.in").unwrap();

    let mut buf = String::new();

    simulator.reset(
        sim::Domain::default(),
        &[
            (top_in_signal, ast::Value::Bool(true)),
        ],
    );

    let wavedump_mod = Arc::new(wavedump::Mod {
        name: "Top".to_string(),
        vars: vec![
            wavedump::Var {
                typ: "wire".to_string(),
                size: 1,
                name: "foo".to_string(),
                indexing: "".to_string(),
            },
        ],
        mods: vec![],
    });

    let mut wave = wavedump::Wavedump::new(wavedump_mod);
    wave.write_header(&mut buf);

    println!("{buf}");

//    println!("{}", pretty::pretty_print(&circuit));
}
