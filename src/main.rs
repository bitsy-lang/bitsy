use std::sync::Arc;

#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub parser);

pub mod ast;
pub mod sim;

fn main() {
    let argv: Vec<String> = std::env::args().collect();
    let default = "Top.bitsy".to_string();
    let filename = argv.get(1).unwrap_or(&default);

    let parser = parser::CircuitParser::new();
    let file = std::fs::read_to_string(filename).unwrap();
    let circuit = parser.parse(&file).unwrap();
//    dbg!(&circuit);

    let mut simulator = sim::Simulator::new(Arc::new(circuit), "Top");

//    let top_in_signal = simulator.signal_by_path("top.in").unwrap();
    let top_foo_signal = simulator.signal_by_path("top.a").unwrap();
    let top_bar_signal = simulator.signal_by_path("top.b").unwrap();

    simulator.reset(
        sim::Domain::default(),
        &[
//            (top_in_signal, ast::Value::Bool(true)),
            (top_foo_signal, ast::Value::Bool(false)),
            (top_bar_signal, ast::Value::Bool(true)),
        ],
    );

    simulator.step(
        sim::Domain::default(),
        &[
//            (top_in_signal, ast::Value::Bool(true)),
            (top_foo_signal, ast::Value::Bool(false)),
            (top_bar_signal, ast::Value::Bool(true)),
        ],
    );
}
