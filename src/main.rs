use std::sync::Arc;

#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub parser);

pub mod ast;
pub mod sim;

fn main() {
    let parser = parser::CircuitParser::new();
    let file = std::fs::read_to_string("Top.bitsy").unwrap();
    let circuit = parser.parse(&file).unwrap();
//    dbg!(&circuit);

    let mut simulator = sim::Simulator::new(Arc::new(circuit), "Top");

    let top_in_signal = simulator.signal_by_path("top.in").unwrap();
    let top_foo_signal = simulator.signal_by_path("top.foo").unwrap();
    let top_bar_signal = simulator.signal_by_path("top.bar").unwrap();

    loop {
        println!("--------------------------------------------------------------------------------");
        simulator.step(
            sim::Domain::default(),
            vec![
                (top_in_signal, ast::Value::Bool(true)),
                (top_foo_signal, ast::Value::Word(42)),
                (top_bar_signal, ast::Value::Word(21)),
            ],
        );
        std::thread::sleep(std::time::Duration::from_secs(1));
    }
}
