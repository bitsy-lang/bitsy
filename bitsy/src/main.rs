#![allow(unused, dead_code)]

use nettle::ast::Nettle;
use bitsy::Bitsy;

fn main() {
    init_logging();

    let argv: Vec<String> = std::env::args().collect();
    let default = "Top.bitsy".to_string();
    let filename = argv.get(1).unwrap_or(&default);
    let text = std::fs::read_to_string(filename).unwrap();

    let mut bitsy = Bitsy::new();
    bitsy.add(&text);
}

fn init_logging() {
    simple_logger::SimpleLogger::new()
        .with_level(log::LevelFilter::Debug)
        .init()
        .unwrap();
}
