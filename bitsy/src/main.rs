#![allow(unused, dead_code)]

use nettle::ast::Nettle;
use bitsy::Bitsy;
use bitsy::verilog::{Verilog, Module, Direction, Port};

fn main() {
    init_logging();

    let argv: Vec<String> = std::env::args().collect();
    let default = "Top.bitsy".to_string();
    let filename = argv.get(1).unwrap_or(&default);
    let text = std::fs::read_to_string(filename).unwrap();

//    let mut bitsy = Bitsy::new();
//    bitsy.add(&text);
    //dbg!(&bitsy);
    //

    let verilog = bitsy::verilog::Verilog {
        filename: "Top.v".to_string(),
        modules: vec![
            Module {
                name: "Top".to_string(),
                ports: vec![
                    Port {
                        name: "clock_12MHz".to_string(),
                        width: None,
                        direction: Direction::Input,
                    },
                    Port {
                        name: "write_data".to_string(),
                        width: Some(16),
                        direction: Direction::Input,
                    },
                ],
            },
        ],
    };
    verilog.dump();
}

fn init_logging() {
    simple_logger::SimpleLogger::new()
        .with_level(log::LevelFilter::Debug)
        .init()
        .unwrap();
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_main() {
        let filename = "Top.bitsy".to_string();
        let text = std::fs::read_to_string(filename).unwrap();

        let mut bitsy = Bitsy::new();
        bitsy.add(&text);
    }
}
