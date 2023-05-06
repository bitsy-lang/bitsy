#![allow(unused, dead_code)]

use nettle::ast::Nettle;
use bitsy::Bitsy;

fn main() {
    let argv: Vec<String> = std::env::args().collect();
    let default = "Top.bitsy".to_string();
    let filename = argv.get(1).unwrap_or(&default);
    let text = std::fs::read_to_string(filename).unwrap();

    let mut bitsy = Bitsy::new();
    bitsy.add(&text);
    dbg!(&bitsy);

    /*
    let mut nettle_circuit = Nettle {
        domains: vec![nettle::ast::Domain("d".to_string())],
        signals: vec![],
    };
    */

    //println!("{}", nettle::pretty::pretty_print(&nettle_circuit));
}
