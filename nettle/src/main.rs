#![allow(dead_code)]

mod circuit;
mod parse;
mod value;
mod expr;
mod nettle;
mod testbench;
mod path;
mod ext;
#[cfg(test)]
mod tests;

use circuit::*;
use parse::*;
use value::*;
use expr::*;
use nettle::*;
use testbench::*;
use path::*;
use ext::*;

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::sync::Arc;

fn main() {
    let argv: Vec<String> = std::env::args().collect();
    let default = "Top.nettle".to_string();
    let filename = argv.get(1).unwrap_or(&default);
    let text = std::fs::read_to_string(filename).unwrap();

    let tb = if let Some(tb_filename) = testbench_for(filename) {
        println!("Using testbench file: {tb_filename}");
        read_testbench_file(&tb_filename).unwrap()
    } else {
        println!("No testbench file");
        Testbench(vec![TestbenchCommand::Debug])
    };

    let top = parse_top(&text);
    let monitor = Box::new(Monitor::new());

    let mut nettle =
        Nettle::new(&top)
            .ext("top.vip", monitor);

    let verbose = true;
    for command in tb.0 {
        match command {
            TestbenchCommand::Peek(terminal) => {
                print!("PEEK {terminal} ");
                let value = nettle.peek(terminal);
                if verbose {
                    println!("=> {value:?}");
                }
            },
            TestbenchCommand::Poke(terminal, value) => {
                if verbose {
                    println!("POKE {terminal} <= {value:?}");
                }
                nettle.poke(terminal, value);
            },
            TestbenchCommand::Set(terminal, value) => {
                if verbose {
                    println!("SET {terminal} = {value:?}");
                }
                nettle.set(terminal, value);
            },
            TestbenchCommand::Clock => {
                if verbose {
                    println!("CLOCK");
                }
                nettle.clock();
            },
            TestbenchCommand::Reset => {
                if verbose {
                    println!("RESET");
                }
                nettle.reset();
            },
            TestbenchCommand::Debug => {
                println!("{nettle:#?}");
            },
            TestbenchCommand::Assert(e) => {
                let result = e.eval(&nettle);
                if result != Value::Bit(true) {
                    println!("Assertion failed: {e:?}");
                    for path in e.paths() {
                        println!("    {path} => {:?}", nettle.peek(path.clone()));

                    }
                    panic!("");
                }
            },
        }
    }
}
