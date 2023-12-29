use bitsy_lang::sim::ext::Ext;
use bitsy_lang::sim::Sim;
//use bitsy_lang::sim::Value;
use std::collections::BTreeMap;
use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    program: String,
}

fn main() {
    let args = Args::parse();

    let text = std::fs::read_to_string("riscv.bitsy").unwrap().to_string();

    let package = match bitsy_lang::load_package_from_string(&text) {
        Ok(package) => package,
        Err(errors) => {
            for error in &errors {
                eprintln!("{error:?}");
            }
            eprintln!("Circuit has {} errors.", errors.len());
            std::process::exit(1);
        }
    };

    let top_name = "Top";

    let circuit = match package.top(&top_name) {
        Ok(circuit) => circuit,
        Err(error) => {
            eprintln!("{error:?}");
            eprintln!("Circuit has errors.");
            std::process::exit(1);
        }
    };

    let mut sim = make_sim(circuit.clone(), &args);
    sim.reset();
    loop {
        let ebreak = sim.peek("top.core.ebreak");
        // let ecall = sim.peek("top.core.ecall");
        let exception = sim.peek("top.core.exception");
        if ebreak.to_bool().unwrap() {
            break;
        } else if exception.to_bool().unwrap() {
            eprintln!("EXCEPTION");
            break;
        }
        sim.clock();
    }
    println!();
}

fn make_sim(circuit: bitsy_lang::Circuit, args: &Args) -> Sim {
    let mut exts: Vec<Box<dyn Ext>> = Vec::new();
    let links: Vec<(String, String, BTreeMap<String, String>)> = vec![
        (
            "RiscVDecoder".to_string(),
            "RiscVDecoder".to_string(),
            BTreeMap::new(),
        ),
        (
            "Terminal".to_string(),
            "Terminal".to_string(),
            BTreeMap::new(),
        ),
        (
            "InstrMem".to_string(),
            "Mem".to_string(),
            vec![
                ("file".to_string(), args.program.clone()),
                ("delay".to_string(), "0".to_string()),
            ]
            .into_iter()
            .collect(),
        ),
        (
            "DataMem".to_string(),
            "Mem".to_string(),
            vec![
                ("file".to_string(), "data/hello.bin".to_string()),
                ("delay".to_string(), "0".to_string()),
            ]
            .into_iter()
            .collect(),
        ),
    ];

    for (extname, driver, mut params_map) in links.into_iter() {
        let ext: Box<dyn Ext> = match driver.as_str() {
            "Monitor" => {
                let e = Box::new(bitsy_lang::sim::ext::monitor::Monitor::new(extname.clone()));
                e
            }
            "RiscVDecoder" => {
                let e = Box::new(bitsy_lang::sim::ext::riscv_decoder::RiscvDecoder::new(
                    extname.clone(),
                ));
                e
            }
            "Ram" => {
                let mut e = Box::new(bitsy_lang::sim::ext::ram::Ram::new(extname.clone()));
                if let Some(data_filename) = params_map.remove("file") {
                    e.load_from_file(data_filename.clone())
                        .expect(&format!("Couldn't load {data_filename}"));
                }
                e
            }
            "Mem" => {
                let mut e = Box::new(bitsy_lang::sim::ext::mem::Mem::new(extname.clone()));
                if let Some(data_filename) = params_map.remove("file") {
                    e.load_from_file(data_filename.clone())
                        .expect(&format!("Couldn't load {data_filename}"));
                }
                if let Some(cycles) = params_map.remove("delay") {
                    e.set_read_delay(cycles.parse().unwrap());
                }
                e
            }
            "Video" => {
                let mut e = Box::new(bitsy_lang::sim::ext::video::Video::new(extname.clone()));
                if params_map.remove("disabled") == Some("true".to_string()) {
                    e.disable()
                }
                e
            }
            "Terminal" => {
                let e = Box::new(bitsy_lang::sim::ext::terminal::Terminal::new(
                    extname.clone(),
                    None,
                ));
                e
            }
            _ => panic!("Unknown ext module being linked: {driver}"),
        };
        assert!(
            params_map.is_empty(),
            "Unused params for ext module linkage: {extname}: {params_map:?}"
        );
        exts.push(ext);
    }
    Sim::new(&circuit, exts)
}
