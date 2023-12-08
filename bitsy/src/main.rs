#![allow(dead_code)]
use bitsy::*;
use bitsy::sim::*;

mod repl;
mod testbench;
use repl::*;
use testbench::*;
use clap::Parser;

use std::collections::BTreeMap;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    filename: String,

    #[arg(long)]
    tb: Option<String>,

    #[arg(long)]
    top: Option<String>,

    #[arg(short, long, default_value_t = false)]
    debug: bool,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let text = std::fs::read_to_string(&args.filename).unwrap().to_string();

    let testbench_filename = args.tb.or_else(|| testbench_for(&args.filename));
    let testbench = if let Some(tb_filename) = testbench_filename {
        println!("Using testbench file: {tb_filename}");
        let text = std::fs::read_to_string(tb_filename.clone()).unwrap();
        let tb: Testbench = parse_testbench(&text).expect(&format!("Error parsing testbench: {tb_filename}"));
        tb
    } else {
        println!("No testbench file");
        let command = TestbenchCommand::Debug;
        Testbench(None, vec![], vec![command])
    };

    let top = args.top.or_else(|| testbench.0.clone());
    let circuit = match parse_top(&text, top.as_deref()) {
        Ok(circuit) => circuit,
        Err(errors) => {
            for error in &errors {
                eprintln!("{error:?}");
            }
            eprintln!("Circuit has {} errors.", errors.len());
            std::process::exit(1);
        },
    };
    if let Err(errors) = circuit.package().check() {
        for (path, error) in &errors {
            eprintln!("{path}: {error:?}");
        }
        eprintln!("Circuit has {} errors.", errors.len());
        std::process::exit(1);
    }


    let sim: Sim = make_sim(circuit.clone(), &testbench);
    let mut repl = Repl::new(sim, circuit, testbench);
    repl.run();
    Ok(())
}

fn testbench_for(filename: &str) -> Option<String> {
    let path = std::path::Path::new(filename);
    let parent: &std::path::Path = path.parent().unwrap();
    let file = format!("{}.tb", path.file_stem().unwrap().to_str().unwrap());
    let tb_filename = parent.join(file).into_os_string().into_string().unwrap();
    let exists = std::fs::metadata(&tb_filename).map(|metadata| metadata.is_file()).unwrap_or(false);

    if exists {
        Some(tb_filename)
    } else {
        None
    }
}

fn make_sim(circuit: Circuit, testbench: &Testbench) -> Sim {
    let mut exts: BTreeMap<Path, Box<dyn ExtInstance>> = BTreeMap::new();
    for TestbenchLink(path, extname, params) in &testbench.1 {
        let mut params_map: BTreeMap<String, String> = params.iter().cloned().collect::<BTreeMap<_, _>>();
        let ext: Box<dyn ExtInstance> = match extname.as_str() {
            "Monitor" => {
                let e = Box::new(bitsy::ext::monitor::Monitor::new());
                e
            },
            "RiscVDecoder" => {
                let e = Box::new(bitsy::ext::riscv_decoder::RiscVDecoder::new());
                e
            },
            "Ram" => {
                let mut e = Box::new(bitsy::ext::ram::Ram::new());
                if let Some(data_filename) = params_map.remove("file") {
                    e.load_from_file(data_filename.clone()).expect(&format!("Couldn't load {data_filename}"));
                }
                e
            },
            "Mem" => {
                let mut e = Box::new(bitsy::ext::mem::Mem::new());
                if let Some(data_filename) = params_map.remove("file") {
                    e.load_from_file(data_filename.clone()).expect(&format!("Couldn't load {data_filename}"));
                }
                e
            },
            "Video" => {
                let mut e = Box::new(bitsy::ext::video::Video::new());
                if params_map.remove("disabled") == Some("true".to_string()) {
                    e.disable()
                }
                e
            },
            "Terminal" => {
                let e = Box::new(bitsy::ext::terminal::Terminal::new());
                e
            },
            _ => panic!("Unknown ext module being linked: {extname}")
        };
        assert!(params_map.is_empty(), "Unused params for ext module linkage: {path}: {params_map:?}");
        exts.insert(path.clone(), ext);
    }
    Sim::new_with_exts(&circuit, exts)
}
