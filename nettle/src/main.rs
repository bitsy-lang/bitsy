#![allow(dead_code)]
use nettle::*;

mod repl;
mod testbench;
use repl::*;
use testbench::*;


use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    let argv: Vec<String> = std::env::args().collect();
    let default = "Top.ntl".to_string();
    let filename = argv.get(1).unwrap_or(&default);
    let text = std::fs::read_to_string(filename).unwrap().to_string().leak();

    let top = parse_top(text)?;
    if let Err(errors) = top.check() {
        for (path, error) in &errors {
            eprintln!("{path}: {error:?}");
        }
        eprintln!("Circuit has {} errors.", errors.len());
        std::process::exit(1);
    }

    let testbench = if let Some(tb_filename) = testbench_for(filename) {
        println!("Using testbench file: {tb_filename}");
        let text = std::fs::read_to_string(tb_filename).unwrap();
        let tb: Testbench = parse_testbench(&text).unwrap();
        tb
    } else {
        println!("No testbench file");
        let command = TestbenchCommand::Debug;
        Testbench(vec![], vec![command])
    };

    let sim: Sim = make_sim(top.clone(), &testbench);
    let mut repl = Repl::new(sim, top, testbench);
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
fn make_sim(top: Circuit, testbench: &Testbench) -> Sim {
    let mut exts: BTreeMap<Path, Box<dyn ExtInstance>> = BTreeMap::new();
    for TestbenchLink(path, extname, params) in &testbench.0 {
        let mut params_map: BTreeMap<String, String> = params.iter().cloned().collect::<BTreeMap<_, _>>();
        let ext: Box<dyn ExtInstance> = match extname.as_str() {
            "Monitor" => {
                let e = Box::new(nettle::ext::monitor::Monitor::new());
                e
            },
            "RiscVDecoder" => {
                let e = Box::new(nettle::ext::riscv_decoder::RiscVDecoder::new());
                e
            },
            "Ram" => {
                let mut e = Box::new(nettle::ext::ram::Ram::new());
                if let Some(data_filename) = params_map.remove("file") {
                    e.load_from_file(data_filename.clone()).expect(&format!("Couldn't load {data_filename}"));
                }
                e
            },
            "Mem" => {
                let mut e = Box::new(nettle::ext::mem::Mem::new());
                if let Some(data_filename) = params_map.remove("file") {
                    e.load_from_file(data_filename.clone()).expect(&format!("Couldn't load {data_filename}"));
                }
                e
            },
            "Video" => {
                let mut e = Box::new(nettle::ext::video::Video::new());
                if params_map.remove("disabled") == Some("true".to_string()) {
                    e.disable()
                }
                e
            },
            _ => panic!("Unknown ext module being linked: {extname}")
        };
        assert!(params_map.is_empty(), "Unused params for ext module linkage: {path}: {params_map:?}");
        exts.insert(path.clone(), ext);
    }
    Sim::new_with_exts(&top, exts)
}
