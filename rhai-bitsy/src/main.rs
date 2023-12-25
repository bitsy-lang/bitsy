use rhai::Engine;
use rhai::Array;
use rhai::Dynamic;
use bitsy_lang::Package;
use bitsy_lang::sim::Sim;
use bitsy_lang::sim::ext::Ext;
use bitsy_lang::sim::Value;
use std::sync::Arc;
use std::sync::Mutex;

fn make_engine() -> Engine {
    let mut engine = Engine::new();
    engine.set_max_call_levels(1024 * 1024);

    engine.register_type_with_name::<Arc<Package>>("Package")
        .register_fn("Package", package_load)
        .register_fn("sim", sim)
        .register_fn("moddefs", package_moddefs);

    engine.register_type_with_name::<Arc<Sim>>("Sim")
        .register_fn("clock", sim_clock)
        .register_fn("reset", sim_reset)
        .register_fn("peek", sim_peek)
        .register_fn("poke", sim_poke);

    engine.register_type_with_name::<Value>("Value")
        .register_fn("to_string", to_string)
        .register_fn("word", word);

    engine
}

fn word(x: i64, w: i64) -> Value {
    Value::Word(w.try_into().unwrap(), x.try_into().unwrap())
}

fn to_string(x: Value) -> String {
    format!("{x}")
}

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    let filename = args.get(1).cloned().unwrap_or_else(|| "script.rhai".to_string());
    let engine = make_engine();
    let _= engine.eval_file::<Dynamic>(filename.into()).unwrap_or_else(|e| {
        eprintln!("ERROR {e:?}");
        std::process::exit(1)
    });
}

fn package_load(path: &str) -> Arc<Package> {
    let text = if path.starts_with("http://") || path.starts_with("https://") {
        let client = reqwest::blocking::Client::builder()
            .redirect(reqwest::redirect::Policy::limited(5))
            .build()
            .unwrap();
        let response = client.get(path).send().unwrap();
        let body = response.text().unwrap();
        body
    } else {
        std::fs::read_to_string(path).unwrap().to_string()
    };

    let package = match bitsy_lang::load_package_from_string(&text) {
        Ok(package) => package,
        Err(errors) => {
            for error in &errors {
                eprintln!("{error:?}");
            }
            eprintln!("Circuit has {} errors.", errors.len());
            std::process::exit(1);
        },
    };
    Arc::new(package)
}

fn package_moddefs(package: Arc<Package>) -> Array {
    package.moddefs().iter().map(|c| c.name().to_string().into()).collect()
}

fn sim(package: Arc<Package>, top_name: &str) -> Arc<Mutex<Sim>> {
    let circuit = match package.top(&top_name) {
        Ok(circuit) => circuit,
        Err(error) => {
            eprintln!("{error:?}");
            eprintln!("Circuit has 1 errors.");
            std::process::exit(1);
        },
    };

    let exts: Vec<Box<dyn Ext>> = vec![
        Box::new(bitsy_lang::sim::ext::monitor::Monitor::new("Monitor".to_string())),
//        Box::new(bitsy_lang::sim::ext::riscv_decoder::RiscvDecoder::new()),
//        Box::new(bitsy_lang::sim::ext::ram::Ram::new()),
//        Box::new(bitsy_lang::sim::ext::mem::Mem::new()),
//        Box::new(bitsy_lang::sim::ext::instrmem::InstrMem::new()),
//        Box::new(bitsy_lang::sim::ext::video::Video::new()),
//        Box::new(bitsy_lang::sim::ext::terminal::Terminal::new()),
    ];
    let sim = bitsy_lang::sim::Sim::new(&circuit, exts);
    Arc::new(Mutex::new(sim))
}

fn sim_clock(sim: Arc<Mutex<Sim>>) {
    let mut sim = sim.lock().unwrap();
    sim.clock();
}

fn sim_reset(sim: Arc<Mutex<Sim>>) {
    let mut sim = sim.lock().unwrap();
    sim.reset();
}

fn sim_peek(sim: Arc<Mutex<Sim>>, path: &str) -> Value {
    let sim = sim.lock().unwrap();
    sim.peek(path)
}

fn sim_poke(sim: Arc<Mutex<Sim>>, path: &str, value: Value) {
    let mut sim = sim.lock().unwrap();
    sim.poke(path, value);
}
