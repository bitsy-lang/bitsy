use std::collections::BTreeMap;
use rhai::Engine;
use rhai::Array;
use rhai::Dynamic;
use bitsy::Package;
use bitsy::sim::Sim;
use bitsy::sim::Value;
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
    let engine = make_engine();
    let _= engine.eval_file::<Dynamic>("script.rhai".into()).unwrap_or_else(|e| {
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

    let package = match bitsy::load_package_from_string(&text) {
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

    let mut exts_map: BTreeMap<bitsy::Path, Box<dyn bitsy::sim::ext::ExtInstance>> = BTreeMap::new();
    /*
    if let Some(exts) = exts {
        for ext in exts.into_iter() {
            let Ext(path, name) = ext.extract().unwrap();
            let e: Box<dyn bitsy::sim::ext::ExtInstance> = match name {
                "Monitor" => {
                    let e = Box::new(bitsy::sim::ext::monitor::Monitor::new());
                    e
                },
                "RiscVDecoder" => {
                    let e = Box::new(bitsy::sim::ext::riscv_decoder::RiscVDecoder::new());
                    e
                },
                "Ram" => {
                    let e = Box::new(bitsy::sim::ext::ram::Ram::new());
                    e
                },
                "Mem" => {
                    let e = Box::new(bitsy::sim::ext::mem::Mem::new());
                    e
                },
                "Video" => {
                    let e = Box::new(bitsy::sim::ext::video::Video::new());
                    e
                },
                "Terminal" => {
                    let e = Box::new(bitsy::sim::ext::terminal::Terminal::new());
                    e
                },
                _ => panic!("Unknown ext module being linked: {name}")
            };
            exts_map.insert(path, e);
        }
    }
    */
    let sim = bitsy::sim::Sim::new_with_exts(&circuit, exts_map);
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
