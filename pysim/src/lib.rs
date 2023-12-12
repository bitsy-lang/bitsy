use pyo3::prelude::*;
use pyo3::types::PyList;
use std::collections::BTreeMap;


#[pymodule]
fn pysim(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<Package>()?;
    m.add_class::<Sim>()?;
    m.add_class::<Value>()?;
    m.add_class::<Ext>()?;
    Ok(())
}

#[pyclass]
#[derive(Clone)]
struct Ext(bitsy::Path, &'static str);

#[pymethods]
impl Ext {
    #[staticmethod]
    fn monitor(path: &str) -> Ext {
        Ext(path.into(), "Monitor")
    }

    #[staticmethod]
    fn riscv_decover(path: &str) -> Ext {
        Ext(path.into(), "RiscVDecoder")
    }

    #[staticmethod]
    fn ram(path: &str) -> Ext {
        Ext(path.into(), "Ram")
    }

    #[staticmethod]
    fn mem(path: &str) -> Ext {
        Ext(path.into(), "Mem")
    }

    #[staticmethod]
    fn video(path: &str) -> Ext {
        Ext(path.into(), "Video")
    }

    #[staticmethod]
    fn terminal(path: &str) -> Ext {
        Ext(path.into(), "Terminal")
    }

}

#[pyclass(unsendable)]
struct Sim {
    sim: bitsy::sim::Sim,
}

#[pymethods]
impl Sim {
    #[staticmethod]
    fn load_from(filename: &str, top_name: &str, exts: Option<&PyList>) -> Sim {
        let text = std::fs::read_to_string(filename).unwrap().to_string();

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
        let circuit = match package.top(&top_name) {
            Ok(circuit) => circuit,
            Err(error) => {
                eprintln!("{error:?}");
                eprintln!("Circuit has 1 errors.");
                std::process::exit(1);
            },
        };

        let mut exts_map: BTreeMap<bitsy::Path, Box<dyn bitsy::sim::ext::ExtInstance>> = BTreeMap::new();
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
        let sim = bitsy::sim::Sim::new_with_exts(&circuit, exts_map);

        Sim {
            sim,
        }
    }

    fn reset(&mut self) {
        self.sim.reset();
    }

    fn clock(&mut self) {
        self.sim.clock();
    }

    fn peek(&self, path: &str) -> Value {
        match self.sim.peek(path) {
            bitsy::sim::Value::Word(_width, n) => Value(n),
            _ => todo!(),
        }
    }

    fn poke(&mut self, path: &str, value: &Value) {
        let Value(n) = value;
        self.sim.poke(path, bitsy::sim::Value::Word(u64::MAX, *n));
    }
}

#[pyclass]
struct Value(u64);

#[pymethods]
impl Value {
    fn __str__(&self) -> String {
        format!("{}", self.0)
    }
}

#[pyclass]
struct Package {
    package: bitsy::Package,
}

#[pymethods]
impl Package {
    #[new]
    fn new(filename: &str) -> Self {
        let text = std::fs::read_to_string(filename).unwrap().to_string();
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
        Package {
            package,
        }
    }

    pub fn moddef_names(&self) -> PyResult<Vec<String>> {
        let results: Vec<String> = self.package.moddefs().iter().map(|moddef| moddef.name().to_string()).collect();
        Ok(results)
    }
}
