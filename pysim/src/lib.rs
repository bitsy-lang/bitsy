use pyo3::prelude::*;
use pyo3::types::PyList;
use std::collections::BTreeMap;
use std::sync::Arc;


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
    fn load_from(filename: &str, exts: Option<&PyList>) -> Sim {
        let text = std::fs::read_to_string(filename).unwrap().to_string();
        let circuit = bitsy::parse_top(&text, None).unwrap();

        let mut exts_map: BTreeMap<bitsy::Path, Box<dyn bitsy::ExtInstance>> = BTreeMap::new();
        if let Some(exts) = exts {
            for ext in exts.into_iter() {
                let Ext(path, name) = ext.extract().unwrap();
                let e: Box<dyn bitsy::ExtInstance> = match name {
                    "Monitor" => {
                        let e = Box::new(bitsy::ext::monitor::Monitor::new());
                        e
                    },
                    "RiscVDecoder" => {
                        let e = Box::new(bitsy::ext::riscv_decoder::RiscVDecoder::new());
                        e
                    },
                    "Ram" => {
                        let e = Box::new(bitsy::ext::ram::Ram::new());
                        e
                    },
                    "Mem" => {
                        let e = Box::new(bitsy::ext::mem::Mem::new());
                        e
                    },
                    "Video" => {
                        let e = Box::new(bitsy::ext::video::Video::new());
                        e
                    },
                    "Terminal" => {
                        let e = Box::new(bitsy::ext::terminal::Terminal::new());
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
            bitsy::Value::Word(_width, n) => Value(n),
            _ => todo!(),
        }
    }

    fn poke(&mut self, path: &str, value: &Value) {
        let Value(n) = value;
        self.sim.poke(path, bitsy::Value::Word(u64::MAX, *n));
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
        let circuit = bitsy::parse_top(&text, None).unwrap();
        let package = circuit.package().clone();
        Package {
            package,
        }
    }

    pub fn moddef_names(&self) -> PyResult<Vec<String>> {
        let results: Vec<String> = self.package.moddefs().iter().map(|moddef| moddef.name().to_string()).collect();
        Ok(results)
    }
}
