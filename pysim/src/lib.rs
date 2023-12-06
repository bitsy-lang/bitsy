use pyo3::prelude::*;
use std::collections::BTreeMap;


#[pymodule]
fn pysim(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<Package>()?;
    m.add_class::<Sim>()?;
    m.add_class::<Value>()?;
    Ok(())
}

#[pyclass(unsendable)]
struct Sim {
    sim: nettle::Sim,
}

#[pymethods]
impl Sim {
    #[staticmethod]
    fn load_from(filename: &str) -> Sim {
        let text = std::fs::read_to_string(filename).unwrap().to_string();
        let circuit = nettle::parse_top(&text, None).unwrap();

        let mut exts: BTreeMap<nettle::Path, Box<dyn nettle::ExtInstance>> = BTreeMap::new();
        let e = Box::new(nettle::ext::monitor::Monitor::new());
        exts.insert("Top.monitor".into(), e);
        let sim = nettle::Sim::new_with_exts(&circuit, exts);

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
            nettle::Value::Word(_width, n) => Value(n),
            _ => todo!(),
        }
    }

    fn poke(&mut self, path: &str, value: Value) {
        let Value(n) = value;
        self.sim.poke(path, nettle::Value::Word(u64::MAX, n));
    }
}

#[pyclass]
#[derive(Clone)]
struct Value(u64);

#[pymethods]
impl Value {
    fn __str__(&self) -> String {
        format!("{}", self.0)
    }
}

#[pyclass]
struct Package {
    package: nettle::Package,
}

#[pymethods]
impl Package {
    #[new]
    fn new(filename: &str) -> Self {
        let text = std::fs::read_to_string(filename).unwrap().to_string();
        let circuit = nettle::parse_top(&text, None).unwrap();
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
