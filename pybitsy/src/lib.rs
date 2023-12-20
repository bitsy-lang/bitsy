use pyo3::prelude::*;
use pyo3::exceptions::PyValueError;
use pyo3::types::PyList;
use std::collections::BTreeMap;
//use bitsy::Type;
//use bitsy::sim::Value;

#[pymodule]
#[pyo3(name = "bitsy")]
fn pybitsy(py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<Package>()?;
    m.add_class::<Sim>()?;
    m.add_class::<Ext>()?;

    m.add_class::<Value>()?;
    m.add_class::<XXX>()?;
    m.add_class::<Word>()?;

    // https://github.com/PyO3/pyo3/discussions/2294#discussioncomment-4441099
    let initializer = PyClassInitializer::from(Value).add_subclass(XXX);
    let x = Py::new(py, initializer)?.into_py(py);
    m.add("X", x)?;

    Ok(())
}

#[pyclass(subclass)]
#[derive(Clone, Debug)]
struct Value;

#[pyclass(extends=Value)]
#[derive(Clone, Debug)]
struct XXX;

#[pymethods]
impl XXX {
    fn __str__(&self) -> String {
        format!("X")
    }

    fn __repr__(&self) -> String {
        format!("X")
    }
}

#[pyclass(extends=Value)]
#[derive(Clone)]
struct Word(Option<bitsy::Width>, u64);

#[pymethods]
impl Word {
    #[new]
    fn new(val: u64, width: Option<u64>) -> (Self, Value) {
        (Word(width, val), Value)
    }

    fn __str__(&self) -> String {
        match self {
            Word(Some(w), v) => format!("{v}w{w}"),
            Word(None, v) => format!("{v}"),
        }
    }

    fn __repr__(&self) -> String {
        self.__str__()
    }

    fn __int__(&self) -> i64 {
        self.1.try_into().unwrap()
    }
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

    fn peek(&self, py: Python, path: &str) -> PyResult<PyObject> {
        match self.sim.peek(path) {
            bitsy::sim::Value::X => {
                let initializer = PyClassInitializer::from(Value).add_subclass(XXX);
                Ok(Py::new(py, initializer)?.into_py(py))
            },
            bitsy::sim::Value::Word(w, n) => {
                let initializer = PyClassInitializer::from(Value).add_subclass(Word(Some(w), n));
                Ok(Py::new(py, initializer)?.into_py(py))
            },
            v => {
                eprintln!("peek() not implemented for {v:?}");
                todo!()
            },
        }
    }

    fn poke(&mut self, path: &str, value: &PyAny) -> PyResult<()> {
        let (width_received, v) = if let Ok(Word(w, v)) = value.extract() {
            (w, v)
        } else {
            return Err(PyValueError::new_err("value cannot be converted to a Word."));
        };
        let width = if let bitsy::Type::Word(w) = self.sim.type_of(path) {
            w
        } else {
            return Err(PyValueError::new_err(format!("Path does not have a Word type: {path}")));
        };

        if width_received.is_some() && width_received.unwrap() != width {
            return Err(PyValueError::new_err(format!("Path does not have a Word type: {path}")));
        }
        self.sim.poke(path, bitsy::sim::Value::Word(width, v));
        Ok(())
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
