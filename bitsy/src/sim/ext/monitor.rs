use super::*;
use std::collections::BTreeMap;

/// A monitor.
/// On reset and on clock, whatever value is presesnted to the `in` port is printed.
#[derive(Debug)]
pub struct Monitor(BTreeMap<Path, Option<String>>);

impl Monitor {
  pub fn new() -> Monitor {
      Monitor(BTreeMap::new())
  }
}

impl Ext for Monitor {
    fn name(&self) -> String {
        "Monitor".to_string()
    }

    fn incoming_ports(&self) -> Vec<PortName> { vec!["in".to_string()] }
    fn outgoing_ports(&self) -> Vec<PortName> { vec![] }

    fn instantiate(&mut self, path: Path) {
        self.0.insert(path, None);
    }

    fn update(&mut self, path: Path, _port: &PortName, value: Value) -> Vec<(PortName, Value)> {
        self.0.insert(path, Some(format!("{value:?}")));
        vec![]
    }

    fn clock(&mut self, path: Path) -> Vec<(PortName, Value)> {
        if let Some(Some(s)) = &self.0.get(&path) {
            println!("{path}: {s}");
            self.0.insert(path, None);
        }
        vec![]
    }

    fn reset(&mut self, path: Path) -> Vec<(PortName, Value)> {
        if let Some(Some(s)) = &self.0.get(&path) {
            println!("{path}: {s}");
            self.0.insert(path, None);
        }
        vec![]
    }
}
