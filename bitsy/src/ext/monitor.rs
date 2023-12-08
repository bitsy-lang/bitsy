use super::*;

/// A monitor.
/// On reset and on clock, whatever value is presesnted to the `in` port is printed.
#[derive(Debug)]
pub struct Monitor(Option<String>);

impl Monitor {
  pub fn new() -> Monitor {
      Monitor(None)
  }
}

impl ExtInstance for Monitor {
    fn incoming_ports(&self) -> Vec<PortName> { vec!["in".to_string()] }
    fn outgoing_ports(&self) -> Vec<PortName> { vec![] }

    fn update(&mut self, _port: &PortName, value: Value) -> Vec<(PortName, Value)> {
        self.0 = Some(format!("{value:?}"));
        vec![]
    }

    fn clock(&mut self) -> Vec<(PortName, Value)> {
        if let Some(s) = &self.0 {
            println!("Monitor: {s}");
            self.0 = None
        }
        vec![]
    }

    fn reset(&mut self) -> Vec<(PortName, Value)> {
        if let Some(s) = &self.0 {
            println!("Monitor: {s}");
            self.0 = None
        }
        vec![]
    }
}
