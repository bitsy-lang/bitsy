use super::*;
use std::io::Write;

#[derive(Debug)]
pub struct Terminal(char, std::fs::File, bool);

impl Terminal {
  pub fn new() -> Terminal {
      let file = std::fs::File::create("terminal.txt").unwrap();
      Terminal('\0', file, false)
  }
}

impl ExtInstance for Terminal {
    fn incoming_ports(&self) -> Vec<PortName> { vec!["in_data".to_string(), "in_valid".to_string()] }
    fn outgoing_ports(&self) -> Vec<PortName> { vec![] }

    fn update(&mut self, port: &PortName, value: Value) -> Vec<(PortName, Value)> {
        if port == "in_valid" {
            self.2 = value.to_bool().unwrap();
        } else if port == "in_data" {
            self.0 = char::from_u32(value.to_u64().unwrap() as u32).unwrap();
        }
        vec![]
    }

    fn clock(&mut self) -> Vec<(PortName, Value)> {
        if self.2 {
            let s = self.0;
            write!(self.1, "{s}").unwrap();
            self.1.flush().unwrap();
        }
        vec![]
    }

    fn reset(&mut self) -> Vec<(PortName, Value)> {
        if self.2 {
            let s = self.0;
            write!(self.1, "{s}").unwrap();
            self.1.flush().unwrap();
        }
        vec![]
    }
}
