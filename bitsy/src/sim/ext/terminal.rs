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
    fn incoming_ports(&self) -> Vec<PortName> { vec!["out_data".to_string(), "out_valid".to_string()] }
    fn outgoing_ports(&self) -> Vec<PortName> { vec![] }

    fn update(&mut self, port: &PortName, value: Value) -> Vec<(PortName, Value)> {
        if value.is_x() {
            return vec![];
        }
        if port == "out_valid" {
            self.2 = value.to_bool().unwrap();
        } else if port == "out_data" {
            if let Some(n) = value.to_u64() {
                if n < 256 {
                    if let Ok(k) = n.try_into() {
                        if let Some(ch) = char::from_u32(k) {
                            self.0 = ch;
                        }
                    }
                }
            }
        }
        vec![]
    }

    fn clock(&mut self) -> Vec<(PortName, Value)> {
        if self.2 {
            let s = self.0;
            eprintln!("TERMINAL OUTPUT: {}", s as u32);
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
