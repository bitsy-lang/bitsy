use super::*;
use std::io::Write;

#[derive(Debug)]
pub struct Terminal {
    name: String,
    chr: char,
    file: std::fs::File,
    out_valid: bool,
}

impl Terminal {
  pub fn new(name: String) -> Terminal {
      let file = std::fs::File::create("terminal.txt").unwrap();
      Terminal {
          name,
          chr: '\0',
          file,
          out_valid: false,
      }
  }
}

impl Ext for Terminal {
    fn name(&self) -> String { self.name.clone() }
    fn instantiate(&mut self, _path: Path) {}
    fn incoming_ports(&self) -> Vec<PortName> { vec!["out_data".to_string(), "out_valid".to_string()] }
    fn outgoing_ports(&self) -> Vec<PortName> { vec![] }

    fn update(&mut self, _path: Path, port: &PortName, value: Value) -> Vec<(PortName, Value)> {
        if value.is_x() {
            return vec![];
        }
        if port == "out_valid" {
            self.out_valid = value.to_bool().unwrap();
        } else if port == "out_data" {
            if let Some(n) = value.to_u64() {
                if n < 256 {
                    if let Ok(k) = n.try_into() {
                        if let Some(ch) = char::from_u32(k) {
                            self.chr = ch;
                        }
                    }
                }
            }
        }
        vec![]
    }

    fn clock(&mut self, _path: Path) -> Vec<(PortName, Value)> {
        if self.out_valid {
            let s = self.chr;
            eprintln!("TERMINAL OUTPUT: {}", s as u32);
            write!(self.file, "{s}").unwrap();
            self.file.flush().unwrap();
        }
        vec![]
    }

    fn reset(&mut self, _path: Path) -> Vec<(PortName, Value)> {
        if self.out_valid {
            let s = self.chr;
            write!(self.file, "{s}").unwrap();
            self.file.flush().unwrap();
        }
        vec![]
    }
}
