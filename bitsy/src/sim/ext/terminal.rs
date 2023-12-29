use super::*;
use std::io::Write;

#[derive(Debug)]
pub struct Terminal {
    name: String,
    chr: char,
    file: Option<std::fs::File>,
    out_valid: bool,
}

impl Terminal {
    pub fn new(name: String, filename: Option<&str>) -> Terminal {
        let file = if let Some(filename) = filename {
            Some(std::fs::File::create(filename).unwrap())
        } else {
            None
        };
        Terminal {
            name,
            chr: '\0',
            file,
            out_valid: false,
        }
    }

    fn print(&mut self) {
        if self.out_valid {
            let s = self.chr;
            if let Some(file) = &mut self.file {
                write!(file, "{s}").unwrap();
                file.flush().unwrap();
            } else {
                print!("{s}");
                std::io::stdout().flush().unwrap();
            }
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
        self.print();
        vec![]
    }

    fn reset(&mut self, _path: Path) -> Vec<(PortName, Value)> {
        self.print();
        vec![]
    }
}
