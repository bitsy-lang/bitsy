use super::*;

#[allow(unused_variables)]
pub trait ExtInstance: std::fmt::Debug {
    fn peek(&mut self, port: &str) -> Value { panic!(); }
    fn poke(&mut self, port: &str, value: Value) -> Vec<String> { panic!(); }
    fn clock(&mut self) {}
    fn reset(&mut self) {}
}

#[derive(Debug)]
pub struct Monitor(Option<String>);

impl Monitor {
  pub fn new() -> Monitor {
      Monitor(None)
  }
}

impl ExtInstance for Monitor {
    fn poke(&mut self, _port: &str, value: Value) -> Vec<String> {
        self.0 = Some(format!("{value:?}"));
        vec![]
    }

    fn clock(&mut self) {
        if let Some(s) = &self.0 {
            println!("Monitor: {s}");
            self.0 = None
        }
    }
}

#[derive(Debug)]
pub struct Ram {
    mem: BTreeMap<u16, u8>,
    read_addr: u16,
}

impl Ram {
    pub fn new() -> Ram {
        let mut mem = BTreeMap::new();
        for (i, ch) in "Hello, World!\0".bytes().enumerate() {
            mem.insert(i as u16, ch);
        }
        Ram {
            mem,
            read_addr: 0,
        }
    }
}

impl ExtInstance for Ram {
    fn peek(&mut self, _port: &str) -> Value {
        let value = self.mem[&self.read_addr];
        Value::Word(16, value.into())
    }

    fn poke(&mut self, port: &str, value: Value) -> Vec<String> {
        if port == "read_addr" {
            if let Value::Word(_width, addr) = value {
                self.read_addr = addr as u16;
                return vec!["read_data".to_string()];
            } else {
                panic!("Ram must receive a Word<16> on read_addr")
            }
        } else {
            panic!("Ram may only recieve data on read_data: received data on {port} {value:?}")
        }
    }

    fn clock(&mut self) {
    }
}
