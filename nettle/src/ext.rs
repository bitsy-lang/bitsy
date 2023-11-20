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
    mem: [u8; 1 << 16],
    read_addr: u16,
    write_enable: bool,
    write_addr: u16,
    write_data: u8,
}

impl Ram {
    pub fn new() -> Ram {
        let mut mem = [0; 1 << 16];
        for (i, ch) in "Hello, World!\0".bytes().enumerate() {
            mem[i] = ch;
        }
        Ram {
            mem,
            read_addr: 0,
            write_enable: false,
            write_addr: 0,
            write_data: 0,
        }
    }
}

impl ExtInstance for Ram {
    fn peek(&mut self, _port: &str) -> Value {
        let value = self.mem[self.read_addr as usize];
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
        } else if port == "write_enable" {
            match value {
                Value::Word(1, 0) => self.write_enable = false,
                Value::Word(1, 1) => self.write_enable = true,
                _ => panic!(),
            }
        } else if port == "write_addr" {
            match value {
                Value::Word(16, v) => self.write_addr = v.try_into().unwrap(),
                _ => panic!(),
            }
        } else if port == "write_data" {
            match value {
                Value::Word(8, v) => self.write_data = v.try_into().unwrap(),
                _ => panic!(),
            }
        } else {
            panic!("Ram may only recieve data on read_data: received data on {port} {value:?}")
        }
        vec![]
    }

    fn reset(&mut self) {
        let mem = if let Some(index) = self.mem.iter().position(|&x| x == 0) {
            &self.mem[..index+1]
        } else {
            &self.mem
        };

        println!("RAM: {:?}", String::from_utf8_lossy(mem));
    }

    fn clock(&mut self) {
        if self.write_enable {
            self.mem[self.write_addr as usize] =  self.write_data;

            let mem = if let Some(index) = self.mem.iter().position(|&x| x == 0) {
                &self.mem[..index+1]
            } else {
                &self.mem
            };
            println!("RAM: {:?}", String::from_utf8_lossy(mem));
        }
    }
}
