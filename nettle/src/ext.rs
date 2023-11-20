use super::*;

pub type PortName = String;

#[allow(unused_variables)]
pub trait ExtInstance: std::fmt::Debug {
    fn poke(&mut self, port: PortName, value: Value) -> Vec<(PortName, Value)>;
    fn clock(&mut self) -> Vec<(PortName, Value)> { vec![] }
    fn reset(&mut self) -> Vec<(PortName, Value)> { vec![] }
}

#[derive(Debug)]
pub struct Monitor(Option<String>);

impl Monitor {
  pub fn new() -> Monitor {
      Monitor(None)
  }
}

impl ExtInstance for Monitor {
    fn poke(&mut self, _port: PortName, value: Value) -> Vec<(PortName, Value)> {
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
        let mem = [0; 1 << 16];
        Ram {
            mem,
            read_addr: 0,
            write_enable: false,
            write_addr: 0,
            write_data: 0,
        }
    }

    fn read(&self) -> Value {
        Value::Word(16,self.mem[self.read_addr as usize] as u64)
    }
}

impl ExtInstance for Ram {
    fn poke(&mut self, port: PortName, value: Value) -> Vec<(PortName, Value)>  {
        if port == "read_addr" {
            if let Value::Word(_width, addr) = value {
                self.read_addr = addr as u16;
                return vec![("read_data".to_string(), self.read())];
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
                _ => panic!("write_data value must be Word<8>: {value:?}"),
            }
        } else {
            panic!("Ram may only recieve data on read_data: received data on {port} {value:?}")
        }
        vec![]
    }

    fn reset(&mut self) -> Vec<(PortName, Value)> {
        for (i, ch) in "Hello, World!\0".bytes().enumerate() {
            self.mem[i] = ch;
        }

        println!("{}", self.render());
        vec![("read_data".to_string(), self.read())]
    }

    fn clock(&mut self) -> Vec<(PortName, Value)> {
        println!("LOL CLOCK {}", self.render());
        if self.write_enable {
            println!("WE");
            self.mem[self.write_addr as usize] =  self.write_data;
            vec![("read_data".to_string(), Value::X)]
        } else {
            println!("NO WE");
            vec![]
        }
    }
}

impl Ram {
    fn render(&self) -> String {
        let mem = if let Some(index) = self.mem.iter().position(|&x| x == 0) {
            &self.mem[..index+1]
        } else {
            &self.mem
        };
        format!("RAM: {:?}", String::from_utf8_lossy(mem))
    }
}
