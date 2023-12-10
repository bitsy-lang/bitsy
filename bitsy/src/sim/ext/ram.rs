use super::*;

/// A same-cycle random-access memory.
/// Has 64KiB of memory.
/// Addressed by a 16-bit address.
/// Reads and writes an 8-bit word at a time.
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

    pub fn load_from_file<P: AsRef<std::path::Path>>(&mut self, path: P) -> anyhow::Result<()> {
        let data = std::fs::read(&path)?;
        let len = data.len().min(1<<16);
        for i in 0..len {
            self.mem[i] = data[i];
        }
        Ok(())
    }

    fn read(&self) -> Value {
        Value::Word(8, self.mem[self.read_addr as usize] as u64)
    }

    pub fn render(&self) -> String {
        //dbg!(self);
        let mem = if let Some(index) = self.mem.iter().position(|&x| x == 0) {
            &self.mem[..index+1]
        } else {
            &self.mem
        };
        format!("RAM: {:?}", String::from_utf8_lossy(mem))
    }
}

impl ExtInstance for Ram {
    fn incoming_ports(&self) -> Vec<PortName> {
        vec![
            "read_addr".to_string(),
            "write_enable".to_string(),
            "write_addr".to_string(),
            "write_data".to_string(),
        ]
    }
    fn outgoing_ports(&self) -> Vec<PortName> { vec!["read_data".to_string()] }

    fn update(&mut self, port: &PortName, value: Value) -> Vec<(PortName, Value)>  {
        if value.is_x() {
            return vec![("read_data".to_string(), self.read())];
        }
        if port == "read_addr" {
            if let Value::Word(16, addr) = value {
                self.read_addr = addr as u16;
                return vec![("read_data".to_string(), self.read())];
            } else {
                panic!("Ram must receive a Word<16> on read_addr. Received {value:?}")
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
        /*
        for (i, ch) in "Hello, World!\0".bytes().enumerate() {
            self.mem[i] = ch;
        }

        vec![("read_data".to_string(), self.read())]
        */
        vec![]
    }

    fn clock(&mut self) -> Vec<(PortName, Value)> {
//        println!("Ram was clocked: {}", self.render());
        if self.write_enable {
            println!("Writing to RAM: 0x{:04x} <= {:02x}", self.write_addr, self.write_data);
            self.mem[self.write_addr as usize] = self.write_data;
            let read_data = Value::Word(8, self.mem[self.read_addr as usize].into());
            //println!("Ram wrote {read_data:?} at address 0x{:x}", self.write_addr);
            vec![("read_data".to_string(), read_data)]
        } else {
            vec![]
        }
    }
}
