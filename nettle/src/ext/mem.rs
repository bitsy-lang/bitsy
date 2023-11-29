use super::*;

#[derive(Debug)]
pub struct Mem {
    mem: [u8; 1 << 16],
    read_addr: u32,
    write_enable: bool,
    write_addr: u32,
    write_data: u32
}

impl Mem {
    pub fn new() -> Mem {
        let mem = [0; 1 << 16];
        Mem {
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

    fn read_at(&self, addr: u32) -> Value {
        let val: u64 =
            (self.mem[addr as usize] as u64) |
            ((self.mem[addr as usize + 1] as u64) << 8) |
            ((self.mem[addr as usize + 2] as u64) << 16) |
            ((self.mem[addr as usize + 3] as u64) << 24)
        ;
        Value::Word(32, val)
    }

    fn read(&self) -> Value {
        self.read_at(self.read_addr)
    }

    fn render(&self) -> String {
        //dbg!(self);
        let mem = if let Some(index) = self.mem.iter().position(|&x| x == 0) {
            &self.mem[..index+1]
        } else {
            &self.mem
        };
        format!("RAM: {:?}", String::from_utf8_lossy(mem))
    }
}

impl ExtInstance for Mem {
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
            if let Value::Word(32, addr) = value {
                self.read_addr = addr as u32;
                return vec![("read_data".to_string(), self.read())];
            } else {
                panic!("Mem must receive a Word<32> on read_addr. Received {value:?}")
            }
        } else if port == "write_enable" {
            match value {
                Value::Word(1, 0) => self.write_enable = false,
                Value::Word(1, 1) => self.write_enable = true,
                _ => panic!(),
            }
        } else if port == "write_addr" {
            match value {
                Value::Word(32, v) => self.write_addr = v.try_into().unwrap(),
                _ => panic!("write_addr value must be Word<32>: {value:?}"),
            }
        } else if port == "write_data" {
            match value {
                Value::Word(32, v) => self.write_data = v.try_into().unwrap(),
                _ => panic!("write_data value must be Word<32>: {value:?}"),
            }
        } else {
            panic!("Mem may only recieve data on read_data: received data on {port} {value:?}")
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
//        println!("Mem was clocked: {}", self.render());
        if self.write_enable {
            println!("Writing to RAM: 0x{:08x} <= {:08x}", self.write_addr, self.write_data);
            self.mem[self.write_addr as usize]       = self.write_data as u8 & 0xff;
            self.mem[(self.write_addr + 1) as usize] = (self.write_data >>  8) as u8;
            self.mem[(self.write_addr + 2) as usize] = (self.write_data >> 16) as u8;
            self.mem[(self.write_addr + 3) as usize] = (self.write_data >> 24) as u8;
        }

        //println!("Mem wrote {read_data:?} at address 0x{:x}", self.write_addr);
        //{
            //use std::io::Write;
            //let mut file = std::fs::File::create("memory.bin").unwrap();
            //file.write_all(&self.mem).unwrap();
        //}

        // clear screen
        print!("\x1B[2J");

        // move cursor to the upper left corner
        print!("\x1B[H");
        let mut addr: u32 = 0x0;
        for _row in 0..32 {
            print!("{addr:08x}        ");
            for _col in 0..8 {
                let data = self.mem[addr as usize];
                addr += 1;
                print!("{data:02x} ");
            }
            print!(" ");
            for _col in 0..8 {
                let data = self.mem[addr as usize];
                addr += 1;
                print!("{data:02x} ");
            }
            println!();
        }

        if self.write_enable {
            let read_data = self.read();
            vec![("read_data".to_string(), read_data)]
        } else {
            vec![]
        }
    }
}

