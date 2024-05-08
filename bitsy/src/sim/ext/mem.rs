use super::*;
use std::collections::BTreeMap;

/// A same-cycle random-access memory.
/// Has 64KiB of memory.
/// Addressed by a 32-bit address (only the bottom 16 bits are usable).
/// Reads and writes a 32-bit word at a time.
#[derive(Debug)]
pub struct Mem {
    name: String,
    instances: BTreeMap<Path, MemInstance>,
    initial_data: Vec<u8>,
    delay: usize,
}

impl Mem {
    pub fn new(name: String) -> Mem {
        Mem {
            name,
            instances: BTreeMap::new(),
            initial_data: vec![],
            delay: 1,
        }
    }

    pub fn set_read_delay(&mut self, delay: usize) {
        self.delay = delay;
    }

    pub fn load_from_file<P: AsRef<std::path::Path>>(&mut self, path: P) -> anyhow::Result<()> {
        self.initial_data = std::fs::read(&path)?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct MemInstance {
    mem: [u8; 1 << 16],
    read_addr: u32,
    write_enable: bool,
    write_addr: u32,
    write_data: u32,
    write_mask: u8,
    delay: usize,
    delay_current: usize,
}

impl MemInstance {
    pub fn new(initial_data: &[u8], delay: usize) -> MemInstance {
        let mut mem = [0; 1 << 16];

        let len = initial_data.len().min(1<<16);
        for i in 0..len {
            mem[i] = initial_data[i];
        }
        MemInstance {
            mem,
            read_addr: 0,
            write_enable: false,
            write_addr: 0,
            write_data: 0,
            write_mask: 0,
            delay,
            delay_current: 0,
        }
    }

    fn read(&self) -> Value {
        if self.read_addr >= 1<<16 {
            //eprintln!("Out of bounds read: {:0x}", self.read_addr);
            return Value::Word(32, 0);
        }
        let val: u64 =
            (self.mem[self.read_addr as usize] as u64) |
            ((self.mem[self.read_addr as usize + 1] as u64) << 8) |
            ((self.mem[self.read_addr as usize + 2] as u64) << 16) |
            ((self.mem[self.read_addr as usize + 3] as u64) << 24)
        ;
        Value::Word(32, val)
    }

    pub fn render(&self) -> String {
        let mem = if let Some(index) = self.mem.iter().position(|&x| x == 0) {
            &self.mem[..index+1]
        } else {
            &self.mem
        };
        format!("RAM: {:?}", String::from_utf8_lossy(mem))
    }

    fn update(&mut self, port: &PortName, value: Value) -> Vec<(PortName, Value)>  {
        if value.is_x() {
            return vec![("read_data".to_string(), self.read())];
        }
        if port == "read_addr" {
            if let Value::Word(32, addr) = value {
                self.read_addr = addr as u32;
                if self.delay_current == 0 {
                    return vec![("read_data".to_string(), self.read())];
                } else {
                    self.delay_current = 0;
                }
            } else {
                panic!("Mem must receive a Word[32] on read_addr. Received {value:?}")
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
                _ => panic!("write_addr value must be Word[32]: {value:?}"),
            }
        } else if port == "write_data" {
            match value {
                Value::Word(32, v) => self.write_data = v.try_into().unwrap(),
                _ => panic!("write_data value must be Word[32]: {value:?}"),
            }
        } else if port == "write_mask" {
            match value {
                Value::Word(4, v) => self.write_mask = v.try_into().unwrap(),
                _ => panic!("write_data value must be Word[32]: {value:?}"),
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
            eprintln!("{:08b}", self.write_mask);
            if self.write_mask & 1 > 0 {
                self.mem[self.write_addr as usize]       = self.write_data as u8 & 0xff;
            }
            if self.write_mask & 2 > 0 {
                self.mem[(self.write_addr + 1) as usize] = (self.write_data >>  8) as u8;
            }
            if self.write_mask & 4 > 0 {
                self.mem[(self.write_addr + 2) as usize] = (self.write_data >> 16) as u8;
            }
            if self.write_mask & 8 > 0 {
                self.mem[(self.write_addr + 3) as usize] = (self.write_data >> 24) as u8;
            }
            //println!("Mem wrote {read_data:?} at address 0x{:x}", self.write_addr);
        }

        if self.delay_current < self.delay {
            self.delay_current += 1;
            vec![]
        } else {
            let read_data = self.read();
            self.delay_current = 0;
            vec![("read_data".to_string(), read_data)]
        }
    }
}

impl Ext for Mem {
    fn name(&self) -> String {
        self.name.clone()
    }
    fn instantiate(&mut self, path: Path) {
        self.instances.insert(path, MemInstance::new(&self.initial_data, self.delay));
    }

    fn incoming_ports(&self) -> Vec<PortName> {
        vec![
            "read_addr".to_string(),
            "write_enable".to_string(),
            "write_addr".to_string(),
            "write_data".to_string(),
            "write_mask".to_string(),
        ]
    }
    fn outgoing_ports(&self) -> Vec<PortName> { vec!["read_data".to_string()] }

    fn update(&mut self, path: Path, port: &PortName, value: Value) -> Vec<(PortName, Value)>  {
        self.instances.get_mut(&path).unwrap().update(port, value)
    }
    fn clock(&mut self, path: Path) -> Vec<(PortName, Value)> {
        self.instances.get_mut(&path).unwrap().clock()
    }
    fn reset(&mut self, path: Path) -> Vec<(PortName, Value)> {
        self.instances.get_mut(&path).unwrap().reset()
    }
}
