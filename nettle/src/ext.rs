use super::*;

pub type PortId = usize;
pub type PortName = String;

#[allow(unused_variables)]
pub trait ExtInstance: std::fmt::Debug {
    fn incoming_ports(&self) -> Vec<PortName> { vec![] }
    fn outgoing_ports(&self) -> Vec<PortName> { vec![] }
    fn update(&mut self, port: &PortName, value: Value) -> Vec<(PortName, Value)>;
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
    fn incoming_ports(&self) -> Vec<PortName> { vec!["in".to_string()] }
    fn outgoing_ports(&self) -> Vec<PortName> { vec![] }

    fn update(&mut self, _port: &PortName, value: Value) -> Vec<(PortName, Value)> {
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
            if let Value::Word(_width, addr) = value {
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
            self.mem[self.write_addr as usize] =  self.write_data;
            let read_data = Value::Word(8, self.mem[self.read_addr as usize].into());
            //println!("Ram wrote {read_data:?} at address 0x{:x}", self.write_addr);
            vec![("read_data".to_string(), read_data)]
        } else {
            vec![]
        }
    }
}

impl Ram {
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

#[derive(Debug)]
pub struct Video {
    signal: u8,
    hsync: bool,
    vsync: bool,
    frame_buffer: String,
    disabled: bool,
}

impl Video {
    pub fn new() -> Video {
        Video {
            signal: 0,
            hsync: false,
            vsync: false,
            frame_buffer: String::new(),
            disabled: false,
        }
    }

    pub fn disable(&mut self) {
        self.disabled = true;
    }
}

impl ExtInstance for Video {
    fn incoming_ports(&self) -> Vec<PortName> { vec!["signal".to_string(), "hsync".to_string(), "vsync".to_string()] }

    fn update(&mut self, portname: &PortName, value: value::Value) -> Vec<(PortName, value::Value)> {
        if value.is_x() {
            return vec![];
        }
        if portname == "signal" {
            self.signal = value.try_into().unwrap();
        } else if portname == "hsync" {
            self.hsync = value.try_into().unwrap();
        } else if portname == "vsync" {
            self.vsync = value.try_into().unwrap();
        }
        vec![]
    }

    fn clock(&mut self) -> Vec<(PortName, Value)> {
        if !self.disabled {
            use std::fmt::Write;
            let c: char = " ░▒▓".chars().collect::<Vec<char>>()[self.signal as usize];

            write!(self.frame_buffer, "{c}{c}").unwrap();
            if self.hsync {
                writeln!(self.frame_buffer).unwrap();
            }

            if self.vsync {
                std::thread::sleep(std::time::Duration::from_millis(30));

                // clear screen
                print!("\x1B[2J");

                // move cursor to the upper left corner
                print!("\x1B[H");

                println!("{}", self.frame_buffer);
                self.frame_buffer.clear();
            }
        }
        vec![]
    }

    fn reset(&mut self) -> Vec<(PortName, Value)>{
        if !self.disabled {
            ctrlc::set_handler(move || {
                // show the cursor
                println!("\x1B[?25h");
                std::process::exit(0);
            }).unwrap();

            // hide the cursor
            print!("\x1B[?25l");

            // clear the screen
            print!("\x1B[2J");
        }

        vec![]
    }
}
