use super::*;

#[derive(Debug)]
pub struct Video {
    signal: u8,
    hsync: bool,
    vsync: bool,
    frame_buffer: String,
    disabled: bool,
    initialized: bool,
}

impl Video {
    pub fn new() -> Video {
        Video {
            signal: 0,
            hsync: false,
            vsync: false,
            frame_buffer: String::new(),
            disabled: false,
            initialized: false,
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
            if !self.initialized {
                ctrlc::set_handler(move || {
                    // show the cursor
                    println!("\x1B[?25h");
                    std::process::exit(0);
                }).unwrap();
            }

            // hide the cursor
            print!("\x1B[?25l");

            // clear the screen
            print!("\x1B[2J");
        }

        self.initialized = true;

        vec![]
    }
}
