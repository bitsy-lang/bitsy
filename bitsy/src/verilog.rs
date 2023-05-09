pub struct Verilog {
    pub filename: String,
    pub modules: Vec<Module>,
}

pub struct Module {
    pub name: String,
    pub ports: Vec<Port>,
}

pub enum Direction {
    Input,
    Output,
}

impl std::fmt::Display for Direction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Direction::Input => write!(f, "input"),
            Direction::Output => write!(f, "output"),
        }
    }
}

pub struct Port {
    pub name: String,
    pub width: Option<u64>,
    pub direction: Direction,
}

impl std::fmt::Display for Port {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let width_str = match self.width {
            Some(width) => format!("[{:3}:0]", width - 1),
            None => "       ".to_string(),
        };

        write!(f, "{} wire {width_str} {}", self.direction, self.name)
    }
}

impl Verilog {
    pub fn dump(&self) {
        for module in &self.modules {
            module.dump();
        }
    }
}

impl Module {
    pub fn dump(&self) {
        println!("module {}(", self.name);
        for (i, port) in self.ports.iter().enumerate() {
            print!("    {port}");
            if i + 1 < self.ports.len() {
                println!(",");
            } else {
                println!();
            }
        }

        println!(");");

        println!("endmodule");
    }
}
