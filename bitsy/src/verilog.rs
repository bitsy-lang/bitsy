use std::collections::HashMap;

pub struct Verilog {
    pub filename: String,
    pub modules: Vec<Module>,
}

pub struct Module {
    pub name: String,
    pub ports: Vec<Port>,
    pub regs: Vec<Reg>,
    pub insts: Vec<Inst>,
    pub alwayses: Vec<Always>,
}

pub enum Direction {
    Input,
    Output,
}

pub struct Port {
    pub name: String,
    pub width: Option<u64>,
    pub direction: Direction,
}

pub struct Reg {
    pub name: String,
    pub width: Option<u64>,
}

pub struct Inst {
    pub module_name: String,
    pub instance_name: String,
    pub connections: HashMap<String, String>,
}

pub struct Always {
    pub sensitivity_list: Vec<Sensitivity>,
}

pub enum Edge {
    PosEdge,
    NegEdge,
    On,
}

pub struct Sensitivity(pub Edge, pub String);

impl std::fmt::Display for Always {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "    always @(")?;
        for (i, sensitivity) in self.sensitivity_list.iter().enumerate() {
            write!(f, "{sensitivity}")?;
            if i + 1 < self.sensitivity_list.len() {
                write!(f, ", ")?;
            }
        }
        writeln!(f, ") begin")?;
        writeln!(f, "    end")?;
        Ok(())
    }
}

impl std::fmt::Display for Sensitivity {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let edge = match self.0 {
            Edge::PosEdge => "posedge ",
            Edge::NegEdge => "posedge ",
            Edge::On => "",
        };
        write!(f, "{edge}{}", &self.1)?;
        Ok(())
    }
}

impl std::fmt::Display for Inst {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "    {} {}(", self.module_name, self.instance_name)?;
        for (i, (port, terminal)) in self.connections.iter().enumerate() {
            if i + 1 == self.connections.len() {
                writeln!(f, "        .{port}({terminal})")?;
            } else {
                writeln!(f, "        .{port}({terminal}),")?;
            }
        }
        writeln!(f, "    );")?;
        Ok(())
    }
}

impl std::fmt::Display for Direction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Direction::Input => write!(f, "input"),
            Direction::Output => write!(f, "output"),
        }
    }
}

impl std::fmt::Display for Port {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let width_str = match self.width {
            Some(width) => format!("[{}:0]", width - 1),
            None => "".to_string(),
        };

        write!(f, "    {} wire {width_str:>8} {}", self.direction, self.name)
    }
}

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let width_str = match self.width {
            Some(width) => format!("[{}:0]", width - 1),
            None => "".to_string(),
        };

        write!(f, "    reg {width_str:>8} {};", self.name)
    }
}

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "module {}(", self.name);
        for (i, port) in self.ports.iter().enumerate() {
            write!(f, "{port}");
            if i + 1 < self.ports.len() {
                writeln!(f, ",");
            } else {
                writeln!(f, );
            }
        }

        writeln!(f, ");");

        for reg in &self.regs {
            writeln!(f, "{reg}");
        }

        for inst in &self.insts {
            writeln!(f, "{inst}");
        }

        for always in &self.alwayses {
            writeln!(f, "{always}");
        }

        writeln!(f, "endmodule")
    }
}

impl std::fmt::Display for Verilog {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for module in &self.modules {
            write!(f, "{module}");
        }
        Ok(())
    }
}
