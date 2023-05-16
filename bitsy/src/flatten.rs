use crate::*;
use crate::ast;
use crate::common::*;
use crate::verilog;

impl Module {
    pub fn to_verilog(&self) -> verilog::Module {
        let name = self.name().to_string();
        let ports: Vec<verilog::Port> = self.verilog_ports();
        let regs: Vec<verilog::Reg> = vec![];//todo!();
        let insts: Vec<verilog::Inst> = vec![];//todo!();
        let alwayses: Vec<verilog::Always> = vec![];//todo!();

        verilog::Module {
            name,     // String
            ports,    // Vec<verilog::Port>
            regs,     // Vec<verilog::Reg>
            insts,    // Vec<verilog::Inst>
            alwayses, // Vec<verilog::Always>
        }
    }

    fn verilog_ports(&self) -> Vec<verilog::Port> {
        let mut result = vec![];
        for port in self.ports() {
            for pin in port.pins() {
                let direction = if pin.direction() == Direction::Incoming {
                    verilog::Direction::Input
                } else {
                    verilog::Direction::Output
                };

                let width = pin.shape().width();

                result.push(verilog::Port {
                    name: format!("{}__{}", port.name().to_string(), pin.name()),
                    width,
                    direction,
                });
            }
        }
        dbg!(result)
    }
}
