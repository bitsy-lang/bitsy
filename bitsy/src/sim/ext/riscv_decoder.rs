use super::*;

/// Debug monitor for a RISC-V core.
/// On reset and on clock, decodes the 32-bit value presented to `in` as a RV32I instruction and
/// prints it to the screen.
#[derive(Debug)]
pub struct RiscvDecoder(String, Option<String>);

impl RiscvDecoder {
  pub fn new(name: String) -> RiscvDecoder {
      RiscvDecoder(name, None)
  }
}

impl Ext for RiscvDecoder {
    fn name(&self) -> String { "RiscvDecoder".to_string() }
    fn instantiate(&mut self, _path: Path) {}
    fn incoming_ports(&self) -> Vec<PortName> { vec!["in".to_string()] }
    fn outgoing_ports(&self) -> Vec<PortName> { vec![] }

    fn update(&mut self, _path: Path, _port: &PortName, value: Value) -> Vec<(PortName, Value)> {
        if let Value::Word(32, v) = value {
            let instr = riscy::decode(v as u32);
            self.1 = Some(format!("{instr:?}"));
//            println!("RiscVDecoder: {instr:?}");
        } else if let Value::X = value {
            // do nothing
        } else {
            panic!("RiscVDecoder expected Word<32>: {value:?}")
        }
        vec![]
    }

    fn clock(&mut self, _path: Path) -> Vec<(PortName, Value)> {
        if let Some(s) = &self.1 {
            println!("{s}");
            self.1 = None;
        }
        vec![]
    }

    fn reset(&mut self, _path: Path) -> Vec<(PortName, Value)> {
        if let Some(s) = &self.1 {
            println!("{s}");
            self.1 = None;
        }
        vec![]
    }
}
