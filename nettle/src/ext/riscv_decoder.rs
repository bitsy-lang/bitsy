use super::*;

/// Debug monitor for a RISC-V core.
/// On reset and on clock, decodes the 32-bit value presented to `in` as a RV32I instruction and
/// prints it to the screen.
#[derive(Debug)]
pub struct RiscVDecoder(Option<String>);

impl RiscVDecoder {
  pub fn new() -> RiscVDecoder {
      RiscVDecoder(None)
  }
}

impl ExtInstance for RiscVDecoder {
    fn incoming_ports(&self) -> Vec<PortName> { vec!["in".to_string()] }
    fn outgoing_ports(&self) -> Vec<PortName> { vec![] }

    fn update(&mut self, _port: &PortName, value: Value) -> Vec<(PortName, Value)> {
        if let Value::Word(32, v) = value {
            let instr = riscy::decode(v as u32);
            self.0 = Some(format!("{instr:?}"));
//            println!("RiscVDecoder: {instr:?}");
        } else if let Value::X = value {
            // do nothing
        } else {
            panic!("RiscVDecoder expected Word<32>: {value:?}")
        }
        vec![]
    }

    fn clock(&mut self) -> Vec<(PortName, Value)> {
        if let Some(s) = &self.0 {
            println!("{s}");
            self.0 = None;
        }
        vec![]
    }

    fn reset(&mut self) -> Vec<(PortName, Value)> {
        if let Some(s) = &self.0 {
            println!("{s}");
            self.0 = None;
        }
        vec![]
    }
}
