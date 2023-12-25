use super::value::Value;
use super::Path;

pub mod monitor;
pub mod ram;
pub mod mem;
pub mod video;
pub mod riscv_decoder;
pub mod terminal;

pub type PortId = usize;
pub type PortName = String;

pub trait Ext: std::fmt::Debug {
    fn name(&self) -> String;
    fn incoming_ports(&self) -> Vec<PortName> { vec![] }
    fn outgoing_ports(&self) -> Vec<PortName> { vec![] }
    fn instantiate(&mut self, path: Path);
    fn update(&mut self, path: Path, port: &PortName, value: Value) -> Vec<(PortName, Value)>;
    fn clock(&mut self, _path: Path) -> Vec<(PortName, Value)> { vec![] }
    fn reset(&mut self, _path: Path) -> Vec<(PortName, Value)> { vec![] }
}
