use super::*;
use crate::sim::value::Value;

pub mod monitor;
pub mod ram;
pub mod mem;
pub mod video;
pub mod riscv_decoder;
pub mod terminal;

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
