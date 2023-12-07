use crate::value::Value;
use std::sync::Arc;

//pub mod monitor;
//pub mod ram;
//pub mod mem;
//pub mod video;
//pub mod riscv_decoder;
//pub mod terminal;

pub type PortId = usize;
pub type PortName = String;

pub trait Ext: std::fmt::Debug + Clone {
    type Instance: ExtInstance;

    fn incoming_ports(&self) -> Vec<PortName> { vec![] }
    fn outgoing_ports(&self) -> Vec<PortName> { vec![] }

    fn update(&self, instance: &mut Self::Instance, port: PortId, value: Value) -> Vec<(PortId, Value)>;
    fn clock(&self, instance: &mut Self::Instance) -> Vec<(PortId, Value)> { vec![] }
    fn reset(&self, instance: &mut Self::Instance) -> Vec<(PortId, Value)> { vec![] }

    fn instantiate<T: ExtInstance<Definition=Self>>(self: Arc<Self>) -> T;
}

pub trait ExtInstance {
    type Definition: Ext;
}

/*

    fn create(

    fn definition(&self) -> Arc<Self::Definition>;
    fn state(&self) -> &Self::Definition::State;
    fn state_mut(&mut self) -> &mut Self::Definition::State;

    fn update(&mut self, port: PortId, value: Value) -> Vec<(PortId, Value)> {
        self.ext.update(&mut self.state, port, value)
    }

    fn clock(&mut self) -> Vec<(PortId, Value)> {
        self.ext.clock(&mut self.state)
    }
    fn reset(&mut self) -> Vec<(PortId, Value)> {
        self.ext.reset(&mut self.state)
    }
*/
