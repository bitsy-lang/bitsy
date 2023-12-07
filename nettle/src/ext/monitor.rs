use super::*;

#[derive(Debug, Clone)]
pub struct Monitor;

impl Monitor {
  pub fn new() -> Monitor {
      Monitor(None)
  }
}

impl Ext for Monitor {
    fn incoming_ports(&self) -> Vec<PortName> { vec!["in".to_string()] }
    fn outgoing_ports(&self) -> Vec<PortName> { vec![] }
}

impl Ext for Monitor {
    type State = Option<String>;

    fn incoming_ports(&self) -> Vec<PortName> { vec!["in".to_string()] }
    fn outgoing_ports(&self) -> Vec<PortName> { vec![] }

    fn instantiate(&self) -> ExtInstance<'_, Self::State> {
        None
    }

    fn update(&self, state: &mut Self::State, port: PortId, value: Value) -> Vec<(PortId, Value)> {
        *state = Some(format!("{value:?}"));
        vec![]
    }

    fn clock(&self, state: &mut Self::State) -> Vec<(PortId, Value)> {
        if let Some(s) = state {
            println!("Monitor: {s}");
            *state = None
        }
        vec![]
    }
    fn reset(&self, state: &mut Self::State) -> Vec<(PortId, Value)> { vec![] }
}
