use std::sync::Arc;

#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub parser);

pub mod ast {
    pub type ComponentName = String;
    pub type PortName = String;

    #[derive(Debug)]
    pub struct Circuit {
        pub mod_defs: Vec<ModDef>,
    }

    impl Circuit {
        pub fn mod_def(&self, name: &str) -> &ModDef {
            for mod_def in &self.mod_defs {
                if mod_def.name == name {
                    return mod_def;
                }
            }

            panic!("No such module found: {name}")
        }
    }

    #[derive(Debug)]
    pub struct ModDef {
        pub name: String,
        pub visibility: Visibility,
        pub ports: Vec<Port>,
        pub components: Vec<Component>,
        pub wires: Vec<Wire>,
    }

    #[derive(Debug, Clone)]
    pub struct Terminal(pub ComponentName, pub PortName);

    impl Terminal {
        pub fn component(&self) -> &ComponentName {
            &self.0
        }

        pub fn port(&self) -> &PortName {
            &self.1
        }
    }

    #[derive(Debug)]
    pub struct Wire(pub Visibility, pub Terminal, pub Terminal);

    impl Wire {
        pub fn visibility(&self) -> Visibility {
            self.0
        }

        pub fn sink(&self) -> &Terminal {
            &self.1
        }

        pub fn source(&self) -> &Terminal {
            &self.2
        }

    }

    #[derive(Debug, Clone, Copy, Eq, PartialEq)]
    pub enum Direction {
        Incoming,
        Outgoing,
    }

    #[derive(Debug, Clone, Copy)]
    pub enum Visibility {
        Public,
        Private,
    }

    #[derive(Debug)]
    pub enum Component {
        Reg(ComponentName, Visibility, RegComponent),
        Mod(ComponentName, Visibility, ModComponent),
    }

    impl Component {
        pub fn name(&self) -> &str {
            match self {
                Component::Reg(name, _, _) => name,
                Component::Mod(name, _, _) => name,
            }
        }
    }

    #[derive(Debug)]
    pub struct RegComponent {
        pub shape: Shape,
        pub domain: Domain,
        pub init: Value,
    }

    #[derive(Debug)]
    pub struct ModComponent {
        pub moddef_name: String,
    }

    #[derive(Debug, Clone, Copy, Eq, PartialEq)]
    pub enum Value {
        Unknown,
        Unobservable,
        Bool(bool),
        Word(u64),
    }

    impl std::fmt::Display for Value {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Value::Unknown => write!(f, "?")?,
                Value::Unobservable => write!(f, "X")?,
                Value::Bool(b) => write!(f,"{b}")?,
                Value::Word(n) => write!(f, "{n}")?,
            }
            Ok(())
        }
    }

    #[derive(Debug)]
    pub struct Port(pub String, pub Direction, pub Shape, pub Domain);

    impl Port {
        pub fn name(&self) -> &str {
            &self.0
        }

        pub fn direction(&self) -> Direction {
            self.1
        }

        pub fn shape(&self) -> &Shape {
            &self.2
        }

        pub fn domain(&self) -> &Domain {
            &self.3
        }
    }

    #[derive(Debug, Clone)]
    pub struct Shape(pub String, pub Vec<ShapeParam>);

    #[derive(Debug, Clone)]
    pub struct Domain;

    impl Domain {
        pub fn name(&self) -> &str {
            &"d"
        }
    }

    #[derive(Debug, Clone)]
    pub enum ShapeParam {
        Nat(u64),
        Shape(Box<Shape>),
    }
}

pub mod sim {
    use std::collections::BTreeMap;
    use std::sync::Arc;
    use super::ast::*;

    #[derive(Clone, Copy, PartialEq, Eq, Debug)]
    pub struct Signal(usize);

    impl Signal {
        fn id(&self) -> usize {
            self.0
        }
    }

    #[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
    pub struct Domain(usize);

    impl Domain {
        fn id(&self) -> usize {
            self.0
        }
    }

    pub struct Simulator {
        circuit: Arc<Circuit>,
        top: String,
        domains: Vec<DomainState>,
        signals: Vec<SignalState>,
    }

    struct DomainState {
        id: Domain,
        name: String,
        cycle: usize,
        reseting: bool,
    }

    #[derive(Debug, Clone, Copy)]
    enum Dep {
        Disconnected,
        Query(Signal),
        Reg(Signal),
    }

    struct SignalState {
        id: Signal,
        path: String,
        domain_id: Domain,
        dep: Dep,
        values: [Value; 2],
    }

    impl Simulator {
        pub fn new(circuit: Arc<Circuit>, top: &str) -> Simulator {
            let mut simulator = Simulator {
                circuit: circuit.clone(),
                top: top.to_string(),
                domains: vec![],
                signals: vec![],
            };

            simulator.add_domains(top);
            simulator.add_signals(top, vec!["top".to_string()]);

            simulator
        }

        fn add_domains(&mut self, module: &str) {
            self.domains.push(DomainState {
                id: Domain(0),
                name: "d".to_string(),
                cycle: 0,
                reseting: true,
            });
        }

        fn add_signals(&mut self, module: &str, mut component_path: Vec<String>) {
            let circuit = self.circuit.clone();
            let mod_def = circuit.mod_def(module).clone();

            let mut local_signals: BTreeMap<String, Signal> = BTreeMap::new();

            for Port(name, _dir, _shape, domain) in &mod_def.ports {
                let id = Signal(self.signals.len());
                self.signals.push(SignalState {
                    id,
                    domain_id: Domain(0),
                    path: format!("{}.{}", component_path.join("."), name),
                    dep: Dep::Disconnected, // to be set shortly
                    values: [Value::Unknown, Value::Unknown],
                });
                local_signals.insert(format!("io.{name}"), id);
            }

            for component in &mod_def.components {
                component_path.push(component.name().to_string());
                match component {
                    Component::Mod(_name, _visibility, module) => {
                        self.add_signals(&module.moddef_name, component_path.clone());
                    },
                    Component::Reg(_name, _visibility, reg) => {
                        let set_id = Signal(self.signals.len());
                        self.signals.push(SignalState {
                            id: set_id,
                            domain_id: Domain(0),
                            path: format!("{}.set", component_path.join(".")),
                            dep: Dep::Disconnected,
                            values: [Value::Unknown, reg.init],
                        });

                        let val_id = Signal(self.signals.len());
                        self.signals.push(SignalState {
                            id: val_id,
                            domain_id: Domain(0),
                            path: format!("{}.val", component_path.join(".")),
                            dep: Dep::Reg(set_id),
                            values: [Value::Unknown, Value::Unknown],
                        });
                    }
                }
                component_path.pop();
            }

            let terminal_to_signal_path = |terminal: &Terminal| -> String {
                if terminal.component() == "io" {
                    format!("{}.{}", component_path.join("."), terminal.port())
                } else {
                    format!("{}.{}.{}", component_path.join("."), terminal.component(), terminal.port())
                }
            };

            for Wire(_visibility, sink, source) in &mod_def.wires {
                let sink_path = terminal_to_signal_path(sink);
                let source_path = terminal_to_signal_path(source);

                let sink_signal = self.signal_by_path(&sink_path).unwrap();
                let source_signal = self.signal_by_path(&source_path).unwrap();

                let sink_signal_state = &mut self.signals[sink_signal.id()];
                sink_signal_state.dep = Dep::Query(source_signal);
            }
        }

        pub fn step(
            &mut self,
            domain: Domain,
            pokes: Vec<(Signal, Value)>,
        ) {
            for signal in self.signals_in_domain(domain).into_iter() {
                let mut signal_state = &mut self.signals[signal.id()];
                signal_state.values[0] = signal_state.values[1];
                signal_state.values[1] = Value::Unknown;
            }

            for (signal, value) in pokes.into_iter() {
                self.poke(signal, value);
            }

            self.dump();

            for signal in self.top_output_signals().into_iter() {
                self.query(signal, 0);
            }
        }

        fn top_output_signals(&self) -> Vec<Signal> {
            let mut result = vec![];
            let circuit = self.circuit.clone();
            let mod_def = circuit.mod_def(&self.top).clone();
            for Port(name, dir, _shape, _domain) in &mod_def.ports {
                if *dir == Direction::Outgoing {
                    let signal = self.signal_by_path(&format!("top.{name}")).unwrap();
                    result.push(signal);
                }
            }
            result
        }

        fn dump(&self) {
            for signal_state in &self.signals {
                println!("{:<30} {:>8} {:>8}", signal_state.path, signal_state.values[0].to_string(), signal_state.values[1].to_string());
            }
        }

        fn query(&mut self, signal: Signal, depth: usize) -> Value {
            let spaces = &vec![b' '; 4 * depth];
            let spaces = String::from_utf8_lossy(spaces);
            let signal_state = &self.signals[signal.id()];
            println!("{spaces}Querying {}", signal_state.path);

            let current_value = self.peek(signal);
            if current_value != Value::Unknown {
                println!("{spaces}    Value has already been computed: {current_value}");
                return current_value;
            }

            match signal_state.dep {
                Dep::Query(depend_signal) => {
                    println!("{spaces}    {} depends on {}", self.signal_path(signal), self.signal_path(depend_signal));
                    let val = self.query(depend_signal, depth + 1);
                    println!("{spaces}    returning {val}");
                    assert!(val != Value::Unknown, "Query failed");
                    self.poke(signal, val);
                    return val;
                }
                Dep::Reg(set_signal) => {
                    let set_signal_state = &self.signals[set_signal.id()];
                    let val = set_signal_state.values[0];
                    println!("{spaces}    The previous cycle of the register {} gives {val}", set_signal_state.path);
                    assert!(val != Value::Unknown, "Query failed");
                    self.poke(signal, val);

                    println!("{spaces}    Querying set pin");
                    self.query(set_signal, depth + 1);

                    println!("{spaces}    returning {val}");
                    return val;
                },
                Dep::Disconnected => panic!("Disconnected: Signal {} has no driver", signal_state.path)
            }
        }

        pub fn signals(&self) -> Vec<Signal> {
            let mut result = vec![];
            for i in 0..self.signals.len() {
                result.push(Signal(i));
            }
            result
        }

        pub fn signal_path(&self, signal: Signal) -> &str {
            let signal_state = &self.signals[signal.id()];
            &signal_state.path
        }

        pub fn signal_by_path(&self, path: &str) -> Option<Signal> {
            for signal in &self.signals {
                if signal.path == path {
                    return Some(signal.id)
                }
            }
            None
        }

        pub fn signals_in_domain(&self, domain: Domain) -> Vec<Signal> {
            let mut result = vec![];
            for signal_state in &self.signals {
                if signal_state.domain_id == domain {
                    result.push(signal_state.id);
                }
            }
            result
        }

        pub fn domains(&self) -> Vec<Domain> {
            let mut result = vec![];
            for i in 0..self.domains.len() {
                result.push(Domain(i));
            }
            result
        }

        pub fn peek(&self, signal: Signal) -> Value {
            let signal_state = &self.signals[signal.id()];
            signal_state.values[1]
        }

        fn poke(&mut self, signal: Signal, value: Value) {
            let mut signal_state = &mut self.signals[signal.id()];
            signal_state.values[1] = value;
        }
    }
}

fn main() {
    let parser = parser::CircuitParser::new();
    let file = std::fs::read_to_string("Top.bitsy").unwrap();
    let circuit = parser.parse(&file).unwrap();
//    dbg!(&circuit);

    let mut simulator = sim::Simulator::new(Arc::new(circuit), "Top");

    let top_in_signal = simulator.signal_by_path("top.in").unwrap();
    let top_foo_signal = simulator.signal_by_path("top.foo").unwrap();

    println!("--------------------------------------------------------------------------------");
    simulator.step(
        sim::Domain::default(),
        vec![
            (top_in_signal, ast::Value::Bool(true)),
            (top_foo_signal, ast::Value::Word(42)),
        ],
    );
    println!("--------------------------------------------------------------------------------");
    simulator.step(
        sim::Domain::default(),
        vec![
            (top_in_signal, ast::Value::Bool(true)),
            (top_foo_signal, ast::Value::Word(42)),
        ],
    );
    println!("--------------------------------------------------------------------------------");
}
