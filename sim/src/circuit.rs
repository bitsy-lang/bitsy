use crate::path::Path;
use crate::value::Value;

pub type NetId = usize;
pub type CombId = usize;
pub type ExtId = usize;
pub type RegId = usize;

#[derive(Debug)]
pub struct Circuit {
    pub components: Vec<Component>,
    pub combs: Vec<Comb>, // indexed by NetId
    pub dependents: Vec<Dependents>, // indexed by NetId
}

#[derive(Clone)]
pub struct Net(pub Vec<Path>);

#[derive(Debug)]
pub struct Dependents {
    pub combs: Vec<CombId>,
}

#[derive(Debug, Clone, Copy)]
pub enum Component {
    Terminal(Terminal),
    Register(Register),
}

impl Circuit {
    pub fn new() -> Circuit {
        Circuit {
            components: vec![],
            combs: vec![],
            dependents: vec![],
        }
    }

    pub fn add_terminal(&mut self) -> Terminal {
        let net_id = self.next_net_id();
        let terminal = Terminal(net_id);
        self.components.push(Component::Terminal(terminal.clone()));
        terminal
    }

    pub fn add_register(&mut self) -> Register {
        let val_net_id = self.next_net_id();
        let set_net_id = val_net_id + 1;
        let register = Register {
            val: Terminal(val_net_id),
            set: Terminal(set_net_id),
            //reset: Expr::Lit(Loc::unknown(), Value::X),
        };
        self.components.push(Component::Register(register.clone()));
        register
    }

    fn next_net_id(&self) -> NetId {
        self.terminals().len()
    }

    fn next_comb_id(&self) -> CombId {
        self.combs.len()
    }

    pub fn add_comb(&mut self, inputs: Vec<Terminal>, outputs: Vec<Terminal>, instrs: Vec<Instr>) -> CombId {
        let comb_id = self.next_comb_id();
        let comb = Comb::new(inputs, outputs, instrs);
        self.combs.push(comb);
        comb_id
    }

    pub fn terminals(&self) -> Vec<Terminal> {
        let mut results = vec![];
        for component in &self.components {
            match component {
                Component::Terminal(terminal) => results.push(terminal.clone()),
                Component::Register(register) => {
                    results.push(register.set().clone());
                    results.push(register.val().clone());
                },
            }
        }
        results
    }

    pub fn registers(&self) -> Vec<Register> {
        let mut results = vec![];
        for component in &self.components {
            match component {
                Component::Register(register) => results.push(register.clone()),
                Component::Terminal(_terminal) => (),
            }
        }
        results
    }

    pub fn net_ids(&self) -> Vec<NetId> {
        self.terminals().into_iter().map(|terminal| terminal.net_id()).collect()
    }

    pub fn combs_dependent_on(&self, net_id: NetId) -> Vec<Comb> {
        let mut results = vec![];
        for comb in &self.combs {
            if comb.depends_on_net(net_id) {
                results.push(comb.clone());
            }
        }
        results
    }
}

#[derive(Clone, Debug, Copy)]
pub struct Terminal(NetId);

impl Terminal {
    pub fn net_id(&self) -> NetId {
        self.0
    }
}

#[derive(Clone, Debug, Copy)]
pub struct Register {
    pub set: Terminal,
    pub val: Terminal,
}

impl Register {
    pub fn val(&self) -> Terminal {
        self.val.clone()
    }

    pub fn set(&self) -> Terminal {
        self.set.clone()
    }
}

#[derive(Debug, Clone)]
pub struct Comb {
    pub inputs: Vec<Terminal>,
    pub outputs: Vec<Terminal>,
    pub instrs: Vec<Instr>,
}

pub type Res = usize;

#[derive(Debug, Clone, Copy)]
pub enum Instr {
    Const(Value),
    Load(Res),
    Add(Res, Res),
    Neg(Res),
    Mux(Res, Res, Res),
}

impl Comb {
    pub fn new(inputs: Vec<Terminal>, outputs: Vec<Terminal>, instrs: Vec<Instr>) -> Comb {
        Comb { inputs, outputs, instrs }
    }

    pub fn inputs(&self) -> &[Terminal] {
        &self.inputs
    }

    pub fn outputs(&self) -> &[Terminal] {
        &self.outputs
    }

    pub fn instrs(&self) -> &[Instr] {
        &self.instrs
    }

    pub fn is_constant(&self) -> bool {
        self.inputs.len() == 0
    }

    pub fn depends_on_net(&self, net_id: NetId) -> bool {
        for input in &self.inputs {
            if input.net_id() == net_id {
                return true;
            }
        }
        false
    }

    pub fn eval(&self, inputs: &[Value]) -> Vec<Value> {
        let mut stack: Vec<Value> = inputs.to_vec();

        for instr in self.instrs().iter().cloned() {
            let result = match instr {
                Instr::Const(val) => val,
                Instr::Load(r0) => stack[r0],
                Instr::Add(r0, r1) => {
                    match (stack[r0], stack[r1]) {
                        (Value::Word(w0, n0), Value::Word(w1, n1)) if w0 == w1 => {
                            let mask = (1 << w0) - 1;
                            Value::Word(w0, (n0 + n1) & mask)
                        },
                        _ => panic!("Can't add non-Words."),
                    }
                },
                Instr::Neg(r0) => {
                    match stack[r0] {
                        Value::Word(w0, n0) => {
                            let mask = (1 << w0) - 1;
                            Value::Word(w0, (1 + !n0) & mask)
                        },
                        _ => panic!("Can't add non-Words."),
                    }
                },
                Instr::Mux(r0, r1, r2) => {
                    match stack[r0] {
                        Value::Word(_w, 0) => stack[r2],
                        Value::Word(_w, 1) => stack[r1],
                        _ => panic!("Can't mux non-Word<1>s."),
                    }
                },
            };
            stack.push(result);
        }
        let results = stack[stack.len()-self.outputs.len()..].to_vec();
        assert_eq!(results.len(), self.outputs.len());
        eprintln!("{stack:?}");
        results
    }
}
