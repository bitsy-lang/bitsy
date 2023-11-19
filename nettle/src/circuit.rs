use super::*;

#[derive(Debug)]
pub struct CircuitNode {
    paths: BTreeMap<Path, PathType>,
    wires: BTreeMap<Path, Expr>,
    path: Vec<String>,
    exts: Vec<Path>,
}

#[derive(Debug, Clone)]
pub struct Circuit(Arc<CircuitNode>);

impl std::ops::Deref for Circuit {
    type Target = CircuitNode;
    fn deref(&self) -> &CircuitNode {
        &self.0
    }
}

impl Circuit {
    pub fn new(name: &str) -> CircuitNode {
        CircuitNode {
            paths: BTreeMap::new(),
            wires: BTreeMap::new(),
            path: vec![name.to_string()],
            exts: vec![],
        }
    }

    pub fn paths(&self) -> &BTreeMap<Path, PathType> {
        &self.paths
    }

    pub fn wires(&self) -> &BTreeMap<Path, Expr> {
        &self.wires
    }

    pub fn path(&self) -> &Vec<String> {
        &self.path
    }

    pub fn exts(&self) -> &Vec<Path> {
        &self.exts
    }
}

impl CircuitNode {
    pub fn module(mut self, path: &str, with_module: impl FnOnce(Self) -> Self) -> Self {
        self = self.push(path);
        self = with_module(self);
        self = self.pop();
        self
    }

    fn push(mut self, path: &str) -> Self {
        self.path.push(path.to_string());
        self
    }

    fn pop(mut self) -> Self {
        self.path.pop();
        self
    }

    fn terminal(&self, name: &str) -> Path {
        let path = self.path.join(".");
        format!("{path}.{name}").into()
    }

    pub fn node(mut self, name: &str) -> Self {
        let terminal = self.terminal(name);
        self.paths.insert(terminal, PathType::Node);
        self
    }

    pub fn reg(mut self, name: &str, reset: Value) -> Self {
        let path = self.terminal(name);
        let set_path = format!("{path}.set");
        let val_path = format!("{path}.val");

        self.paths.insert(path, PathType::Reg(reset));
        self.paths.insert(set_path.into(), PathType::Node);
        self.paths.insert(val_path.into(), PathType::Node);
        self
    }

    pub fn wire(mut self, name: &str, expr: &Expr) -> Self {
        let terminal = self.terminal(name);
        self.wires.insert(terminal, expr.clone().relative_to(&self.current_path()));
        self
    }

    pub fn instantiate(mut self, name:  &str, circuit: &CircuitNode) -> Self {
        let path = self.current_path();
        self = self.push(name);

        for (terminal, typ) in &circuit.paths {
            let target = relative_to(&path, terminal);
            self.paths.insert(target, typ.clone());
        }

        for (terminal, expr) in &circuit.wires {
            let target = relative_to(&path, terminal);
            let expr = expr.clone().relative_to(&path);
            self.wires.insert(target, expr);
        }
        self = self.pop();
        self
    }

    fn current_path(&self) -> Path {
        self.path.join(".").into()
    }

    pub fn ext(mut self, name: &str, terminals: &[&str]) -> Self {
        let ext = self.terminal(name);
        self.exts.push(ext.clone());

        for terminal in terminals {
            let target = format!("{ext}.{terminal}");
            self.paths.insert(target.into(), PathType::Node);
        }
        self
    }

    fn regs(&self) -> Vec<Path> {
        let mut result = vec![];
        for (path, typ) in &self.paths {
            if let PathType::Reg(_reset) = typ {
                result.push(path.clone());
            }
        }
        result
    }

    fn expand_regs(mut self) -> Self {
        let regs: Vec<Path> = self.regs();

        // fix sets (on the right)
        let targets: Vec<Path> = self.wires.keys().cloned().collect();
        for target in targets {
            if regs.contains(&target) {
                let set_path = format!("{target}.set");
                let expr = self.wires.remove(&target).unwrap();
                self.wires.insert(set_path.into(), expr);
            }
        }

        // fix vals (on the left)
        let mut wires: Vec<(Path, Expr)> = vec![];
        for (target, expr) in &self.wires {
            let expr = expr.clone().expand_regs_as_val(&regs);
            wires.push((target.clone(), expr));
        }
        self.wires = wires.into_iter().collect();
        self
    }

    pub fn build(self) -> Circuit {
        Circuit(Arc::new(self.expand_regs()))
    }
}
