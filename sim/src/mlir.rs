use super::*;

impl Comb {
    pub fn mlir(&self) {
        println!("hw.module @FOO(");
        println!("    {}", self.port_list());
        println!(") {{");
        for (i, instr) in self.instrs().iter().enumerate() {
            let rd = i + self.inputs().len();
            println!("    // {instr:?}");
            print!("    %{rd} = ");
            match instr {
                Instr::Const(Value::Word(w, n)) => {
                    println!("hw.constant {n} : i{w}");
                },
                Instr::Add(r0, r1) => {
                    println!("comb.add %{r0}, %{r1} : i8");
                },
                Instr::Load(r0) => {
                    println!("comb.add %{r0} : i8");
                },
                _ => todo!(),
            }
        }

        print!("    hw.output ");
        for i in 0..self.outputs.len() {
            let rd = self.inputs().len() + self.instrs().len() - self.outputs.len() + i;
            let is_last = i == self.outputs.len() - 1;
            print!("%{rd}");
            if !is_last {
                print!(", ");
            }
        }

        print!(" : ");

        for i in 0..self.outputs.len() {
            let is_last = i == self.outputs.len() - 1;
            print!("i{}", 8);
            if !is_last {
                print!(", ");
            } else {
                println!();
            }
        }
        println!("}}");
    }

    fn port_list(&self) -> String {
        let mut ports: Vec<String> = vec![];
        ports.extend(self.inputs().iter().enumerate().map(|(i, _terminal)| format!("in %{} : i{}", i, 8)));
        ports.extend(self.outputs().iter().map(|terminal| format!("out net_{} : i{}", terminal.net_id(), 8)));
        ports.join(",\n    ")
    }
}
