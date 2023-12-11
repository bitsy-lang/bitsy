use super::*;

pub struct Repl {
    current_path: Path,
    sim: Sim,
    circuit: Circuit,
    testbench: Testbench,
    readline: rustyline::DefaultEditor,
    watches: Vec<Watch>,
}

impl Repl {
    pub fn new(sim: Sim, circuit: Circuit, testbench: Testbench) -> Repl {
        let current_path = "top".into();
        let readline = rustyline::DefaultEditor::new().unwrap();

        Repl {
            current_path,
            sim,
            circuit,
            testbench,
            readline,
            watches: vec![],
        }
    }

    fn readline(&mut self) -> String {
        loop {
            let result = self.readline.readline(&format!("{}> ", self.current_path));
            match result {
                Ok(line) => {
                    self.readline.add_history_entry(line.as_str()).unwrap();
                    return line;
                },
                Err(rustyline::error::ReadlineError::Eof) => std::process::exit(0),
                Err(rustyline::error::ReadlineError::Interrupted) => (),
                Err(e) => panic!("{e:?}"),
            }
        }
    }

    pub fn run(&mut self) {
        // self.show();
        let commands = self.testbench.2.clone();
        for command in commands{
            self.exec_tb_command(command);
        }
    }

    fn exec_tb_command(&mut self, command: TestbenchCommand) {
        let verbose = true;
        match command {
            TestbenchCommand::Watch(watch) => self.watches.push(watch),
            TestbenchCommand::Cd(path) => {
                if let Some(path) = path {
                    if path == "..".into() { // HACK
                        self.current_path = self.current_path.parent();
                    } else {
                        let new_path = self.current_path.join(path);
                        if let Some(component) = self.circuit.component(new_path.clone()) {
                            if component.is_mod() {
                                self.current_path = new_path;
                            } else {
                                eprintln!("{new_path} is not a mod");
                            }
                        } else {
                            eprintln!("No such path: {new_path}");
                        }
                    }
                } else {
                    self.current_path = "top".into();
                }
            },
            TestbenchCommand::Peek(terminal) => {
                print!("PEEK {terminal} ");
                let value = self.sim.peek(terminal);
                if verbose {
                    println!("=> {value:?}");
                }
            },
            TestbenchCommand::Poke(terminal, value) => {
                if verbose {
                    println!("POKE {terminal} <= {value:?}");
                }
                self.sim.poke(terminal, value);
            },
            TestbenchCommand::Set(terminal, value) => {
                if verbose {
                    println!("SET {terminal} = {value:?}");
                }
                // TODO
                // self.sim.set(terminal, value);
                eprintln!("Not implemented");
            },
            TestbenchCommand::Clock => {
                if verbose {
                    println!("CLOCK");
                }
                self.sim.clock();
                self.show_watches();
            },
            TestbenchCommand::Reset => {
                if verbose {
                    println!("RESET");
                }
                self.sim.reset();
                self.show_watches();
            },
            TestbenchCommand::Run => {
                if verbose {
                    println!("RUN");
                }
                loop {
                    self.sim.clock();
                    self.show_watches();
                }
            },
            TestbenchCommand::Show => {
                self.show();
            },
            TestbenchCommand::Debug => {
                loop {
                    match parse_testbench_command(&self.readline()) {
                        Ok(command) => {
                            if let TestbenchCommand::Debug = command {
                                break;
                            } else if let TestbenchCommand::Show = command {
                                self.show();
                            } else {
                                self.exec_tb_command(command);
//                                self.show();
                            }
                        },
                        Err(err) => eprintln!("{err:?}"),
                    }
                }
            },
            TestbenchCommand::Eval(e) => {
                print!("EVAL {e:?}");
                let result = e.rebase(self.current_path.clone()).eval(&self.sim);
                println!("=> {result:?}");
            },
            TestbenchCommand::Assert(e) => {
                let result = e.eval(&self.sim);
                println!("ASSERT {e:?}");
                if let Value::Word(1, 1) = result {
                    // Do nothing.
                } else {
                    println!("Assertion failed");
                    for path in e.paths() {
                        println!("    {path} => {:?}", self.sim.peek(path.clone()));

                    }
                    panic!("");
                }
            },
        }
    }

    fn show(&self) {
        for (net_id, value) in self.sim.net_values() {
            let net = &self.sim.net(net_id);
            let terminals = net.terminals()
                .iter()
                .map(|t| t.to_string())
                .filter(|p| p.starts_with(&self.current_path.to_string()))
                .map(|p| p[self.current_path.to_string().len() + 1..].to_string())
                .collect::<Vec<String>>();

            if !terminals.is_empty() {
                print!("{:>4}    {:>5}   ", format!("#{net_id}"), format!("{value:?}"));
                println!("{}", terminals.join(" "));
            }
        }
    }

    fn show_watches(&self) {
        for Watch { path, format } in &self.watches {
            let value = self.sim.peek(&**path);
            match format {
                WatchFormat::Normal =>  println!("    {:>10}   {path}", format!("{value:?}")),
                WatchFormat::Hex =>     println!("    {:>10}   {path}", format!("{value:x}")),
                WatchFormat::Bin =>     println!("    {:>10}   {path}", format!("{value:b}")),
                WatchFormat::Bool =>    println!("    {:>10}   {path}", value.to_bool().unwrap()),
            }
        }
    }
}
