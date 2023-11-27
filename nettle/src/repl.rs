use super::*;

pub struct Repl {
    current_path: Path,
    sim: Sim,
    testbench: Testbench,
    readline: rustyline::DefaultEditor,
}

impl Repl {
    pub fn new(sim: Sim, testbench: Testbench) -> Repl {
        let current_path = sim.root();
        let readline = rustyline::DefaultEditor::new().unwrap();

        Repl {
            current_path,
            sim,
            testbench,
            readline,
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
        let commands = self.testbench.1.clone();
        for command in commands{
            self.exec_tb_command(command);
        }
    }

    fn exec_tb_command(&mut self, command: TestbenchCommand) {
        let verbose = true;
        match command {
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
                self.sim.set(terminal, value);
            },
            TestbenchCommand::Clock => {
                if verbose {
                    println!("CLOCK");
                }
                self.sim.clock();
            },
            TestbenchCommand::Reset => {
                if verbose {
                    println!("RESET");
                }
                self.sim.reset();
            },
            TestbenchCommand::Run => {
                if verbose {
                    println!("RUN");
                }
                loop {
                    self.sim.clock();
                }
            },
            TestbenchCommand::Show => {
                println!("{:#?}", self.sim);
            },
            TestbenchCommand::Debug => {
                println!("{:#?}", self.sim);
                loop {
                    match parse_testbench_command(&self.readline()) {
                        Ok(command) => {
                            if let TestbenchCommand::Debug = command {
                                () // you can't nest debug commands
                            } else if let TestbenchCommand::Show = command {
                                println!("{:#?}", self.sim);
                            } else {
                                self.exec_tb_command(command);
                                println!("{:#?}", self.sim);
                            }
                        },
                        Err(err) => eprintln!("{err:?}"),
                    }
                }
            },
            TestbenchCommand::Eval(e) => {
                print!("EVAL {e:?}");
                let result = e.eval(&self.sim);
                println!("=> {result:?}");
            },
            TestbenchCommand::Assert(e) => {
                let result = e.eval(&self.sim);
                println!("ASSERT {e:?}");
                if result != true.into() {
                    println!("Assertion failed");
                    for path in e.paths() {
                        println!("    {path} => {:?}", self.sim.peek(path.clone()));

                    }
                    panic!("");
                }
            },
        }
    }
}
