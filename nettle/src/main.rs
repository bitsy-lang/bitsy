#![allow(dead_code)]

#[macro_use]
extern crate lazy_static;

mod circuit;
mod parse;
mod value;
mod expr;
mod sim;
mod testbench;
mod path;
mod ext;
#[cfg(test)]
mod tests;

use circuit::*;
use parse::*;
use value::*;
use expr::*;
use sim::*;
use testbench::*;
use path::*;
use ext::*;

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::sync::Arc;
use std::sync::Mutex;

fn main() {
    let argv: Vec<String> = std::env::args().collect();
    let default = "Top.nettle".to_string();
    let filename = argv.get(1).unwrap_or(&default);
    let text = std::fs::read_to_string(filename).unwrap();

    let top = parse_top(&text);
    let monitor = Box::new(Monitor::new());
    let ram = Box::new(Ram::new());
    let video = Box::new(Video::new());

    let mut nettle =
        Sim::new(&top)
            .ext("top.vip", monitor)
            .ext("top.ram", ram)
            .ext("top.video", video);

    if let Some(tb_filename) = testbench_for(filename) {
        println!("Using testbench file: {tb_filename}");
        let tb = read_testbench_file(&tb_filename).unwrap();
        for command in tb.0 {
            exec_tb_command(&mut nettle, command);
        }
    } else {
        let command = TestbenchCommand::Debug;
        exec_tb_command(&mut nettle, command);
        loop {
            match parse_testbench_command(&readline()) {
                Ok(command) => exec_tb_command(&mut nettle, command),
                Err(err) => eprintln!("{err:?}"),
            }
        }
    };
}

fn exec_tb_command(nettle: &mut Sim, command: TestbenchCommand) {
    let verbose = true;
    match command {
        TestbenchCommand::Peek(terminal) => {
            print!("PEEK {terminal} ");
            let value = nettle.peek(terminal);
            if verbose {
                println!("=> {value:?}");
            }
        },
        TestbenchCommand::Poke(terminal, value) => {
            if verbose {
                println!("POKE {terminal} <= {value:?}");
            }
            nettle.poke(terminal, value);
        },
        TestbenchCommand::Set(terminal, value) => {
            if verbose {
                println!("SET {terminal} = {value:?}");
            }
            nettle.set(terminal, value);
        },
        TestbenchCommand::Clock => {
            if verbose {
                println!("CLOCK");
            }
            nettle.clock();
        },
        TestbenchCommand::Reset => {
            if verbose {
                println!("RESET");
            }
            nettle.reset();
        },
        TestbenchCommand::Run => {
            if verbose {
                println!("RUN");
            }
            loop {
                nettle.clock();
            }
        },
        TestbenchCommand::Show => {
            println!("{nettle:#?}");
        },
        TestbenchCommand::Debug => {
            println!("{nettle:#?}");
            loop {
                match parse_testbench_command(&readline()) {
                    Ok(command) => {
                        if let TestbenchCommand::Debug = command {
                            () // you can't nest debug commands
                        } else if let TestbenchCommand::Show = command {
                            println!("{nettle:#?}");
                        } else {
                            exec_tb_command(nettle, command);
                            println!("{nettle:#?}");
                        }
                    },
                    Err(err) => eprintln!("{err:?}"),
                }
            }
        },
        TestbenchCommand::Eval(e) => {
            print!("EVAL {e:?}");
            let result = e.eval(&nettle);
            println!("=> {result:?}");
        },
        TestbenchCommand::Assert(e) => {
            let result = e.eval(&nettle);
            println!("ASSERT {e:?}");
            if result != Value::Bit(true) {
                println!("Assertion failed");
                for path in e.paths() {
                    println!("    {path} => {:?}", nettle.peek(path.clone()));

                }
                panic!("");
            }
        },
    }
}

lazy_static! {
    static ref READLINE: Arc<Mutex<rustyline::DefaultEditor>> = {
        let rl = rustyline::DefaultEditor::new().unwrap();
//        rl.load_history("history.txt").unwrap();
        Arc::new(Mutex::new(rl))
    };
}

fn readline() -> String {
    let mut rl = READLINE.lock().unwrap();
    loop {
        let result = rl.readline("nettle> ");
        match result {
            Ok(line) => {
                rl.add_history_entry(line.as_str()).unwrap();
                return line;
            },
            Err(rustyline::error::ReadlineError::Eof) => std::process::exit(0),
            Err(rustyline::error::ReadlineError::Interrupted) => (),
            Err(e) => panic!("{e:?}"),
        }
    }
}
