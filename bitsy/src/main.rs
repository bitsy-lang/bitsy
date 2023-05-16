#![allow(unused, dead_code)]

use nettle::ast::Nettle;
use bitsy::Bitsy;
use bitsy::verilog::*;

fn main() {
    init_logging();

    let argv: Vec<String> = std::env::args().collect();
    let default = "Top.bitsy".to_string();
    let filename = argv.get(1).unwrap_or(&default);
    let text = std::fs::read_to_string(filename).unwrap();

    let mut bitsy = Bitsy::new();
    if let Err(err) = bitsy.add(&text) {
        eprintln!("{err:?}");
        std::process::exit(-1);
    }

    let top = bitsy.module_by_name(&"Top").unwrap();
    use bitsy::flatten::*;
    let verilog = top.to_verilog();
    print!("{verilog}");

    /*
    {
        Err(e) => {
            match (e) {
                ParseError::InvalidToken { location } => {
                    eprintln!("Syntax error: Invalid token");
                    eprintln!();

                    let mut bad_line_start = 0;
                    let mut bad_line_end = 0;
                    let mut bad_lineno = 1;

                    let mut text_lines: Vec<&str> = text.split("\n").collect();
                    if text_lines[text_lines.len()-1] == "" {
                        text_lines.pop();
                    }

                    for line in &text_lines {
                        if bad_line_start + line.len() >= location {
                            bad_line_end = bad_line_start + line.len() + 1;
                            break;
                        } else {
                            bad_line_start += line.len() + 1;
                            bad_lineno += 1;
                        }
                    }

                    let spaces = String::from(" ").repeat(location - bad_line_start);
                    let carrots = String::from("^");


                    for i in 0..text_lines.len() {
                        if i + 5 >= bad_lineno && i <= bad_lineno + 5 {
                            eprintln!("{:>6}    {}", i + 1, &text_lines[i]);
                        }
                        if i + 1 == bad_lineno {
                            eprintln!("          {spaces}{carrots}");
                        }
                    }

                    eprintln!();
                },
                ParseError::UnrecognizedToken { token, expected } => {
                    let (location_start, found, location_end) = token;
                    eprintln!("Syntax error: Expected one of {} but found {:?}", expected.join(", "), found.to_string());
                    eprintln!();

                    let mut bad_line_start = 0;
                    let mut bad_line_end = 0;
                    let mut bad_lineno = 1;

                    let mut text_lines: Vec<&str> = text.split("\n").collect();
                    if text_lines[text_lines.len()-1] == "" {
                        text_lines.pop();
                    }

                    for line in &text_lines {
                        if bad_line_start + line.len() >= location_start {
                            bad_line_end = bad_line_start + line.len() + 1;
                            break;
                        } else {
                            bad_line_start += line.len() + 1;
                            bad_lineno += 1;
                        }
                    }

                    let spaces = String::from(" ").repeat(location_start - bad_line_start);
                    let carrots = String::from("^").repeat(location_end - location_start);


                    for i in 0..text_lines.len() {
                        if i + 5 >= bad_lineno && i <= bad_lineno + 5 {
                            eprintln!("{:>6}    {}", i + 1, &text_lines[i]);
                        }
                        if i + 1 == bad_lineno {
                            eprintln!("          {spaces}{carrots}");
                        }
                    }

                    eprintln!();
                },
                ParseError::UnrecognizedEOF { location, expected } => {
                    eprintln!("Syntax error: Expected {} but found the end of the file", expected.join(", "));
                    eprintln!();

                    let mut bad_line_start = 0;
                    let mut bad_line_end = 0;
                    let mut bad_lineno = 1;

                    let mut text_lines: Vec<&str> = text.split("\n").collect();
                    if text_lines[text_lines.len()-1] == "" {
                        text_lines.pop();
                    }

                    for line in &text_lines {
                        if bad_line_start + line.len() >= location {
                            bad_line_end = bad_line_start + line.len() + 1;
                            break;
                        } else {
                            bad_line_start += line.len() + 1;
                            bad_lineno += 1;
                        }
                    }

                    let spaces = String::from(" ").repeat(location - bad_line_start);
                    let carrots = String::from("^");

                    for i in 0..text_lines.len() {
                        if i + 5 >= bad_lineno && i <= bad_lineno + 5 {
                            eprintln!("{:>6}    {}", i + 1, &text_lines[i]);
                        }
                        if i + 1 == bad_lineno {
                            eprintln!("          {spaces}{carrots}");
                        }
                    }

                    eprintln!();
                },
                _ => eprintln!("{e:?}"),
            }
        }
    }
    */
    /*
    let verilog = bitsy::verilog::Verilog {
        filename: "Top.v".to_string(),
        modules: vec![
            Module {
                name: "Top".to_string(),
                ports: vec![
                    Port {
                        name: "clock_12MHz".to_string(),
                        width: None,
                        direction: Direction::Input,
                    },
                    Port {
                        name: "write_data".to_string(),
                        width: Some(16),
                        direction: Direction::Input,
                    },
                ],
                regs: vec![
                    Reg {
                        name: "foo".to_string(),
                        width: None,
                    },
                    Reg {
                        name: "bar".to_string(),
                        width: Some(8),
                    },
                ],
                insts: vec![
                    Inst {
                        module_name: "Baz".to_string(),
                        instance_name: "baz".to_string(),
                        connections: vec![
                            ("i_thingy".to_string(), "bar".to_string()),
                            ("i_dingy".to_string(), "foo".to_string()),
                        ].into_iter().collect(),
                    },
                ],
                alwayses: vec![
                    Always {
                        sensitivity_list: vec![Sensitivity(Edge::PosEdge, "clock_12MHz".to_string())],
                    },
                ],
            },
        ],
    };
    */
}

fn init_logging() {
    simple_logger::SimpleLogger::new()
        .with_level(log::LevelFilter::Debug)
        .init()
        .unwrap();
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_main() {
        let filename = "Top.bitsy".to_string();
        let text = std::fs::read_to_string(filename).unwrap();

        let mut bitsy = Bitsy::new();
        bitsy.add(&text);
    }
}
