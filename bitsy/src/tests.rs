use super::*;
use crate::sim::{Sim, Value};
use crate::sim::ext::ExtInstance;
use crate::sim::ext::monitor::Monitor;

use std::collections::{BTreeSet, BTreeMap};
use std::sync::Arc;

#[test]
fn path_depends() {
    let tests = vec![
        ("top.x", vec!["top.x"]),
        ("1w8", vec![]),
        ("(top.a + top.b)", vec!["top.a", "top.b"]),
        ("(top.b - top.a)", vec!["top.a", "top.b"]),
        ("(top.a == top.b)".into(), vec!["top.a", "top.b"]),
        ("(top.a != top.b)".into(), vec!["top.a", "top.b"]),
        ("(!top.x)".into(), vec!["top.x"]),
        ("if top.x { top.a } else { top.b }".into(), vec!["top.a", "top.b", "top.x"]),
//        ("if !top.x { top.a } else { top.b }".into(), ),
    ];

    for (expr_str, paths) in tests {
        let paths: Vec<Path> = paths.into_iter().map(|s| s.into()).collect();
        let expr: Expr = expr_str.into();
        assert_eq!(expr.paths(), paths, "{expr:?} has paths {:?} /= {paths:?}", expr.paths());
    }
}

#[test]
fn buffer() {
    let buffer = load_package_from_string("
        mod Top {
            incoming in of Word<1>;
            reg r of Word<1>;
            outgoing out of Word<1>;
            r <= in;
            out := r;
        }
    ").unwrap();
    let buffer = buffer.top("Top").unwrap();

    let mut bitsy = Sim::new(&buffer);

    bitsy.poke("top.in", true.into());
    dbg!(&bitsy.peek("top.r"));
    assert_eq!(bitsy.peek("top.r"), Value::X);
    assert_eq!(bitsy.peek("top.out"), Value::X);

    bitsy.clock();
    assert_eq!(bitsy.peek("top.r"), true.into());
    assert_eq!(bitsy.peek("top.out"), true.into());
}

#[test]
fn counter() {
    let counter = load_package_from_string("
        mod Top {
            outgoing out of Word<4>;
            reg counter of Word<4> reset 0w4;
            out := counter;
            counter <= counter + 1w4;
        }
    ").unwrap();
    let counter = counter.top("Top").unwrap();

    let mut bitsy = Sim::new(&counter);

    bitsy.reset();

    for i in 0..16 {
        assert_eq!(bitsy.peek("top.out"), Value::Word(4, i));
        bitsy.clock();
    }
    assert_eq!(bitsy.peek("top.out"), Value::Word(4, 0));
}

#[test]
fn triangle_numbers() {
    let top = load_package_from_string("
        mod Top {
            outgoing out of Word<32>;
            reg sum of Word<32> reset 0w32;
            mod counter {
                outgoing out of Word<32>;
                reg counter of Word<32> reset 1w32;
                out := counter;
                counter <= counter + 1w32;
            }
            out := sum;
            sum <= sum + counter.out;
        }
    ").unwrap();
    let top = top.top("Top").unwrap();

    let mut bitsy = Sim::new(&top);
    bitsy.reset();

    for i in 0..16 {
        let triange = (i * (i + 1)) / 2;
        assert_eq!(bitsy.peek("top.out"), Value::Word(32, triange), "Failed on iteration i = {i}");
        bitsy.clock();
    }
}

#[test]
fn vip() {
    let top = load_package_from_string("
        mod Top {
            mod counter {
                outgoing out of Word<4>;
                reg counter of Word<4>;
                counter <= counter + 1w4;
                out := counter;
            }

            mod vip of Vip;
            vip.in := counter.out;
        }

        ext mod Vip {
            incoming in of Word<4>;
        }

    ").unwrap();
    let top = top.top("Top").unwrap();

    let monitor = Box::new(Monitor::new());
    let mut exts: BTreeMap<Path, Box<dyn ExtInstance>> = BTreeMap::new();
    exts.insert("top.vip".into(), monitor);

    let mut bitsy = Sim::new_with_exts(&top, exts);

    bitsy.reset();
    bitsy.clock();
    bitsy.clock();
    bitsy.clock();
    bitsy.clock();
}

#[test]
fn ifs() {
    let top = load_package_from_string("
        mod Top {
            outgoing out of Word<8>;
            incoming in of Word<1>;

            out := if in {
                42w8
            } else {
                100w8
            };
        }
    ").unwrap();
    let top = top.top("Top").unwrap();

    let mut bitsy = Sim::new(&top);

    bitsy.poke("top.in", true.into());
    assert_eq!(bitsy.peek("top.out"), Value::Word(8, 42));
    bitsy.poke("top.in", false.into());
    assert_eq!(bitsy.peek("top.out"), Value::Word(8, 100));
}

#[test]
fn test_parse() {
    let exprs = vec![
        "x",
        "x.y",
        "1w8",
        "true",
        "false",
        "X",
        "(a + b)",
        "(a && b)",
        "(a || b)",
        "(a == b)",
        "(a != b)",
        "(!a)",
        "if c { e1 } else { e2 }",
    ];
    for e in exprs {
        let expr: Expr = e.into();
        assert_eq!(format!("{:?}", expr), e);
    }
}

#[test]
fn test_node() {
    let top = load_package_from_string("
        mod Top {
            outgoing out of Word<1>;
            node n of Word<1>;
            n := 1w1;
            out := n;
        }
    ").unwrap();
    let top = top.top("Top").unwrap();

    let bitsy = Sim::new(&top);
    assert_eq!(bitsy.peek("top.n"), Value::Word(1, 1));
}

#[test]
fn test_check() {
    let top = load_package_from_string("
        mod Top {
            outgoing out of Word<8>;
            incoming in of Word<8>;

            out := in;
        }
    ").unwrap();
    let top = top.top("Top").unwrap();

    top.package().check().unwrap();

    /*
    let top2 = parse_top("
        top {
            outgoing out of Word<8>;
            incoming in of Word<8>;
            node n of Word<1>;
            in := n;
        }
    ", None);

    dbg!(top2.check());
    */
}

#[test]
fn typeinfer() {
    let tests = vec![
        ("1w8", Context::empty(), Some(Type::word(8))),
        ("7w3", Context::empty(), Some(Type::word(3))),
        ("?foo", Context::empty(), None),
        ("if 0w1 { 0w2 } else { 0w2 }", Context::empty(), None),
        ("1w4 + 1w4", Context::empty(), None),
        ("!1w4", Context::empty(), None),
    ];

    for (expr_str, ctx, type_expected) in tests {
        let expr: Arc<Expr> = Arc::new(expr_str.into());
        let type_actual = expr.typeinfer(ctx);
        assert!(
            type_actual == type_expected,
            "typeinfer did not produced expected type for: {expr_str}. Produced {type_actual:?}"
        );
    }
}

#[test]
fn typecheck() {
    let test_err = vec![
        ("x", Context::empty(), Type::word(1)),
        ("x", Context::from(vec![("x".into(), Type::word(8))]), Type::word(1)),
        ("1w8", Context::empty(), Type::word(7)),
        ("8w3", Context::empty(), Type::word(3)),
        ("if 0w2 { 0w2 } else { 0w2 }", Context::empty(), Type::word(2)),
        ("if 0w1 { 0w3 } else { 0w2 }", Context::empty(), Type::word(2)),
    ];

    for (expr_str, ctx, type_expected) in test_err {
        let expr: Arc<Expr> = Arc::new(expr_str.into());
        assert!(expr.typecheck(type_expected, ctx).is_err(), "typecheck passsed but shouldn't have: {expr_str}");
    }

    let test_ok = vec![
        ("1w8", Context::empty(), Type::word(8)),
        ("7w3", Context::empty(), Type::word(3)),
        ("?foo", Context::empty(), Type::word(3)),
        ("?foo", Context::empty(), Type::word(8)),
        ("if 0w1 { 0w2 } else { 0w2 }", Context::empty(), Type::word(2)),
        /*
        ("(top.a + top.b)", vec!["top.a", "top.b"]),
        ("(top.b - top.a)", vec!["top.a", "top.b"]),
        ("(top.a == top.b)".into(), vec!["top.a", "top.b"]),
        ("(top.a != top.b)".into(), vec!["top.a", "top.b"]),
        ("(!top.x)".into(), vec!["top.x"]),
        ("if top.x { top.a } else { top.b }".into(), vec!["top.a", "top.b", "top.x"]),
        ("if !top.x { top.a } else { top.b }".into(), ),
        */
    ];

    for (expr_str, ctx, type_expected) in test_ok {
        let expr: Arc<Expr> = Arc::new(expr_str.into());
        assert!(expr.typecheck(type_expected, ctx).is_ok(), "typecheck failed but shouldn't have: {expr_str}");
    }
}

#[test]
fn test_examples() {
    let examples_dir = std::path::Path::new("examples");
    let mut errors = vec![];

    if let Ok(entries) = std::fs::read_dir(examples_dir) {
        for entry in entries {
            if let Ok(entry) = entry {
                let filename = entry.file_name();
                if let Some(filename) = filename.to_str() {
                    if filename.ends_with(".bitsy") {
                        let text = match std::fs::read_to_string(entry.path()) {
                            Ok(text) => text,
                            Err(_) => panic!("Failed to read file {:?}", entry.path()),
                        };

                        if let Err(_error) = std::panic::catch_unwind(|| {
                            let package = load_package_from_string(&text).expect(&format!("Testing {:?}", entry.path()));
                            package.check().expect(&format!("Failed to check: {filename}"));
                        }) {
                            errors.push(filename.to_string());
                        }
                    }
                }
            }
        }
    } else {
        panic!("Failed to read examples directory");
    }

    if errors.len() > 0 {
        panic!("Errors in examples:\n  - {}", errors.join("\n  - "))
    }
}

#[test]
fn test_locs() {
    let text = "
        mod Top {
            outgoing out of Word<8>;
            incoming in of Word<8>;

            out := in;
        }
    ";

    let source_info = SourceInfo::from_string(text);
    let package = load_package_from_string(text).unwrap();
    package.check().unwrap();
    let top = package.top("Top").unwrap();
    let wires = top.wires();
    let Wire(_loc, _target, expr, _wiretype) = wires.first().unwrap();
    assert_eq!(source_info.start(expr).to_string(), "6:20");
    assert_eq!(source_info.end(expr).to_string(), "6:22");
}

#[test]
fn test_parse_package_from_string() {
    let examples_dir = std::path::Path::new("examples");
    let mut errors = vec![];

    if let Ok(entries) = std::fs::read_dir(examples_dir) {
        for entry in entries {
            if let Ok(entry) = entry {
                let filename = entry.file_name();
                if let Some(filename) = filename.to_str() {
                    if filename.ends_with(".bitsy") {
                        let text = match std::fs::read_to_string(entry.path()) {
                            Ok(text) => text,
                            Err(_) => panic!("Failed to read file {:?}", entry.path()),
                        };

                        if let Err(_error) = std::panic::catch_unwind(|| {
                            let _package = ast::parse_package_from_string(&text).expect(&format!("Testing {:?}", entry.path()));
                        }) {
                            errors.push(filename.to_string());
                        }
                    }
                }
            }
        }
    } else {
        panic!("Failed to read examples directory");
    }

    if errors.len() > 0 {
        panic!("Errors in examples:\n  - {}", errors.join("\n  - "))
    }
}
