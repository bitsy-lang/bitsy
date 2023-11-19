use super::*;

#[test]
fn path_depends() {
    let tests = vec![
        ("top.x", vec!["top.x"]),
        ("1w8", vec![]),
        ("true", vec![]),
        ("false", vec![]),
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
fn expand_regs() {
    let m = Module::new("top")
        .reg("r", Value::X)
        .node("n")
        .node("m")
        .wire("r", &Expr::Path("n".into()))
        .wire("m", &Expr::Path("r".into()))
        .build();

    assert!(m.wires.contains_key(&"top.r.set".into()));
    assert!(!m.wires.contains_key(&"top.r".into()));
    assert!(m.wires.contains_key(&"top.m".into()));
    assert_eq!(m.wires[&"top.m".into()], Expr::Path("top.r.val".into()));
}

#[test]
fn buffer() {
    let buffer = parse_top("
        top {
            port in;
            reg r;
            port out;
            r <= in;
            out <= r;
        }
    ");

    let mut nettle = Nettle::new(&buffer);

    nettle.poke("top.in", true.into());
    assert_eq!(nettle.peek("top.r"), Value::X);
    assert_eq!(nettle.peek("top.out"), Value::X);

    nettle.clock();
    assert_eq!(nettle.peek("top.r"), true.into());
    assert_eq!(nettle.peek("top.out"), true.into());
}

#[test]
fn counter() {
    let counter = parse_top("
        top {
            port out;
            reg counter reset 0w4;
            out <= counter;
            counter <= counter.val + 1w4;
        }
    ");

    let mut nettle = Nettle::new(&counter);

    nettle.reset();

    for i in 0..16 {
        assert_eq!(nettle.peek("top.out"), Value::Word(4, i));
        nettle.clock();
    }
    assert_eq!(nettle.peek("top.out"), Value::Word(4, 0));
}

#[test]
fn triangle_numbers() {
    let top = parse_top("
        top {
            port out;
            reg sum reset 0w32;
            mod counter {
                port out;
                reg counter reset 1w32;
                out <= counter;
                counter <= counter + 1w4;
            }
            out <= sum;
            sum <= sum + counter.out;
        }
    ");

    let mut nettle = Nettle::new(&top);
    nettle.reset();

    for i in 0..16 {
        let triange = (i * (i + 1)) / 2;
        assert_eq!(nettle.peek("top.out"), Value::Word(32, triange));
        nettle.clock();
    }
}

#[test]
fn vip() {
    let top = parse_top("
        top {
            mod counter {
                port out;
                reg counter;
                counter <= counter + 1w4;
                out <= counter;
            }

            ext vip {
                port in;
            }

            vip.in <= counter.out;
        }
    ");

    let monitor = Box::new(Monitor::new());

    let mut nettle =
        Nettle::new(&top)
            .ext("top.vip", monitor);

    nettle.set("top.counter.counter", Value::Word(4, 0));
    nettle.clock();
    nettle.clock();
    nettle.clock();
    nettle.clock();
}

#[test]
fn ifs() {
    let top = parse_top("
        top {
            port out;
            port in;

            out <= if in {
                42w8
            } else {
                100w8
            };
        }
    ");

    let mut nettle = Nettle::new(&top);

    nettle.poke("top.in", Value::Bit(true));
    assert_eq!(nettle.peek("top.out"), Value::Word(8, 42));
    nettle.poke("top.in", Value::Bit(false));
    assert_eq!(nettle.peek("top.out"), Value::Word(8, 100));
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
fn test_eval() {
    let buffer = parse_top("
        top {
            node x;
            reg r reset false;
            x <= true;
            r <= r;

            reg a reset 2w32;
            reg b reset 3w32;
            a <= a;
            b <= b;

            node p;
            node q;
            p <= true;
            q <= false;
        }
    ");

    let mut nettle = Nettle::new(&buffer);
    nettle.reset();

    let tests = vec![
        ("top.x", Value::Bit(true)),
        ("1w8", Value::Word(8, 1)),
        ("true", Value::Bit(true)),
        ("false", Value::Bit(false)),
        ("(top.a + top.b)", Value::Word(32, 5)),
        ("(top.b - top.a)", Value::Word(32, 1)),
//        "(a && b)",
//        "(a || b)",
        ("(top.a == top.b)".into(), Value::Bit(false)),
        ("(top.b < top.a)".into(), Value::Bit(false)),
        ("(top.a != top.b)".into(), Value::Bit(true)),
        ("(!top.x)".into(), Value::Bit(false)),
        ("if top.x { top.a } else { top.b }".into(), Value::Word(32, 2)),
        ("if !top.x { top.a } else { top.b }".into(), Value::Word(32, 3)),
    ];

    for (expr_str, v) in tests {
        let expr: Expr = expr_str.into();
        assert_eq!(expr.eval(&nettle), v, "{expr:?} does not equal {v:?}");
    }
}