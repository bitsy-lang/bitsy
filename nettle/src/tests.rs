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
    let m = Circuit::new("top")
        .reg("r", Type::Word(1), Value::X)
        .node("n", Type::Word(1))
        .node("m", Type::Word(1))
        .wire("r", &Expr::Reference("n".into()))
        .wire("m", &Expr::Reference("r".into()))
        .build();

    assert!(m.wires().contains_key(&"top.r.set".into()));
    assert!(!m.wires().contains_key(&"top.r".into()));
    assert!(m.wires().contains_key(&"top.m".into()));
    assert_eq!(m.wires()[&"top.m".into()], Expr::Reference("top.r.val".into()));
}

#[test]
fn buffer() {
    let buffer = parse_top("
        top {
            incoming in of Bit;
            reg r of Bit;
            outgoing out of Bit;
            r <= in;
            out <= r;
        }
    ");

    let mut nettle = Sim::new(&buffer);

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
            outgoing out of Word<4>;
            reg counter of Word<4> reset 0w4;
            out <= counter;
            counter <= counter.val + 1w4;
        }
    ");

    let mut nettle = Sim::new(&counter);

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
            outgoing out of Word<32>;
            reg sum of Word<32> reset 0w32;
            mod counter {
                outgoing out of Word<32>;
                reg counter of Word<32> reset 0w32;
                out <= counter;
                counter <= counter + 1w32;
            }
            out <= sum;
            sum <= sum + counter.out;
        }
    ");

    let mut nettle = Sim::new(&top);
    nettle.reset();

    for i in 0..16 {
        let triange = (i * (i + 1)) / 2;
        assert_eq!(nettle.peek("top.out"), Value::Word(32, triange), "Failed on iteration i = {i}");
        nettle.clock();
    }
}

#[test]
fn vip() {
    let top = parse_top("
        top {
            mod counter {
                outgoing out of Word<4>;
                reg counter of Word<4>;
                counter <= counter + 1w4;
                out <= counter;
            }

            ext vip {
                incoming in of Word<4>;
            }

            vip.in <= counter.out;
        }
    ");

    let monitor = Box::new(Monitor::new());

    let mut nettle =
        Sim::new(&top)
            .ext("top.vip", monitor);

    nettle.reset();
    nettle.clock();
    nettle.clock();
    nettle.clock();
    nettle.clock();
}

#[test]
fn ifs() {
    let top = parse_top("
        top {
            outgoing out of Word<8>;
            incoming in of Word<8>;

            out <= if in {
                42w8
            } else {
                100w8
            };
        }
    ");

    let mut nettle = Sim::new(&top);

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
            node x of Bit;
            reg r of Bit reset false;
            x <= true;
            r <= r;

            reg a of Word<32> reset 2w32;
            reg b of Word<32> reset 3w32;
            a <= a;
            b <= b;

            node p of Bit;
            node q of Bit;
            p <= true;
            q <= false;
        }
    ");

    let mut nettle = Sim::new(&buffer);
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

#[test]
fn test_nets() {
    let top = parse_top("
        top {
            incoming in of Bit;
            reg r of Bit;
            outgoing out of Bit;
            r <= in;
            out <= r;
        }
    ");

    let nets = top.nets();
    assert_eq!(nets.len(), 2);

    let drivers: BTreeSet<Path> = nets
        .iter()
        .map(|net| net.driver().into())
        .collect();

    let expected_drivers: BTreeSet<Path> = vec!["top.r.val", "top.in"]
        .into_iter()
        .map(|path| path.into())
        .collect();

    assert_eq!(drivers, expected_drivers);

    let triangle_numbers_top = parse_top("
        top {
            node out of Word<32>;
            reg sum of Word<32> reset 0w32;
            mod counter {
                node out of Word<32>;
                reg c of Word<32> reset 1w32;
                c <= c + 1w4;
                out <= c;
            }
            out <= sum;
            sum <= sum + counter.out;
        }
    ");

    let triangle_numbers_nets = triangle_numbers_top.nets();
    assert_eq!(triangle_numbers_nets.len(), 4);
}
