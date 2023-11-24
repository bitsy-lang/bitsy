use super::*;

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
    let buffer = parse_top("
        top {
            incoming in of Word<1>;
            reg r of Word<1>;
            outgoing out of Word<1>;
            r <= in;
            out := r;
        }
    ");

    let mut nettle = Sim::new(&buffer);

    nettle.poke("top.in", true.into());
    dbg!(&nettle.peek("top.r"));
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
            out := counter;
            counter <= counter + 1w4;
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
                reg counter of Word<32> reset 1w32;
                out := counter;
                counter <= counter + 1w32;
            }
            out := sum;
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
                out := counter;
            }

            ext vip {
                incoming in of Word<4>;
            }

            vip.in := counter.out;
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

            out := if in {
                42w8
            } else {
                100w8
            };
        }
    ");

    let mut nettle = Sim::new(&top);

    nettle.poke("top.in", true.into());
    assert_eq!(nettle.peek("top.out"), Value::Word(8, 42));
    nettle.poke("top.in", false.into());
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
    let top = parse_top("
        top {
            node x of Word<1>;
            reg r of Word<1> reset 0w1;
            x := 1w1;
            r <= r;

            reg a of Word<32> reset 2w32;
            reg b of Word<32> reset 3w32;
            a <= a;
            b <= b;

            node p of Word<1>;
            node q of Word<1>;
            p := 1w1;
            q := 0w1;
        }
    ");

    let mut nettle = Sim::new(&top);
    nettle.reset();

    let tests = vec![
        ("top.x", true.into()),
        ("1w8", Value::Word(8, 1)),
        ("(top.a + top.b)", Value::Word(32, 5)),
        ("(top.b - top.a)", Value::Word(32, 1)),
        ("(1w1 + 1w1)", Value::Word(1, 0)),
        ("cat(1w1, 1w1)", Value::Word(2, 0b11)),
        ("3w4[3..1]", Value::Word(2, 0b01)),
        ("3w4[4..0]", Value::Word(4, 0b0011)),
        ("7w4[4..0]", Value::Word(4, 0b0111)),
        ("6w4[4..1]", Value::Word(3, 0b11)),
        ("7w4[3w2]", Value::Word(1, 0)),
        ("7w4[2w2]", Value::Word(1, 1)),
        ("7w4[1w2]", Value::Word(1, 1)),
        ("7w4[0w2]", Value::Word(1, 1)),
//        "(a && b)",
//        "(a || b)",
        ("(top.a == top.b)".into(), false.into()),
        ("(top.b < top.a)".into(), false.into()),
        ("(top.a != top.b)".into(), true.into()),
        ("(!top.x)".into(), false.into()),
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
            incoming in of Word<1>;
            reg r of Word<1>;
            outgoing out of Word<1>;
            r <= in;
            out := r;
        }
    ");

    let top_nets = nets(&top);
    assert_eq!(top_nets.len(), 2);

    let drivers: BTreeSet<Path> = top_nets
        .iter()
        .map(|net| net.driver().into())
        .collect();

    let expected_drivers: BTreeSet<Path> = vec!["top.r", "top.in"]
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
                out := c;
            }
            out := sum;
            sum <= sum + counter.out;
        }
    ");

    let triangle_numbers_nets = nets(&triangle_numbers_top);
    assert_eq!(triangle_numbers_nets.len(), 4);
}

#[test]
fn test_node() {
    let top = parse_top("
        top {
            outgoing out of Word<1>;
            node n of Word<1>;
            n := 1w1;
        }
    ");

    let nettle = Sim::new(&top);
    assert_eq!(nettle.peek("top.n"), Value::Word(1, 1));
}

#[test]
fn test_circuit() {
    let top = parse_top("
        top {
            outgoing out of Word<8>;
            incoming in of Word<8>;
            mod foo {
                incoming a of Word<1>;
                outgoing z of Word<1>;
                reg r of Word<1>;

                r <= r + 1w1;
                mod bar {
                }
                mod baz {
                    reg c of Word<1>;
                    c <= 0w1;
                }
            }

            mod quux {
                outgoing out of Word<1>;
                out := 1w1;
            }
        }
    ");

    assert_eq!(
        top.modules(),
        vec![
            "top".into(),
            "top.foo".into(),
            "top.foo.bar".into(),
            "top.foo.baz".into(),
            "top.quux".into(),
        ],
    );

    assert_eq!(top.component("top.foo.baz".into()).unwrap().name(), "baz");

//    assert_eq!(
//        top.visible_terminals(),
//        vec![
//            "out".into(),
//            "in".into(),
//            "foo.a".into(),
//            "foo.z".into(),
//        ],
//    );

    let baz = top.component("top.foo.baz".into()).unwrap();
    assert_eq!(baz.wires().len(), 1);
    assert_eq!(baz.wires()[0].2, WireType::Latch);
}

#[test]
fn test_check() {
    let top = parse_top("
        top {
            outgoing out of Word<8>;
            incoming in of Word<8>;

            out := in;
        }
    ");

    top.check().unwrap();

    /*
    let top2 = parse_top("
        top {
            outgoing out of Word<8>;
            incoming in of Word<8>;
            node n of Word<1>;
            in := n;
        }
    ");

    dbg!(top2.check());
    */
}
