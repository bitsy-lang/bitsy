use crate::Path;
use crate::load_package_from_string;
use crate::sim::{Sim, Value};
use crate::sim::ext::ExtInstance;
use crate::sim::ext::monitor::Monitor;
use std::collections::{BTreeSet, BTreeMap};

#[test]
fn test_nets() {
    let top = load_package_from_string("
        mod Top {
            incoming in of Word<1>;
            reg r of Word<1>;
            outgoing out of Word<1>;
            r <= in;
            out := r;
        }
    ").unwrap();
    let top = top.top("Top").unwrap();

    let top_nets = crate::sim::nets(&top);
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

    let triangle_numbers_top = load_package_from_string("
        mod Top {
            node out of Word<32>;
            reg sum of Word<32> reset 0w32;
            mod counter {
                outgoing out of Word<32>;
                reg c of Word<32> reset 1w32;
                c <= c + 1w32;
                out := c;
            }
            out := sum;
            sum <= sum + counter.out;
        }
    ").unwrap();
    let triangle_numbers_top = triangle_numbers_top.top("Top").unwrap();
    let triangle_numbers_nets = crate::sim::nets(&triangle_numbers_top);
    assert_eq!(triangle_numbers_nets.len(), 4);
}

/*
#[test]
fn test_eval() {
    let top = load_package_from_string("
        mod Top {
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
    ").unwrap();
    let top = top.top("Top").unwrap();

    let mut bitsy = Sim::new(&top);
    bitsy.reset();

    let tests = vec![
        ("top.x", true.into()),
        ("1w8", Value::Word(8, 1)),
        ("(top.a + top.b)", Value::Word(32, 5)),
        ("(top.b - top.a)", Value::Word(32, 1)),
        ("(1w1 + 1w1)", Value::Word(1, 0)),
        ("cat(1w1, 1w1)", Value::Word(2, 0b11)),
        ("sext(1w1, 4)", Value::Word(4, 0b1111)),
        ("sext(1w2, 4)", Value::Word(4, 0b0001)),
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
        assert_eq!(expr.eval(&bitsy), v, "{expr:?} does not equal {v:?}");
    }
}
*/

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
