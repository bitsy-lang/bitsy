use sim::Circuit;
use sim::Comb;
use sim::Instr;
use sim::Value;
use sim::Sim;

#[test]
fn test_sim() {
    let mut circuit = Circuit::new();
    let a = circuit.add_terminal();
    let b = circuit.add_terminal();

    circuit.add_comb(
        vec![],
        vec![a],
        vec![Instr::Const(Value::Word(8, 3))],
    );

    circuit.add_comb(
        vec![a],
        vec![b],
        vec![
            Instr::Const(Value::Word(8, 1)),
            Instr::Add(0, 1),
        ],
    );

    let mut sim = Sim::new(circuit);
    assert_eq!(sim.peek(a), Value::Word(8, 3));
    assert_eq!(sim.peek(b), Value::Word(8, 4));
}

#[test]
fn test_regs() {
    let mut circuit = Circuit::new();
    let r = circuit.add_register(Value::Word(4, 0));

    circuit.add_comb(
        vec![r.val()],
        vec![r.set()],
        vec![
            Instr::Const(Value::Word(4, 1)),
            Instr::Add(0, 1),
        ],
    );

    let mut sim = Sim::new(circuit);
    sim.reset();

    for i in 0..15 {
        assert_eq!(sim.peek(r.val()), Value::Word(4, i));
        assert_eq!(sim.peek(r.set()), Value::Word(4, i + 1));
        sim.clock();
    }

    assert_eq!(sim.peek(r.val()), Value::Word(4, 15));
    assert_eq!(sim.peek(r.set()), Value::Word(4, 0));
}

#[test]
fn test_eval() {
    let mut circuit = Circuit::new();
    let a = circuit.add_terminal();
    let b = circuit.add_terminal();
    let c = circuit.add_terminal();
    let d = circuit.add_terminal();

    let comb = Comb::new(
        vec![a],
        vec![b],
        vec![
            Instr::Const(Value::Word(1, 0)),
        ],
    );

    assert_eq!(
        comb.eval(&[Value::Word(1, 1)]),
        vec![Value::Word(1, 0)],
    );

    let comb = Comb::new(
        vec![a],
        vec![b],
        vec![
            Instr::Load(0),
        ],
    );

    assert_eq!(
        comb.eval(&[Value::Word(1, 1)]),
        vec![Value::Word(1, 1)],
    );

    let comb = Comb::new(
        vec![a, b],
        vec![c],
        vec![
            Instr::Add(0, 1),
        ],
    );

    assert_eq!(
        comb.eval(&[Value::Word(8, 20), Value::Word(8, 22)]),
        vec![Value::Word(8, 42)],
    );

    let comb = Comb::new(
        vec![a, b],
        vec![c, d],
        vec![
            Instr::Load(0),
        ],
    );

    assert_eq!(
        comb.eval(&[Value::Word(8, 1), Value::Word(8, 99)]),
        vec![Value::Word(8, 99), Value::Word(8, 1)],
    );

    let comb = Comb::new(
        vec![a],
        vec![b],
        vec![
            Instr::Neg(0),
        ],
    );

    assert_eq!(
        comb.eval(&[Value::Word(4, 1)]),
        vec![Value::Word(4, 15)],
    );


    let comb = Comb::new(
        vec![c, a, b],
        vec![d],
        vec![
            Instr::Mux(0, 1, 2),
        ],
    );

    assert_eq!(
        comb.eval(&[Value::Word(1, 1), Value::Word(8, 44), Value::Word(8, 99)]),
        vec![Value::Word(8, 44)],
    );
    assert_eq!(
        comb.eval(&[Value::Word(1, 0), Value::Word(8, 44), Value::Word(8, 99)]),
        vec![Value::Word(8, 99)],
    );

    let comb = Comb::new(
        vec![c, a, b],
        vec![d],
        vec![
            Instr::Neg(2),
            Instr::Add(1, 2),
            Instr::Add(1, 3),
            Instr::Mux(0, 4, 5),
        ],
    );

    assert_eq!(
        comb.eval(&[Value::Word(1, 1), Value::Word(8, 99), Value::Word(8, 44)]),
        vec![Value::Word(8, 99 + 44)],
    );
    assert_eq!(
        comb.eval(&[Value::Word(1, 0), Value::Word(8, 99), Value::Word(8, 44)]),
        vec![Value::Word(8, 99 - 44)],
    );

    let comb = Comb::new(
        vec![a],
        vec![b],
        vec![],
    );
    assert_eq!(
        comb.eval(&[Value::Word(8, 42)]),
        vec![Value::Word(8, 42)],
    );
}
