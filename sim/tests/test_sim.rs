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
    let r = circuit.add_register();

    circuit.add_comb(
        vec![a.clone()],
        vec![b.clone()],
        vec![],
    );

    let mut sim = Sim::new(circuit);
    assert_eq!(sim.peek_net(r.set().net_id()), Value::Word(8, 3));
    sim.clock();
    assert_eq!(sim.peek_net(r.val().net_id()), sim.peek_net(r.set().net_id()));
    assert_eq!(sim.peek_net(b.net_id()), sim.peek_net(r.val().net_id()));
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
}
