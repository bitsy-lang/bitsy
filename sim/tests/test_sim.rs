use sim::Circuit;
use sim::Expr;
use sim::Value;
use sim::Loc;
use sim::Sim;

#[test]
fn test_sim() {
    let mut circuit = Circuit::new();
    let top_foo = circuit.add_terminal("top.foo");
    let top_out = circuit.add_terminal("top.out");
    let top_r = circuit.add_register("top.r");

    circuit.connect(top_foo.clone(), Expr::Lit(Loc::unknown(), Value::Word(8, 3)));
    circuit.latch(top_r.clone(), Expr::Net(Loc::unknown(), top_foo.net_id()));
    circuit.connect(top_out.clone(), Expr::Net(Loc::unknown(), top_r.val().net_id()));


    let mut sim = Sim::new(circuit);
    assert_eq!(sim.peek_net(top_r.set().net_id()), Value::Word(8, 3));
    sim.clock();
    assert_eq!(sim.peek_net(top_r.val().net_id()), sim.peek_net(top_r.set().net_id()));
    assert_eq!(sim.peek_net(top_out.net_id()), sim.peek_net(top_r.val().net_id()));
}
