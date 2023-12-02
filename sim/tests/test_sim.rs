use sim::Circuit;
use sim::Expr;
use sim::Loc;

#[test]
fn test_sim() {
    let mut circuit = sim::Circuit::new();
    let top_in = circuit.add_terminal("top.in");
    let top_out = circuit.add_terminal("top.out");
    let top_r = circuit.add_register("top.r");
    circuit.latch(top_r, Expr::Net(Loc::unknown(), top_in.net_id()));
}
