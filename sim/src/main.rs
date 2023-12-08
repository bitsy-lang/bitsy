use sim::*;
use sim::mlir;

fn main() {
    let mut circuit = Circuit::new();
    let a = circuit.add_terminal();
    let b = circuit.add_terminal();
    let c = circuit.add_terminal();
    let d = circuit.add_terminal();

    let comb = Comb::new(
        vec![a, b],
        vec![c, d],
        vec![
            Instr::Load(0),
        ],
    );

    comb.mlir();
}
