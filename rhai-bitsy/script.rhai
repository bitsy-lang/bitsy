#!/usr/bin/env rhai-bitsy

let p = Package("top.bitsy");

let moddefs = p.moddefs();
print("Module definitions:");
for moddef in moddefs {
    print("  - " + moddef);
}
print();

let sim = p.sim("Top");

sim.reset();
sim.poke("top.in", word(0, 4));
for i in 0..20 {
    let v = sim.peek("top.out");
    print(v);
    sim.clock();
}
