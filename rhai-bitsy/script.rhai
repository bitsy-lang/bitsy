let p = Package("https://raw.githubusercontent.com/bitsy-lang/bitsy/master/bitsy/examples/counter.bitsy");

let moddefs = p.moddefs();
print("Module definitions:");
for moddef in moddefs {
    print("  - " + moddef);
}
print();

let sim = p.sim("Counter");

sim.reset();
for i in 0..20 {
    let v = sim.peek("top.out");
    print(v);
    sim.clock();
}
