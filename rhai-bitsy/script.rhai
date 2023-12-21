let p = package_load("https://raw.githubusercontent.com/bitsy-lang/bitsy/master/bitsy/examples/counter.bitsy");

let moddefs = package_moddefs(p);
print(moddefs);

for (moddef, i) in moddefs {
    print(moddef);
}

let sim = Sim(p, "Counter");
sim.reset();
for i in 0..20 {
    let v = sim.peek("top.out");
    print(v);
    sim.clock();
}
