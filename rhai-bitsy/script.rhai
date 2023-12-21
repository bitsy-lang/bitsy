let p = package_load("top.bitsy");

let moddefs = package_moddefs(p);
print(moddefs);

for (moddef, i) in moddefs {
    print(moddef);
}

let sim = Sim(p, "Top");
sim.reset();
sim.poke("top.in", word(2, 3));
sim.clock();
sim.clock();
let v = sim.peek("top.out");
print(v);
