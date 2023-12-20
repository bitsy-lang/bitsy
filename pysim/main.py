import pysim

monitor = pysim.Ext.monitor("top.monitor")
sim = pysim.Sim.load_from("Top.bitsy", "Top", [monitor])

sim.reset()
for x in range(10):
    sim.clock()
    print('top.counter =', sim.peek("top.counter"))

print()
sim.poke("top.counter", pysim.Word(0, 7))
print('top.counter =', sim.peek("top.counter"))

for x in range(10):
    sim.clock()
    print('top.counter =', int(sim.peek("top.counter")))
