import pysim

monitor = pysim.Ext.monitor("top.monitor")
sim = pysim.Sim.load_from("Top.bitsy", "Top", [monitor])

sim.reset()
for x in range(10):
    sim.clock()
    print(sim.peek("top.counter"))
