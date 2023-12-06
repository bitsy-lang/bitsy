import pysim

monitor = pysim.Ext.monitor("Top.monitor")
sim = pysim.Sim.load_from("Top.ntl", [monitor])

sim.reset()
for x in range(10):
    sim.clock()
    print(sim.peek("Top.counter"))
