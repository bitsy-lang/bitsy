import pysim

sim = pysim.Sim.load_from("Top.ntl")
sim.reset()
for x in range(10):
    sim.clock()

    print(sim.peek("Top.counter"))

