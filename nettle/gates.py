def Not(a):
    return not a

def And(a, b):
    return a and b

def Xor(a, b):
    return (a and not b) or (b and not a)

def Mux(sel, a, b):
    if sel:
        return a
    else:
        return b

osc = False

def Osc():
    global osc
    osc = not osc
    return osc
