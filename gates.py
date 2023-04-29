def Not(a):
    return not a


def And(a, b):
    return a and b


def Mux(sel, a, b):
    if sel:
        return a
    else:
        return b
