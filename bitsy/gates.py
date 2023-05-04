def Const2():
    return 2


def Const3():
    return 3


def Not(a):
    return not a


def And(a, b):
    return a and b


def Add(a, b):
    return a + b


def Mul(a, b):
    return a * b


def Mux(sel, a, b):
    if sel:
        return a
    else:
        return b
