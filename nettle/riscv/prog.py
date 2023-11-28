f = open('prog.bin', 'wb')

def byte(n):
    return n.to_bytes(1, byteorder='little')

def write_instr(instr):
    f.write(byte(instr & 0xff))
    f.write(byte((instr >> 8) & 0xff))
    f.write(byte((instr >> 16) & 0xff))
    f.write(byte((instr >> 24) & 0xff))

write_instr(0x02a00093) # addi x1, x0, 42
write_instr(0x06102223) # sw x1, 100(x0)
write_instr(0xff9ff06f) # jal zero, -8
