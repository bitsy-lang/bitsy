    .text
    .globl _start

_start:
    li gp, 0x10000
    li t0, 0x0

.loop:
    lw t1, 0(t0)
    andi t1, t1, 0xff
    sw t1, 0(gp)
    addi t0, t0, 1

    j .loop

.halt:
    j .halt
