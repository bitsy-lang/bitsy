    .text
    .globl _start

_start:
    li gp, 0
    li t0, 0
    li t1, 1

.loop:
    sw t0, 0(gp)
    addi gp, gp, 4
    add t2, t0, t1
    mv t0, t1
    mv t1, t2
    j .loop
