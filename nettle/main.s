    .text
    .globl _start

_start:
    li gp, 0
    li t1, 0
    li t2, 1

.loop:
    li t1, 0xffffffff
    sw t1, 0(gp)
    addi gp, gp, 4
    add t3, t1, t2
    mv t1, t2
    mv t2, t3
    j      .loop
