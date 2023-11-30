    .text
    .globl _start

_start:
    li gp, 0
    li a0, 3
    li a1, 4
    call multiply
.end:
    sw a0, 0(gp)
    j .end

multiply:
    li t1, 0
.start_multiply:
    beqz a0, .end_multiply
    addi a0, a0, -1
    add t1, t1, a1
    j .start_multiply
.end_multiply:
    ret
