    .text
    .globl _start

_start:
    li gp, 0x10000
    li sp, 0x100
    li t0, 0x0

.loop:
    lbu t1, 0(t0)
    beqz t1, .halt
    mv a0, t1
    call putc
    addi t0, t0, 1
    j .loop

.halt:
    ebreak
    j .halt

putc:
    sb a0, 0(gp)
    jalr zero, 0(ra)
