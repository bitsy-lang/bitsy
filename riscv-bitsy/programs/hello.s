    .text
    .globl _start

_start:
    li gp, 0x10000  # 0
    li sp, 0x100    # 4
    li t0, 0x0      # 8

.loop:
    lw t1, 0(t0)    # 12
    beqz t1, .halt  # 16
    mv a0, t1       # 20
    call putc       # 24
    addi t0, t0, 1  # 28
    j .loop         # 32

.halt:
    ebreak          # 36
    j .halt         # 40

putc:
    andi a0, a0, 0xff   # 44
    sw a0, 0(gp)        # 48
    jalr zero, 0(ra)
