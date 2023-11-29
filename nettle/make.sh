#!/bin/bash
riscv64-unknown-elf-gcc -march=rv32i -mabi=ilp32 -nostdlib main.s -o main
riscv64-unknown-elf-objcopy -O binary -j .text --set-start 0 main ./riscv/prog.bin
