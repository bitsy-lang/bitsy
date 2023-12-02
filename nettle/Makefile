#!/bin/bash

PROGRAMS = $(wildcard programs/*.s)
EXECUTABLES = $(patsubst programs/%.s, programs/%.exe, $(PROGRAMS))
BINS = $(patsubst programs/%, programs/%.bin, $(EXECUTABLES))

all: $(EXECUTABLES) $(BINS)

%.exe: %.s
	riscv64-unknown-elf-gcc -march=rv32i -mabi=ilp32 -nostdlib $< -o $@

%.bin: %
	riscv64-unknown-elf-objcopy -O binary -j .text --set-start 0 $< $@

clean:
	rm $(EXECUTABLES) $(BINS)
