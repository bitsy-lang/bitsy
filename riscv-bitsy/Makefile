ASMS := $(wildcard programs/*.s)
# BINS := $(wildcard programs/*.bin)
BINS := $(patsubst programs/%.s,build/%,$(ASMS))



all: build $(BINS)

build:
	mkdir -p build/

build/%.exe: programs/%.s
	riscv64-unknown-elf-gcc -march=rv32i -mabi=ilp32 $< programs/linker.ld -nostdlib -o $@

build/%: build/%.exe
	riscv64-unknown-elf-objcopy -O binary -j .text $< $@

clean:
	rm -rf build/

test:
	cargo test
