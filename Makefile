build:
	cargo check --manifest-path bitsy/Cargo.toml
	cargo check --manifest-path sim/Cargo.toml
	cargo check --manifest-path riscv-bitsy/Cargo.toml
#	make -f pybitsy/Makefile

test:
	make -C bitsy test
	make -C sim test
	make -C riscv-bitsy test

install:
	cargo install --path bitsy
