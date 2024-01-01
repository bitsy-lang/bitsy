build:
	cargo check --manifest-path bitsy/Cargo.toml
	cargo check --manifest-path sim/Cargo.toml
	cargo check --manifest-path rhai-bitsy/Cargo.toml
	cargo check --manifest-path pybitsy/Cargo.toml
	cargo check --manifest-path riscv-bitsy/Cargo.toml
#	make -f pybitsy/Makefile

test:
	cargo test --manifest-path bitsy/Cargo.toml
	cargo test --manifest-path sim/Cargo.toml
	cargo test --manifest-path rhai-bitsy/Cargo.toml
	cargo test --manifest-path pybitsy/Cargo.toml
	cargo test --manifest-path riscv-bitsy/Cargo.toml

install:
	cargo install --path bitsy
