build:
	cargo check --manifest-path bitsy/Cargo.toml
	cargo check --manifest-path sim/Cargo.toml
	cargo check --manifest-path rhai-bitsy/Cargo.toml
#	make -f pybitsy/Makefile

install:
	cargo install --path bitsy
