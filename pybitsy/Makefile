

develop:
	maturin develop

build:
	maturin build

clean:
	cargo clean

ROOT := $(shell git rev-parse --show-toplevel)

isolated-build:
	@echo $(ROOT)
	podman run --rm -it -v $(ROOT):/io --workdir=/io/pybitsy ghcr.io/pyo3/maturin build --release

publish:
	@echo $(ROOT)
	podman run --rm -it -v $(ROOT):/io --workdir=/io/pybitsy ghcr.io/pyo3/maturin publish
