# Minimal makefile for Sphinx documentation
#

# You can set these variables from the command line, and also
# from the environment for the first two.
SPHINXOPTS    ?=
SPHINXBUILD   ?= sphinx-build
SOURCEDIR     = source
BUILDDIR      = build
EXAMPLES      = $(wildcard ../bitsy/examples/*.bitsy)

auto: examples $(EXAMPLES) typesystem.pdf rustdocs
	mkdir -p ./source/examples/
	cp ../bitsy/examples/*.bitsy ./source/examples/
	poetry run make html

.PHONY: auto clean examples push rustdocs

html: Makefile
	$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

clean:
	rm -rf ./source/examples
	rm -rf $(BUILDDIR)
	rm -rf ./latexbuild/

%.pdf: %.tex
	mkdir -p ./build/html
	mkdir -p ./latexbuild/
	pdflatex -output-directory=./latexbuild/ $<
	cp ./latexbuild/$@ $(BUILDDIR)/html/

push: auto
	rsync -azvhP $(BUILDDIR)/html/* digitalocean:/root/projects/bitsy/docs/build/html

rustdocs:
	cargo doc --manifest-path ../bitsy/Cargo.toml
	cp -r ../bitsy/target/doc/ $(BUILDDIR)/html/.
