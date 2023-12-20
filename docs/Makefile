# Minimal makefile for Sphinx documentation
#

# You can set these variables from the command line, and also
# from the environment for the first two.
SPHINXOPTS    ?=
SPHINXBUILD   ?= sphinx-build
SOURCEDIR     = source
BUILDDIR      = build
EXAMPLES      = $(wildcard ../nettle/examples/*.bitsy)

auto: examples $(EXAMPLES) typesystem.pdf
	mkdir -p ./source/examples/
	cp ../bitsy/examples/*.bitsy ./source/examples/
	poetry run make html

.PHONY: auto clean examples push

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
