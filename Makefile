all:
	dune build

install:
	dune build @install && dune install

test:
	dune runtest

clean:
	dune clean

format:
	dune build @fmt --auto-promote

odig-stdlib:
	tar -cC `odig cache path`/html highlight.pack.js _odoc-theme gospel/Gospelstdlib | tar xv -C docs/static

.PHONY: all gospel clean format test odig-stdlib
