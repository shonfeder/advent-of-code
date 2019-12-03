.PHONY: default
default:
	dune build

.PHONY: deps
deps:
	-dune build
	git add dune-project aoc_2019.opam
	git commit -m "Updating deps"
	opam install . --deps-only

.PHONY: test
test:
	dune runtest

.PHONY: run
run:
	dune exec aoc_2019
