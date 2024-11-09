.PHONY: build
build:
	dune build

.PHONY: test
test:
	dune runtest

.PHONY: setup
setup:
	opam install . --deps-only --with-test --with-dev-setup
