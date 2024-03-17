.PHONY: build
build:
	dune build

.PHONY: test
test:
	dune runtest
