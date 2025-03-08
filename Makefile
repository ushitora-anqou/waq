.PHONY: build
build:
	dune build

.PHONY: test
test:
	dune runtest

.PHONY: fmt
fmt:
	dune fmt
