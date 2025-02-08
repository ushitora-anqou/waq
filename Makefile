.PHONY: build
build:
	dune build

.PHONY: test
test:
	dune runtest

.PHONY: flake-update
flake-update:
	nix flake update opam-nix waq-external-repo
