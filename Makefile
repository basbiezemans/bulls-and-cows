.PHONY: test

# update the shell environment
update:
	eval $(opam config env)

build:
	dune build

# Show number of tests
test:
	dune test --force

# Quiet test run (default)
qtest:
	dune test

run:
	dune exec bulls-and-cows

all: build qtest run