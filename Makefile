.PHONY: test

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