.PHONY: all clean doc test bench

all:
	dune build -p pprint

clean:
	dune clean

doc:
	dune build @doc
	@echo You can find the documentation in _build/default/_doc/_html/index.html

test:
	dune runtest

bench:
	dune build ./src/PPrintBench.exe
	time dune exec ./src/PPrintBench.exe
