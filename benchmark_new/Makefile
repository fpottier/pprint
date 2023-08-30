.PHONY: all
all:
	dune build @all

.PHONY: clean
clean:
	git clean -fX .

.PHONY: bench
bench:
	dune exec ./main.exe -- time alloc samples -quota 10

.PHONY: once
once:
	dune exec ./main.exe -- time alloc -quota 1x
