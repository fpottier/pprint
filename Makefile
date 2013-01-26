.PHONY: all clean

OCAMLBUILD := ocamlbuild -use-ocamlfind -cflags "-g" -lflags "-g" -classic-display
MAIN       := PPrint

all:
	$(OCAMLBUILD) $(MAIN).native

clean:
	rm -f *~ $(MAIN).native
	$(OCAMLBUILD) -clean

