.PHONY: all clean doc

OCAMLBUILD := ocamlbuild -use-ocamlfind -cflags "-g" -lflags "-g" -classic-display
MAIN       := PPrint

all:
	$(OCAMLBUILD) $(MAIN).native

clean:
	rm -f *~ $(MAIN).native
	rm -f html
	$(OCAMLBUILD) -clean

doc: all
	@rm -rf html
	@mkdir html
	ocamlfind ocamldoc \
	  -html \
	  -I _build \
	  -d html \
	  -charset utf8 \
	  *.{ml,mli}
