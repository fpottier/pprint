.PHONY: all clean doc test

OCAMLBUILD := ocamlbuild -use-ocamlfind -cflags "-g" -lflags "-g" -classic-display
DOCDIR     := doc
MAIN       := PPrintTest

all:
	$(OCAMLBUILD) $(MAIN).native

clean:
	rm -f *~ $(MAIN).native
	rm -rf doc
	$(OCAMLBUILD) -clean

doc: all
	@rm -rf $(DOCDIR)
	@mkdir $(DOCDIR)
	ocamlfind ocamldoc \
	  -html \
	  -I _build \
	  -d $(DOCDIR) \
	  -charset utf8 \
	  PPrintRenderer.ml *.mli PPrint.ml

test: all
	./$(MAIN).native
