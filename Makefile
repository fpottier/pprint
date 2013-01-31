.PHONY: all install clean doc test

include Makefile.arch
OCAMLBUILD := ocamlbuild -use-ocamlfind -cflags "-g" -lflags "-g" -classic-display
OCAMLFIND  := ocamlfind
DOCDIR     := doc
MAIN       := PPrintTest
TO_BUILD   := PPrintLib.cma PPrintLib.cmxa
TO_INSTALL := META \
	$(patsubst %,_build/%,$(TO_BUILD)) \
	_build/PPrintLib.a \
	$(wildcard _build/*.cmx) $(wildcard _build/*.cmi)

all:
	$(OCAMLBUILD) $(TO_BUILD)

install: all
	$(OCAMLFIND) install PPrint $(TO_INSTALL)

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
	$(OCAMLBUILD) $(MAIN).native
	./$(MAIN).native
