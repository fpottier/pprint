include Makefile

.PHONY: archive export headers billet bench

# --------------------------------------------------------------------------------

# [make headers] updates the headers.

HEADACHE := headache
HEADER   := header

headers:
	for f in *.ml *.mli ; do \
	  $(HEADACHE) -h $(HEADER) $$f ; \
	done

# --------------------------------------------------------------------------------

# [make archive] builds an archive of everything.

DATE     := $(shell /bin/date +%Y%m%d)
BASE     := pprint
ARCHIVE  := $(BASE)-$(DATE)

archive: headers all doc
	rm -rf $(ARCHIVE) $(ARCHIVE).tar.gz
	mkdir $(ARCHIVE) && cp README AUTHORS LICENSE CHANGES $(ARCHIVE)
	mkdir $(ARCHIVE)/src && cp *.ml *.mli *.mllib Makefile META $(ARCHIVE)/src
	echo version = \"$(DATE)\" >> $(ARCHIVE)/src/META
	tar -c -v -z -f $(ARCHIVE).tar.gz -X .exclude $(ARCHIVE)
	md5sum $(ARCHIVE).tar.gz

# --------------------------------------------------------------------------------

# [make export] copies the archive to the Web site.

SERVER := yquem.inria.fr
WEBDIR := public_html/$(BASE)

export: archive
	scp $(ARCHIVE).tar.gz $(SERVER):$(WEBDIR)
	ssh $(SERVER) "bash -c 'cd $(WEBDIR) && /bin/ln -sf $(ARCHIVE).tar.gz $(BASE).tar.gz && rm -rf doc'"
	scp -r doc $(SERVER):$(WEBDIR)

# --------------------------------------------------------------------------------

# [make billet] creates the blog entry.

billet: billet.html

%.html: %.markdown
	pandoc -s $< -c style.css > $@

# --------------------------------------------------------------------------------

# [make bench] runs a performance benchmark.

OCAMLBUILD := ocamlbuild -use-ocamlfind -cflags "-g" -lflags "-g" -classic-display

bench: all
	$(OCAMLBUILD) -tag use_unix PPrintBench.native
	time ./PPrintBench.native

