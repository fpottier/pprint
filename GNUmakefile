include Makefile

.PHONY: archive export headers

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
	mkdir $(ARCHIVE) && cp README AUTHORS LICENSE $(ARCHIVE)
	mkdir $(ARCHIVE)/src && cp *.ml *.mli Makefile META $(ARCHIVE)/src
	echo version = \"$(DATE)\" >> $(ARCHIVE)/src/META
	tar cvfz $(ARCHIVE).tar.gz $(ARCHIVE)

# --------------------------------------------------------------------------------

# [make export] copies the archive to the Web site.

SERVER := yquem.inria.fr
WEBDIR := public_html/$(BASE)

export: archive
	scp $(ARCHIVE).tar.gz $(SERVER):$(WEBDIR)
	ssh $(SERVER) "bash -c 'cd $(WEBDIR) && /bin/ln -sf $(ARCHIVE).tar.gz $(BASE).tar.gz'"
	scp -r doc $(SERVER):$(WEBDIR)

