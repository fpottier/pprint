# --------------------------------------------------------------------------------

.PHONY: all install clean doc test archive export headers

all install clean doc test:
	$(MAKE) -C src $@

# --------------------------------------------------------------------------------

# [make headers] updates the headers.

HEADACHE := headache
HEADER   := header

headers:
	for f in PPrint*.ml PPrint*.mli ; do \
	  $(HEADACHE) -h $(HEADER) $$f ; \
	done

# --------------------------------------------------------------------------------

# [make archive] builds an archive of everything.

DATE     := $(shell /bin/date +%Y%m%d)
BASE     := pprint
ARCHIVE  := $(BASE)-$(DATE)
MD5      := $(shell if [ `which md5` ] ; then echo md5 ; else echo md5sum ; fi)

archive: headers all doc
	rm -rf $(ARCHIVE) $(ARCHIVE).tar.gz
	mkdir $(ARCHIVE) && cp README AUTHORS LICENSE CHANGES $(ARCHIVE)
	mkdir $(ARCHIVE)/src && cp src/*.ml src/*.mli src/*.mllib src/Makefile src/META $(ARCHIVE)/src
	echo version = \"$(DATE)\" >> $(ARCHIVE)/src/META
	tar -c -v -z -f $(ARCHIVE).tar.gz -X .exclude $(ARCHIVE)
	$(MD5) $(ARCHIVE).tar.gz

# --------------------------------------------------------------------------------

# [make export] copies the archive to the Web site.

SERVER := yquem.inria.fr
WEBDIR := public_html/$(BASE)

export: archive
	scp $(ARCHIVE).tar.gz $(SERVER):$(WEBDIR)
	ssh $(SERVER) "bash -c 'cd $(WEBDIR) && /bin/ln -sf $(ARCHIVE).tar.gz $(BASE).tar.gz && rm -rf doc'"
	scp -r doc $(SERVER):$(WEBDIR)
