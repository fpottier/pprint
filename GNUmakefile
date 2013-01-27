include Makefile

.PHONY: mezzo archive export

# --------------------------------------------------------------------------------

# [make mezzo] copies the pprint library into the Mezzo source tree.

MEZZO := $(HOME)/dev/mezzo/src/pprint

mezzo: all
	rm -rf $(MEZZO)
	mkdir $(MEZZO)
	cp -f README AUTHORS LICENSE *.ml *.mli $(MEZZO)

# --------------------------------------------------------------------------------

# [make archive] builds an archive of everything.

DATE     := $(shell /bin/date +%Y%m%d)
BASE     := pprint
ARCHIVE  := $(BASE)-$(DATE)

archive: all doc
	rm -rf $(ARCHIVE) $(ARCHIVE).tar.gz
	mkdir $(ARCHIVE) && cp README AUTHORS LICENSE $(ARCHIVE)
	mkdir $(ARCHIVE)/src && cp *.{ml,mli} Makefile $(ARCHIVE)/src
	tar cvfz $(ARCHIVE).tar.gz $(ARCHIVE)

# --------------------------------------------------------------------------------

# [make export] copies the archive to the Web site.

SERVER := yquem.inria.fr
WEBDIR := public_html/$(BASE)

export: archive
	scp $(ARCHIVE).tar.gz $(SERVER):$(WEBDIR)
	ssh $(SERVER) "bash -c 'cd $(WEBDIR) && /bin/ln -sf $(ARCHIVE).tar.gz $(BASE).tar.gz'"
