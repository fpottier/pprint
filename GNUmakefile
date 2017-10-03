# -------------------------------------------------------------------------

# This Makefile is not distributed.

SHELL := bash
export CDPATH=

# --------------------------------------------------------------------------------

.PHONY: all install uninstall reinstall clean doc test package export headers

all install uninstall reinstall clean doc test:
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

# [make package] builds a package of everything.

DATE     := $(shell /bin/date +%Y%m%d)
BASE     := pprint
PACKAGE  := $(BASE)-$(DATE)
MD5      := $(shell if [ `which md5` ] ; then echo md5 ; else echo md5sum ; fi)

package: headers all doc
	rm -rf $(PACKAGE) $(PACKAGE).tar.gz
	mkdir $(PACKAGE) && cp README AUTHORS LICENSE CHANGES $(PACKAGE)
	mkdir $(PACKAGE)/src && cp src/*.ml src/*.mli src/*.mllib src/Makefile src/META $(PACKAGE)/src
	echo version = \"$(DATE)\" >> $(PACKAGE)/src/META
	tar -c -v -z -f $(PACKAGE).tar.gz -X .exclude $(PACKAGE)
	$(MD5) $(PACKAGE).tar.gz

# --------------------------------------------------------------------------------

# [make export] copies the package to the Web site.

SERVER := yquem.inria.fr
WEBDIR := public_html/$(BASE)

export: package
	scp $(PACKAGE).tar.gz $(SERVER):$(WEBDIR)
	ssh $(SERVER) "bash -c 'cd $(WEBDIR) && /bin/ln -sf $(PACKAGE).tar.gz $(BASE).tar.gz && rm -rf doc'"
	scp -r doc $(SERVER):$(WEBDIR)
