include Makefile

# ------------------------------------------------------------------------------

# This Makefile is not distributed.

SHELL := bash
export CDPATH=

# ------------------------------------------------------------------------------

.PHONY: package check export headers

# ------------------------------------------------------------------------------

# [make headers] updates the headers.

HEADACHE := headache
HEADER   := header

headers:
	for f in PPrint*.ml PPrint*.mli ; do \
	  $(HEADACHE) -h $(HEADER) $$f ; \
	done

# ------------------------------------------------------------------------------

# [make package] builds a package of everything.

DATE     := $(shell /bin/date +%Y%m%d)
BASE     := pprint
PACKAGE  := $(BASE)-$(DATE)
TARBALL  := $(shell pwd)/$(PACKAGE).tar.gz
MD5      := $(shell if [ `which md5` ] ; then echo md5 ; else echo md5sum ; fi)

package: headers all doc
	rm -rf $(PACKAGE) $(TARBALL)
	mkdir $(PACKAGE) && cp README.md AUTHORS LICENSE CHANGES Makefile $(PACKAGE)
	mkdir $(PACKAGE)/src && cp src/*.ml src/*.mli src/*.mllib src/Makefile src/META $(PACKAGE)/src
	echo version = \"$(DATE)\" >> $(PACKAGE)/src/META
	tar -c -v -z -f $(TARBALL) -X .exclude $(PACKAGE)
	$(MD5) $(TARBALL)

# ------------------------------------------------------------------------------

# Checking the tarball that was created above.

check:
	@ echo "Checking the package ..."
# Create a temporary directory; extract, build, and install.
	@ TEMPDIR=`mktemp -d /tmp/pprint-test.XXXXXX` && { \
	echo "   * Extracting. " && \
	(cd $$TEMPDIR && tar xfz $(TARBALL)) && \
	echo "   * Compiling and installing." && \
	(cd $$TEMPDIR/$(PACKAGE) && make reinstall \
	) > $$TEMPDIR/install.log 2>&1 \
		|| (cat $$TEMPDIR/install.log; exit 1) && \
	echo "   * Uninstalling." && \
	(cd $$TEMPDIR/$(PACKAGE) && make uninstall \
	) > $$TEMPDIR/uninstall.log 2>&1 \
		|| (cat $$TEMPDIR/uninstall.log; exit 1) && \
	rm -rf $$TEMPDIR ; }
	@ echo "The package $(PACKAGE) seems ready for distribution!"

# ------------------------------------------------------------------------------

# [make export] copies the package to the Web site.

SERVER := yquem.inria.fr
WEBDIR := public_html/$(BASE)

export: package
	scp $(PACKAGE).tar.gz $(SERVER):$(WEBDIR)
	ssh $(SERVER) "bash -c 'cd $(WEBDIR) && /bin/ln -sf $(PACKAGE).tar.gz $(BASE).tar.gz && rm -rf doc'"
	scp -r doc $(SERVER):$(WEBDIR)
