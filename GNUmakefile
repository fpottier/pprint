include Makefile

# ------------------------------------------------------------------------------

# This Makefile is not distributed.

SHELL := bash
export CDPATH=

# ------------------------------------------------------------------------------

.PHONY: headers package check export tag opam

# ------------------------------------------------------------------------------

# [make headers] updates the headers.

HEADACHE := headache
HEADER   := header

headers:
	for f in src/PPrint*.{ml,mli} ; do \
	  $(HEADACHE) -h $(HEADER) $$f ; \
	done

# ------------------------------------------------------------------------------

# [make package] builds a package of everything.

DATE     := $(shell /bin/date +%Y%m%d)
BASE     := pprint
PACKAGE  := $(BASE)-$(DATE)
TARBALL  := $(shell pwd)/$(PACKAGE).tar.gz

package: all doc
	rm -rf $(PACKAGE) $(TARBALL)
	mkdir $(PACKAGE) && cp README.md AUTHORS LICENSE CHANGES.md Makefile $(PACKAGE)
	mkdir $(PACKAGE)/src
	cp -r src/*.ml src/*.mli src/*.mllib src/Makefile src/META src/doc $(PACKAGE)/src
	echo version = \"$(DATE)\" >> $(PACKAGE)/src/META
	tar -c -v -z -f $(TARBALL) -X .exclude $(PACKAGE)

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

export:
	scp $(TARBALL) $(SERVER):$(WEBDIR)
	ssh $(SERVER) "bash -c 'cd $(WEBDIR) && /bin/ln -sf $(PACKAGE).tar.gz $(BASE).tar.gz && rm -rf doc'"
	scp -r src/doc $(SERVER):$(WEBDIR)

# -------------------------------------------------------------------------

# Creating a git tag.

tag:
	git tag -a $(DATE) -m "Release $(DATE)."

# -------------------------------------------------------------------------

# Utilities.

MD5SUM  := $(shell if command -v md5 >/dev/null 2>/dev/null ; \
                   then echo "md5 -r" ; else echo md5sum ; fi)

# -------------------------------------------------------------------------

# Updating the opam package.

# This entry assumes that "make package" and "make export" have been
# run on the same day.

OPAM := $(HOME)/dev/opam-repository
CSUM  = $(shell $(MD5SUM) $(BASE)-$(DATE).tar.gz | cut -d ' ' -f 1)

opam:
# Update my local copy of the opam repository.
	@ echo "Updating local opam repository..."
	@ cd $(OPAM) && \
	  git fetch upstream && \
	  git merge upstream/master
# Create a new package, based on the last one.
	@ echo "Creating a new package description $(BASE)-$(DATE)..."
	@ cd $(OPAM)/packages/$(BASE) && \
	  cp -r `ls | grep $(BASE) | tail -1` $(BASE).$(DATE)
# Update the file "url".
	@ cd $(OPAM)/packages/$(BASE)/$(BASE).$(DATE) && \
	  rm url && \
	  echo 'archive: "http://gallium.inria.fr/~fpottier/$(BASE)/$(BASE)-$(DATE).tar.gz"' >> url && \
	  echo 'checksum: "$(CSUM)"' >> url
# Copy the file "opam" from our repository to opam's.
	@ cp -f opam $(OPAM)/packages/$(BASE)/$(BASE).$(DATE)
# Prepare a commit.
	@ echo "Preparing a new commit..."
	@ cd $(OPAM)/packages/$(BASE) && \
	  git add $(BASE).$(DATE) && \
	  git status
# Ask for review.
	@ echo "If happy, please run:"
	@ echo "  cd $(OPAM)/packages/$(BASE) && git commit -a && git push && firefox https://github.com/"
	@ echo "and issue a pull request."
