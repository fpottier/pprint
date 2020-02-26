include Makefile

# ------------------------------------------------------------------------------

# Private Makefile for package maintenance.

SHELL := bash
export CDPATH=

# ------------------------------------------------------------------------------

# The version number is automatically set to the current date,
# unless DATE is defined on the command line.
DATE     := $(shell /bin/date +%Y%m%d)

# The project's name and version number.
BASE     := pprint
PACKAGE  := $(BASE).$(DATE)

# The archive's URL (https).
ARCHIVE  := https://github.com/fpottier/$(BASE)/archive/$(DATE).tar.gz

# ------------------------------------------------------------------------------

.PHONY: headers export opam submit pin unpin

# ------------------------------------------------------------------------------

# [make headers] updates the headers.

HEADACHE := headache
HEADER   := header

headers:
	for f in src/PPrint*.{ml,mli} ; do \
	  $(HEADACHE) -h $(HEADER) $$f ; \
	done

# -------------------------------------------------------------------------

# Publish a release. (Remember to commit everything first!)

export:
# Check if everything has been committed.
	@ if [ -n "$$(git status --porcelain)" ] ; then \
	    echo "Error: there remain uncommitted changes." ; \
	    git status ; \
	    exit 1 ; \
	  else \
	    echo "Now making a release..." ; \
	  fi
# Create a git tag.
	@ git tag -a $(DATE) -m "Release $(DATE)."
# Upload. (This automatically makes a .tar.gz archive available on gitlab.)
	@ git push
	@ git push --tags

# -------------------------------------------------------------------------

# Updating the opam package.

# This entry assumes that "make package" and "make export" have been
# run on the same day.

# The "opam" file must have a "name" field that contains the package name.

opam:
	@ opam lint
	@ opam-publish prepare $(PACKAGE) $(ARCHIVE)

submit:
	@ opam-publish submit $(PACKAGE)

# -------------------------------------------------------------------------

# Pinning.

pin:
	opam pin add $(PACKAGE) `pwd` -k git

unpin:
	opam pin remove $(PACKAGE)
