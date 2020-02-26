# ------------------------------------------------------------------------------

.PHONY: all
all:
	dune build

.PHONY: clean
clean:
	dune clean

.PHONY: doc
doc:
	dune build @doc
	@echo You can find the documentation in _build/default/_doc/_html/index.html

.PHONY: test
test:
	dune runtest

.PHONY: bench
bench:
	dune build ./bench/PPrintBench.exe
	time dune exec ./bench/PPrintBench.exe

# ------------------------------------------------------------------------------

# The version number is automatically set to the current date,
# unless DATE is defined on the command line.
DATE     := $(shell /bin/date +%Y%m%d)

# The project's name.
THIS     := pprint

# The archive's URL (https).
ARCHIVE  := https://github.com/fpottier/$(THIS)/archive/$(DATE).tar.gz

# ------------------------------------------------------------------------------

.PHONY: install
install: all
	dune install -p $(THIS)

.PHONY: uninstall
uninstall:
	ocamlfind remove $(THIS) || true

.PHONY: reinstall
reinstall: uninstall
	@ make install

.PHONY: show
show: reinstall
	@ echo "#require \"pprint\";;\n#show PPrint;;" | ocaml

.PHONY: pin
pin:
	opam pin add $(THIS) .

.PHONY: unpin
unpin:
	opam pin remove $(THIS)

# ------------------------------------------------------------------------------

# [make headers] updates the headers.

HEADACHE := headache
HEADER   := header

.PHONY: headers
headers:
	for f in src/PPrint*.{ml,mli} ; do \
	  $(HEADACHE) -h $(HEADER) $$f ; \
	done

# -------------------------------------------------------------------------

# Publishing a release.

.PHONY: release
release:
# Make sure the current version can be compiled and installed.
	@ make uninstall
	@ make clean
	@ make install
# Check the current package description.
	@ opam lint
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

.PHONY: opam
opam:
	@ opam lint
	@ opam-publish prepare $(THIS).$(DATE) $(ARCHIVE)

.PHONY: submit
submit:
	@ opam-publish submit $(THIS).$(DATE)
