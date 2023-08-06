# ------------------------------------------------------------------------------

# The version number is automatically set to the current date,
# unless DATE is defined on the command line.
DATE     := $(shell /bin/date +%Y%m%d)

# The project's name.
THIS     := pprint

# The archive's URL (https).
ARCHIVE  := https://github.com/fpottier/$(THIS)/archive/$(DATE).tar.gz

# ------------------------------------------------------------------------------

.PHONY: all
all:
	@ dune build @all

.PHONY: clean
clean:
	@ git clean -fX

.PHONY: test
test:
	@ dune runtest --force

.PHONY: bench
bench:
	@ dune build ./bench/PPrintBench.exe
	@ time dune exec ./bench/PPrintBench.exe

.PHONY: install
install: all
	@ dune install -p $(THIS)

.PHONY: uninstall
uninstall:
	@ ocamlfind remove $(THIS) || true

.PHONY: reinstall
reinstall: uninstall
	@ make install

.PHONY: show
show: reinstall
	@ echo "#require \"pprint\";;\n#show PPrint;;" | ocaml

.PHONY: pin
pin:
	@ opam pin add $(THIS) . --yes

.PHONY: unpin
unpin:
	@ opam pin remove $(THIS) --yes

# ------------------------------------------------------------------------------

# Documentation.

DOCDIR = _build/default/_doc/_html
DOC    = $(DOCDIR)/index.html

.PHONY: doc
doc:
	@ rm -rf _build/default/_doc
	@ dune clean
	@ dune build @doc
	@ echo "You can view the documentation by typing 'make view'".

.PHONY: view
view: doc
	@ echo Attempting to open $(DOC)...
	@ if command -v firefox > /dev/null ; then \
	  firefox $(DOC) ; \
	else \
	  open -a /Applications/Firefox.app/ $(DOC) ; \
	fi

.PHONY: export
export: doc
	ssh yquem.inria.fr rm -rf public_html/$(THIS)/doc
	scp -r $(DOCDIR) yquem.inria.fr:public_html/$(THIS)/doc

# ------------------------------------------------------------------------------

# [make versions] compiles the package under many versions of OCaml,
# whose list is specified below.

# This requires appropriate opam switches to exist. A missing switch
# can be created like this:
#   opam switch create 4.03.0

VERSIONS := \
  4.03.0 \
  4.04.2 \
  4.05.0 \
  4.06.1 \
  4.07.1 \
  4.08.1 \
  4.09.1 \
  4.09.0+bytecode-only \
  4.10.0 \
  4.11.1 \
  4.12.0 \
  4.13.0 \
  4.14.1 \
  5.0.0 \

.PHONY: versions
versions:
	@(echo "(lang dune 2.0)" && \
	  for v in $(VERSIONS) ; do \
	    echo "(context (opam (switch $$v)))" ; \
	  done) > dune-workspace.versions
	@ dune build --workspace dune-workspace.versions

# ------------------------------------------------------------------------------

# [make headache] updates the headers.

HEADACHE := headache
HEADER   := header

.PHONY: headache
headache:
	@ for f in src/*.{ml,mli} bench/*.{ml,mli} ; do \
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
# Done.
	@ echo "Done."
	@ echo "If happy, please type:"
	@ echo "  \"make publish\"   to publish a new opam package"
	@ echo "  \"make export\"    to upload the documentation to yquem.inria.fr"

.PHONY: publish
publish:
# Publish an opam description.
	@ opam publish -v $(DATE) $(THIS) $(ARCHIVE) .

.PHONY: undo
undo:
# Undo the last release (assuming it was done on the same date).
	@ git tag -d $(DATE)
	@ git push -u origin :$(DATE)

# -------------------------------------------------------------------------

# Copying pprint into Menhir's working directory.

MENHIR_WORKING_COPY=$(HOME)/dev/menhir
PPRINT_COPY=$(MENHIR_WORKING_COPY)/pprint

.PHONY: menhir
menhir: clean
# Copy our source files to the Menhir repository.
	@ rm -rf $(PPRINT_COPY)
	@ cp -r $(shell pwd) $(PPRINT_COPY)
# Remove a number of unneeded files and subdirectories.
	@ (cd $(PPRINT_COPY) && rm -rf .git .gitignore Makefile README.md TODO.md bench blog header test src/Makefile)
