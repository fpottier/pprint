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

.PHONY: export
export: doc
	ssh yquem.inria.fr rm -rf public_html/$(THIS)/doc
	scp -r _build/default/_doc/_html yquem.inria.fr:public_html/$(THIS)/doc

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
	opam pin add $(THIS) . --yes

.PHONY: unpin
unpin:
	opam pin remove $(THIS) --yes

# ------------------------------------------------------------------------------

# [make versions] compiles the package under many versions of OCaml,
# whose list is specified below.

# This requires appropriate opam switches to exist. A missing switch
# can be created like this:
#   opam switch create 4.03.0

VERSIONS := \
  4.02.3 \
  4.03.0 \
  4.04.2 \
  4.05.0 \
  4.06.1 \
  4.07.1 \
  4.08.1 \
  4.09.0 \
  4.09.0+bytecode-only \
  4.10.0 \

.PHONY: versions
versions:
	@(echo "(lang dune 2.0)" && \
	  for v in $(VERSIONS) ; do \
	    echo "(context (opam (switch $$v)))" ; \
	  done) > dune-workspace.versions
	@ dune build --workspace dune-workspace.versions

# ------------------------------------------------------------------------------

# [make headers] updates the headers.

HEADACHE := headache
HEADER   := header

.PHONY: headers
headers:
	for f in src/*.{ml,mli} ; do \
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

# This entry assumes that [make release] has been run on the same day.

# Once the opam package has been published, run [make export].

.PHONY: opam
opam:
	@ opam lint
	@ opam publish -v $(DATE) $(THIS) $(ARCHIVE)
