This is an adaptation of Daan Leijen's
[PPrint](https://hackage.haskell.org/package/wl-pprint-1.2/docs/Text-PrettyPrint-Leijen.html)
library, which itself is based on the ideas developed by Philip Wadler in
[A Prettier Printer](http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf).

To install the latest released version of PPrint, it should be sufficient to type:
```
  opam install pprint
```

To install PPrint from source, type:
``
  `make install
```
The requirements are OCaml 4.02.0 or later, `ocamlbuild`, and `ocamlfind`.

The documentation for PPrint is built by `make doc` and is then found in the
file `src/doc/index.html`.
