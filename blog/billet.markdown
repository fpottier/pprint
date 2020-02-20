<!-- :!pandoc -s % -c style.css -H header.html > billet.html -->

I am pleased to announce the first official release of _**PPrint**_, an OCaml
library for pretty-printing textual documents.

## A taste of the layout language
 
At the heart of _**PPrint**_ is a little domain-specific language of
documents. This language has a well-defined semantics, which the printing
engine implements. This language rests upon a small number of fundamental
concepts.

There are combinators for creating atomic documents. For
instance,

```ocaml
string "hello"
```

is a simple, unbreakable document.

There is also a concatenation operator, which joins two documents.
For instance,

```ocaml
string "hello" ^^ string "world"
```

is a composite document. It is in fact equivalent to `string "helloworld"`.

So far, nothing very exciting. The next two combinators are more original and
interesting.

The first of these combinators, `break 1`, is a breakable space. If printed in
flat mode, it produces an ordinary space character; if printed in normal mode,
it produces a newline character.

Yes, there are two printing modes, namely flat mode and normal mode. The
printing engine goes back and forth between these two modes. Exactly where and
how the engine switches from one mode to the other is controlled by the next
combinator.

The second of these combinators, `group`, introduces a choice between flat
mode and normal mode. It is a document transformer: if `d` is a document, then
`group d` is a document. When the printing engine encounters `group d`, two
possibilities arise. The first possibility is to print all of `d` on a single
line. This is known as flat mode. The engine tries this first (ignoring any
`group` combinators inside `d`). If it succeeds, great. If it fails, by lack
of space on the current line, then the engine backtracks and reverts to the
second possibility, which is to simply ignore the `group` combinator, and just
print `d`. This has subtle consequences: there might be further groups inside
`d`, and each of these groups will give rise to further choices.

This gives rise to an interesting language, where `group` is used to indicate
a choice point, and the appearance of `break` is dependent upon the choice
point(s) that appear higher up in the hierarchical structure of the document.
For instance, the document:

```ocaml
group (string "This" ^^ break 1 ^^ string "is" ^^ break 1 ^^ string "pretty.")
```

will be printed either on a single line, if it fits, or on three lines. It
will not be printed on two lines: there is just one choice point, so either
the two breakable spaces will be broken, or none of them will. By the way,
this document can be abbreviated as follows:

```ocaml
group (string "This" ^/^ string "is" ^/^ string "pretty.")
```

On the other hand, the document:

```ocaml
string "This" ^^
group (break 1 ^^ string "is") ^^
group (break 1 ^^ string "pretty.")
```

could be printed on one, two, or three lines. There are two choice points,
each of which influences one of the two breakable spaces. The two choices are
independent of one another. Each of the words in the sentence `This is
pretty.` will be printed on the current line if it fits, and on a new line
otherwise. By the way, this document can be abbreviated as follows:

```ocaml
flow (break 1) [
  string "This" ;
  string "is" ;
  string "pretty."
]
```

There are more combinators, such as `nest`, which controls indentation, and
it is relatively easy to roll your own combinators on top of those that are
provided.

One limitation of the library is that the document must be entirely built in
memory before it is printed. So far, we have used the library in small- to
medium-scale applications, and this has not been a problem. In principle,
one could work around this limitation by adding a new document constructor
whose argument is a suspended document computation.

## Acknowledgements
 
The document language and the printing engine are inspired by Daan Leijen's
[PPrint](http://www.cs.uu.nl/~daan/pprint.html)
library, which itself is based on the ideas developed by Philip
Wadler in the paper
[A Prettier Printer](http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf).

_**PPrint**_ was written by François Pottier and Nicolas Pouillard, with
contributions by Yann Régis-Gianas, Gabriel Scherer, and Jonathan
Protzenko.


## Installation

The library is available online
([source code](http://gallium.inria.fr/~fpottier/pprint/pprint.tar.gz),
 [documentation](http://gallium.inria.fr/~fpottier/pprint/doc/)),
and can also be installed via OPAM: just type `opam install pprint`
if you already have a working OPAM installation.

Have fun! Feel free to make comments, suggestions, and to let me know if
and how you are using this library.

