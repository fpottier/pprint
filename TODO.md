* Document that `align` is dangerous, and point to Pretty Expressive.

* Document that the target width can be exceeded. At each group, the engine
  asks: can this group fit on the current line? If the answer is positive,
  then the engine commits to printing the entire group in flat mode, on the
  current line. Yet it is possible that the material that *follows this group*
  cannot fit on the current line! There is no "lookahead" when deciding
  whether a group should be printed flat or dissolved. As a result, there is
  no guarantee that the target width supplied by the user will be respected.

* Test the interaction of `range` with the automatic removal of trailing
  blank characters. Do we obtain the desired behavior?

* Set up a real test suite.

* Fix the warnings produced by `make doc`. Review its output.

* Update the private `Makefile` so as to publish the package documentation
  on yquem (or gitlab?).

* Try to speed up the random generator.
  `choose`, applied to a list, is too slow: use an array?
  avoid building n suspensions when only one will be forced?

* Extend `PPrintBench` to also try non-random documents of large size.
