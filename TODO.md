* Test the interaction of `range` with the automatic removal of trailing
  blank characters. Do we obtain the desired behavior?

* Set up a real test suite.

* Write a real documentation and tutorial.

* Fix the warnings produced by `make doc`. Review its output.

* Update the private `Makefile` so as to publish the package documentation
  on yquem (or gitlab?).

* Try to speed up the random generator.
  `choose`, applied to a list, is too slow: use an array?
  avoid building n suspensions when only one will be forced?

* Extend `PPrintBench` to also try non-random documents of large size.
