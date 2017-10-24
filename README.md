Interval
========

This is an [interval arithmetic][] library for OCaml.

This library uses assembly code to compute all operations with proper
rounding, and currently **ONLY** works on Intel processors.
The package has been developed for Linux systems but should probably
work on windows distribution with a few tweaks.

To build the library just type `make` in the main directory.

After compilation, the documentation will be available in the `doc/`
directory in HTML format.  You can also consult the interfaces
of [Interval](interval.mli) and [Fpu](fpu.mli).  It is extremely wise
to read the whole documentation, even if you intend to only use the
interval module.

Tests are available in the `TESTS/` directory.  They are mainly for
debugging purpose and quite complicated.  You may run them (`make
tests`) to check that everything is working properly for your machine.
The `test` program runs also a speed test for your particular
architecture.

Examples are available in the `EXAMPLES/` directory.  There is a
`B_AND_B` sub-directory with an example of a branch-and-bound
algorithm that uses interval arithmetics for function optimization
(the example is for the Griewank function, but you can substitute any
function you like).


All bug reports should be sent to  
jean-marc.alliot@irit.fr  
gottelan@recherche.enac.fr

Happy interval programming...

[interval arithmetic]: https://en.wikipedia.org/wiki/Interval_arithmetic
