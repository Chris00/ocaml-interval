1.5 2018-
--------------

- The library is now organized as 3 packages:
  - `interval_base` (which defines the module `Interval`) group the
    functions that work on any IEEE-754 processor;
  - `interval_intel`: module using assembly instructions on Intel
    Processors;
  - `interval_crlibm`: module using the library CRLibm to evaluate
    standard functions (a bit slower but more precise than the Intel
    package).
- New functions: `invx` (extended inverse), `cancelminus`,
  `cancelplus`, `inter` and `inter_exn`.
- New binary relations `equal`, `=`, `subset`, `<=`, `>=`, `precedes`,
  `interior`, `<`, `>`, `strict_precedes`, `disjoint`.
- New predicates `is_bounded`, `is_entire`.
- New constant `I.entire` for [-∞, +∞].
- The module `I.U` also restores inequality relations.
- Speed and documentation improvements.

1.4 2018-03-01
--------------

- Improved interface for the `Interval` library by using sub-modules
  and standard mathematical names.  In particular, all operations —
  including infix operators — are in a sub-module `I` which can
  conveniently be used to introduce local scopes after issuing `open
  Interval`.

- Improved pretty-printing functions allowing to pass the format of
  the interval bounds.

- The library functions now signal errors by exceptions
  `Division_by_zero` and `Domain_error` that are *local* to
  `Interval`.

- The `Fpu` module has been redesigned: the rounding up or down of
  functions is controlled by the sub-module (`Low` or `High`) to which
  they belong.  This allows for natural expressions such as
  `Low.(x**2. +. 2. *. x +. 1.)`.

- Jbuilder/dune is used to compile and install the library.

- TravisCI and AppVeyor continuous integration ensure the library
  works on a variety of OCaml versions and platforms.
