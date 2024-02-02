- open ml files [gospel#366](https://github.com/ocaml-gospel/gospel/issues/366)
  - goal for gospel: Cameleer use the released gospel, dev efforts are focused
    on one version
  - parse interfaces and implementation files
  - type check the contents of module signatures in ml files (common pattern
    for OCaml programmers), but not the implementation code
  - related: getting rid of the pps for dune integration (see:
    [n-osborne/experiment-with-parsing](https://github.com/n-osborne/gospel/tree/experiment-with-parsing)
    as it may change the way gospel attributes are caught (in signatures)

- exceptions specification
  [gospel#373](https://github.com/ocaml-gospel/gospel/issues/373)
  - the goal is to make it easy to express when (in what conditions) an
    exception is raised
  - we should find a syntax that makes value-result distinction clear
