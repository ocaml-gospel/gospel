- Where are we with
  [issue#366](https://github.com/ocaml-gospel/gospel/issues/366)?

- Make the Gospel type-checker modular
  [issue#377](https://github.com/ocaml-gospel/gospel/issues/377)
  - goal for gospel: scale, performance issue
  - why now?
    + we are planning to use ortac on larger code base so it is a good timing
      to think about performance
    + coordinate with dune integration
  - what to do?
    - decide where to write typing information? Apart in a `.gospel` file or in
      the `.cmti`? see discussion and there is already some experimentation
      with the first way
      [pr#376](https://github.com/ocaml-gospel/gospel/pull/376)
    - revise Ident as unicity of identifiers will not be guaranteed anymore in
      the current implementation see
      [issue#381](https://github.com/ocaml-gospel/gospel/issues/381)?
    - do we need to revise namespace too? Could be an opportunity to clarify it
      and document it.

- Continue discussion about exceptional postcondition syntax see my
  [branch](https://github.com/n-osborne/gospel/tree/xpost-syntax-examples)
