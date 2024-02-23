Discussing about the content of gospel.0.3 release, as a new (and needed)
release of ortac depends on some new features of gospel.

- [pr#376](https://github.com/ocaml-gospel/gospel/pull/376) has been reviewed
  by Samuel and updated accordingly this morning. Mário should review it too
  (depending on time) and the PR should be included in gospel.0.3
- merging the cameleer version of gospel into main takes some time, this won't
  be included in gospel.0.3, but later.
- including the typing of module types in `.ml` files may be included in
  gospel.0.3 if Mário find the time, but this is not necessary on the ortac
  side, so it can also wait

Other discussion on the type-checker:

- Tiago noted that names in the typed ast are not qualified (no path). This is
  a problem for translation into Why3 and to Coq. A github issue should be
  created, or some comment on
  [issue#381](https://github.com/ocaml-gospel/gospel/issues/381) if
  appropriate, but having the paths in the typed ast seems a good idea.


About the gospel standard library:

- Tiago made some progress on rewriting the gospel standard library in the
  spirit of what had been said in the math library meeting last week: focusing
  on sequences and define behaviours of function with axioms.
- No problem is expected on translating it to OCaml: a large enough subset of
  the library should be computations.
- Please create a draft PR as soon as possible.
