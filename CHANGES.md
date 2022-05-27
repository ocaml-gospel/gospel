# Unreleased

## Added

- Added a `with` construct to name a variable in type invariants referring to a
  value of the specified type.
  [\#187](https://github.com/ocaml-gospel/gospel/pull/187)
- Added support for `int` literals.
  [\#175](https://github.com/ocaml-gospel/gospel/pull/175) and
  [\#177](https://github.com/ocaml-gospel/gospel/pull/177)
- Added the `Failure` exception to the Stdlib.
  [\#154](https://github.com/ocaml-gospel/gospel/pull/154)
- Added a `gospel dumpast` command.
  [\#98](https://github.com/ocaml-gospel/gospel/pull/98) and
  [\#184](https://github.com/ocaml-gospel/gospel/pull/184)

## Improved

- Check for pattern-matching exhaustivity in terms.
  [\#170](https://github.com/ocaml-gospel/gospel/pull/185)
- Issue a warning when a function returns `unit` but has no `modifies` clause.
  [\#185](https://github.com/ocaml-gospel/gospel/pull/185)
- Improved locations for syntax errors in specs.
  [\#164](https://github.com/ocaml-gospel/gospel/pull/164)
- Documentation improvements.
  [\#108](https://github.com/ocaml-gospel/gospel/pull/108) and
  [\#110](https://github.com/ocaml-gospel/gospel/pull/110) and
  [\#149](https://github.com/ocaml-gospel/gospel/pull/149)
- Added sanity checks to type invariants: invariants are only allowed on private
  or abstract types. [\#117](https://github.com/ocaml-gospel/gospel/pull/117)
  and [\#116](https://github.com/ocaml-gospel/gospel/pull/116)
- Stdlib changes. [\#102](https://github.com/ocaml-gospel/gospel/pull/102)
  - Removed `ref` operator.
  - Removed `elements` for `Bag` and `Set`.
  - `compare` functions returns `integer`s instead of `int`s.

## Fixed

- Fix inconsistencies in typing/parsing of exceptional postconditions patterns.
  [\#203](https://github.com/ocaml-gospel/gospel/pull/203)
- Fixed the type-checking of interfaces involving the OCaml Stdlib, which was
  not opened by default. GADTs are considered abstract types.
  [\#195](https://github.com/ocaml-gospel/gospel/pull/195)
- Fixed incorrectly rejected interfaces with absent\partial function contracts
  in the presence of tuples as return values.
  [\#193](https://github.com/ocaml-gospel/gospel/pull/193)
- Fixed support for types containing functions.
  [\#186](https://github.com/ocaml-gospel/gospel/pull/186)
- Fixed wrong syntax errors in patterns.
  [\#181](https://github.com/ocaml-gospel/gospel/pull/181)
- Fixed wrong invalid patterns over `unit` values.
  [\#174](https://github.com/ocaml-gospel/gospel/pull/174)
- Fixed the error message for pattern type errors.
  [\#172](https://github.com/ocaml-gospel/gospel/pull/172)

## Internals

- Fixed the order of exceptional postconditions in the AST.
  [\#200](https://github.com/ocaml-gospel/gospel/pull/200)
- Refactored the error handling: Gospel now only raises a single `Gospel.Error`
  exception.
  [\#189](https://github.com/ocaml-gospel/gospel/pull/189)
- Fixed the representation for type variables in `ts_tuple` and `fs_tuple`.
  [\#183](https://github.com/ocaml-gospel/gospel/pull/183)
- Added `int` as a primitive type.
  [\#171](https://github.com/ocaml-gospel/gospel/pull/171)
- Refactored `Tterm` and `Symbols`.
  [\#85](https://github.com/ocaml-gospel/gospel/pull/85)
- Redefined the `ghost` type.
  [\#155](https://github.com/ocaml-gospel/gospel/pull/155)
- Removed the `Pty_open` type constructor
  [\#109](https://github.com/ocaml-gospel/gospel/pull/109)


# 0.1.0 (11-03-2021)

- Initial release
