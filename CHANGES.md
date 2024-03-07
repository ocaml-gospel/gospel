# 0.3

## Added

- Created a gallery of Gospel examples that might serve as a working ground
  to experiment with Gospel syntax and future extensions of the language.
  [#361] (https://github.com/ocaml-gospel/gospel/pull/361)

## Improved

- Make the type-checker save type information in a file
  [\#376](https://github.com/ocaml-gospel/gospel/pull/376)
- Make the `with` necessary when declaring type invariants
  [\#372](https://github.com/ocaml-gospel/gospel/pull/372) and
  [\#374](https://github.com/ocaml-gospel/gospel/pull/374)

## Internals

- Fix premature parsing of specification keywords in the preprocessor.
  [\#394](https://github.com/ocaml-gospel/gospel/pull/394)
- Fix `ls_name` of `unit` logical symbol to be `()`
  [\#387](https://github.com/ocaml-gospel/gospel/pull/387)
- Fix the `is_ts_tuple` function so that it doesn't take `unit` to be a tuple
  [\#386](https://github.com/ocaml-gospel/gospel/pull/386)
- Use unique identifiers rather than physical equality for `Symbols.ls_equal`
  [\#380](https://github.com/ocaml-gospel/gospel/pull/380)
- Remove the `gospel_expected` prologue at the end of successful tests
  [\#363](https://github.com/ocaml-gospel/gospel/pull/363)

# 0.2

## Added

- Introduce a generic `pp_gen` pretty-printer for error messages, so that
  external tools can pretty-print errors in the same style
  [\#326](https://github.com/ocaml-gospel/gospel/pull/326)
- Add ppx rewriter to display gospel contents as documentation with odoc
  [\#288](https://github.com/ocaml-gospel/gospel/pull/288)
- Add specific error message for patterns with guard on every clause.
  [\#220](https://github.com/ocaml-gospel/gospel/pull/220)
- Added `when` guards in pattern-matching
  [\#206](https://github.com/ocaml-gospel/gospel/pull/206)
- Added a `with` construct to name a variable in type invariants referring to a
  value of the specified type.
  [\#218](https://github.com/ocaml-gospel/gospel/pull/218) and
  [\#187](https://github.com/ocaml-gospel/gospel/pull/187)
- Added support for `int` literals.
  [\#175](https://github.com/ocaml-gospel/gospel/pull/175) and
  [\#177](https://github.com/ocaml-gospel/gospel/pull/177) and
  [\#223](https://github.com/ocaml-gospel/gospel/pull/223)
- Added the `Failure` exception to the Stdlib.
  [\#154](https://github.com/ocaml-gospel/gospel/pull/154)
- Added a `gospel dumpast` command.
  [\#98](https://github.com/ocaml-gospel/gospel/pull/98) and
  [\#184](https://github.com/ocaml-gospel/gospel/pull/184)

## Improved

- Forbid `old` operator in precondition clauses (`requires` and `checks`)
  [\#335](https://github.com/ocaml-gospel/gospel/pull/335)
- Display a warning when encountering an `include`
  [\#334](https://github.com/ocaml-gospel/gospel/pull/334)
- Allow patterns in arguments and return type annotation in anonymous functions
  [\#309](https://github.com/ocaml-gospel/gospel/pull/309)
- Propagate pattern locations to report errors to the precise patterns
  [\#308](https://github.com/ocaml-gospel/gospel/pull/308)
- Support partial application of functions and enforce OCaml syntax
  for constructor application
  [\#290](https://github.com/ocaml-gospel/gospel/pull/290)
- Add a pretty-printer for locations
  [\#294](https://github.com/ocaml-gospel/gospel/pull/294)
- Gospel preprocessor does not fail when the file is an implementation file
  [\#265](https://github.com/ocaml-gospel/gospel/pull/265)
- Rename standard library `Seq` and `'a seq` to `Sequence` and `'a sequence`.
  [\#253](https://github.com/ocaml-gospel/gospel/pull/253)
- Allow unit result in function header
  [\#215](https://github.com/ocaml-gospel/gospel/pull/215)
- Highlight source locations when reporting errors.
  [\#214](https://github.com/ocaml-gospel/gospel/pull/214)
- Check for pattern-matching redundancy in terms.
  [\#213](https://github.com/ocaml-gospel/gospel/pull/213)
- Check for pattern-matching exhaustivity in terms.
  [\#170](https://github.com/ocaml-gospel/gospel/pull/170)
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

- Fix the performance issues in the preprocessor
  [\353](https://github/ocaml-gospel/gospel/pull/353)
- Gospel preprocessor support documentation for ghost declaration
  [\#331](https://github/ocaml-gospel/gospel/pull/331)
- Consider comments as spaces while preprocessing (to ensure specification can
  be attached to a ghost function or type, for instance)
  [\#321](https://github/ocaml-gospel/gospel/pull/321)
- Fix source-location tracking (directives and overridden filename)
  [\#319](https://github/ocaml-gospel/gospel/pull/319)
- Set up location in parsing ghost specifications
  [\#310](https://github/ocaml-gospel/gospel/pull/310)
- Check that all patterns in a disjunction bind the same variables
  [\#300](https://github/ocaml-gospel/gospel/pull/300)
- Handle the special case of `MODULE_ALIASES` in `stdlib.mli` in the parser
  [\#306](https://github/ocaml-gospel/gospel/pull/306)
- Take recursivity into account when typing type declarations
  [\#304](https://github/ocaml-gospel/gospel/pull/304)
- Support pattern with cast
  [\#301](https://github/ocaml-gospel/gospel/pull/301)
- Use payload location for specification text
  [\#299](https://github/ocaml-gospel/gospel/pull/299)
- Support patterns of one-parameter constructors with a tuple argument
  [\#297](https://github/ocaml-gospel/gospel/pull/297)
- Use correct location for arity mismatches in type applications
  [\#258](https://github/ocaml-gospel/gospel/pull/258)
- Avoid uncaught exception when displaying a warning for builtins (using
  `Location.none`)
  [\#283](https://github.com/ocaml-gospel/gospel/pull/283)
- Gospel preprocessor no longer detach documentation below a declaration
  [\#281](https://github.com/ocaml-gospel/gospel/pull/281)
- Fixed pattern match analysis in exceptional postconditions
  [\#277](https://github/ocaml-gospel/gospel/pull/277)
- Avoid uncaught exception when displaying a warning on a dummy
  position
  [\#262](https://github.com/ocaml-gospel/gospel/pull/262)
- Constants can now be referenced in specifications.
  [\#211](https://github.com/ocaml-gospel/gospel/pull/211)
- Infix operators in specificaion headers are now accepted.
  [\#205](https://github.com/ocaml-gospel/gospel/pull/205)
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

- Display the backtrace of an error when `$GOSPELDEBUG` is set
  [\#295](https://github.com/ocaml-gospel/gospel/pull/295)
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
