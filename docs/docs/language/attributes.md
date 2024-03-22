---
sidebar_position: 10
---

# Appendix: Gospel in OCaml Attributes

Gospel processes a file in various stages:

- Gospel preprocessor turns the special comments `(*@ ... *)` into [OCaml
  attributes],
- it then relies on the standard OCaml parser to generate an AST,
- it finally parses the Gospel specifications inside attributes.

[OCaml attributes]: https://caml.inria.fr/pub/docs/manual-ocaml/attributes.html

Gospel uses OCaml attributes with the identifier `gospel` to bear the Gospel
specifications in their payload as strings: `[@@gospel "<spec>"]` and
`[@@@gospel "<spec>"]`.

## Floating Attributes

[Ghost and logical declarations](logical.md) must lie in floating attributes,
inside module signatures:

```ocaml
[@@@gospel "val f : int -> int"]
[@@@gospel "predicate is_zero (x: integer) = x = 0"]
```

## Attached Attributes

Specification bits which are semantically attached to OCaml declarations (e.g.
[function contracts](function-contracts.md) or [type
specifications](type-specifications.md)) should be written in an attached
attribute, following OCaml's attachment rules:

```ocaml
val f: int -> int
[@@gospel "y = f x ensures x > 0"]
```

## Specification of Ghost and Logical Declarations

When ghost and logical declarations need to be specified with a contract, the
contract should reside in an attribute attached to the string containing the
declaration:

```ocaml
[@@@gospel "val f : int -> int"
  [@@gospel "y = f x ensures x > 0"]]
```

## Gospel Preprocessor

The preprocessor is available via the `gospel pps` command. It is also applied
automatically on type-checking, so you should not have to worry about manually
applying it.
