---
sidebar_position: 1
---

# Specification locations

## General conventions

Gospel annotations are written in interface files (`.mli`).

We use [OCaml
attributes](https://caml.inria.fr/pub/docs/manual-ocaml/attributes.html) with
the identifier `gospel` to bear the Gospel specifications in their payload, as
strings: `[@@gospel "<spec>"]` and `[@@@gospel "<spec>"]`.

### Floating attributes

[Ghost and logical declarations](logical.md) must lie in floating attributes,
inside module signatures:

```ocaml
[@@@gospel "val f : int -> int"]
[@@@gospel "predicate is_zero (x: integer) = x = 0"]
```

### Attached attributes

Specification bits which are semantically attached to OCaml declarations (e.g.
[function contracts](function-contracts.md) or [type
specifications](type-specifications.md)) should be written in an attached
attribute, following OCaml's attachment rules:

```ocaml
val f: int -> int
[@@gospel "y = f x ensures x > 0"]
```

### Specification of ghost and logical declarations

When ghost and logical declarations need to be specified with a contract, the
contract should reside in an attribute attached to the string containing the
declaration:

```ocaml
[@@@gospel "val f : int -> int"
  [@@gospel "y = f x ensures x > 0"]]
```

## Gospel preprocessor

Writing attributes is tedious, especially when nested. Gospel provides a
preprocessor that lets you write Gospel specifications in special comments,
starting with the `@` character[^1]:

[^1]: Existing specification languages for other host languages introduced this
    notation, *e.g.* [JML](https://www.cs.ucf.edu/~leavens/JML/index.shtml) for
    Java and [ACSL](https://frama-c.com/html/acsl.html) for C. Hence Gospel
    also uses this convention.

```ocaml
val f: int -> int           (* An OCaml value declaration *)
(*@ y = f x
    ensures x > 0 *)        (* Its Gospel specification   *)

(*@ type t *)               (* A ghost type declaration   *)
(*@ ephemeral
    model size: int *)      (* Its Gospel specification   *)
```

Although the preprocessor is available via the `gospel pps` command, it is also
applied automatically on type-checking, so you should not have to worry about
manually applying it.
At the moment, the preprocessor only supports comments located _after_ the item
they specify.

:::info

The special `(*@ ... *)` comment notation will be used throughout the
documentation, and the attribute notation will not appear anymore.

:::


## Specifications and documentation comments

Note that Gospel annotations can be combined with traditional documentation
comments, *e.g.* as follows:

```ocaml
val eucl_division: int -> int -> int * int
(** this is an implementation of Euclidean division *)
(*@ q, r = eucl_division x y
    ... *)
```


