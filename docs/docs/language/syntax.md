---
sidebar_position: 1
---

# Gospel Special Comment Syntax

Gospel specifications are written in interface files (`.mli`). They are written
in special comments, starting with the `@` character[^1]:

[^1]: Existing specification languages for other host languages introduced this
    notation, *e.g.,* [JML](https://www.cs.ucf.edu/~leavens/JML/index.shtml) for
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

Those comments must be located _after_ the item they specify.


:::tip

Gospel is using a preprocessor to turn those special comments into OCaml
attributes behind the scene before feeding the result to the standard OCaml
parser (see the [appendix page](attributes) for more information).

This has the following consequence: Gospel special comments can only appear at a
positions where OCaml accepts attributes. So given the following interface
`misplaced.mli`:

```ocaml invalidSyntax
val f : int -> (*@ misplaced *) int
```

`gospel check misplaced.mli` will complain with the following somewhat
surprising message:

```
1 | val f : int -> (*@ misplaced *) int
                   ^^^
Error: Syntax error.
```

:::


## Specifications and Documentation Comments

Note that Gospel annotations can be combined with traditional documentation
comments. For example:

```ocaml invalidSyntax
val eucl_division: int -> int -> int * int
(** this is an implementation of Euclidean division *)
(*@ q, r = eucl_division x y
    ... *)
```
