---
sidebar_position: 1
---

# Gospel special comment syntax

Gospel specifications are written in interface files (`.mli`). They are written
in special comments, starting with the `@` character[^1]:

[^1]: Existing specification languages for other host languages introduced this
    notation, *e.g.* [JML](https://www.cs.ucf.edu/~leavens/JML/index.shtml) for
    Java and [ACSL](https://frama-c.com/html/acsl.html) for C. Hence Gospel
    also uses this convention.

<!-- invalidSyntax: see #320 -->
```ocaml invalidSyntax
val f: int -> int           (* An OCaml value declaration *)
(*@ y = f x
    ensures x > 0 *)        (* Its Gospel specification   *)

(*@ type t *)               (* A ghost type declaration   *)
(*@ ephemeral
    model size: int *)      (* Its Gospel specification   *)
```

Those comments must be located _after_ the item they specify.


## Specifications and documentation comments

Note that Gospel annotations can be combined with traditional documentation
comments, *e.g.* as follows:

```ocaml invalidSyntax
val eucl_division: int -> int -> int * int
(** this is an implementation of Euclidean division *)
(*@ q, r = eucl_division x y
    ... *)
```
