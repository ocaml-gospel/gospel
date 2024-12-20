---
sidebar_position: 5
---

# Constant Specifications

OCaml constant specifications are simple. They consist of a list of clauses
starting with the `ensures` keyword followed by a formula.

```ebnf title="Constant specification syntax"
constant_specification = ("ensures" expr)*
```

Here is an example:

```ocaml
type 'a array
(*@ mutable model contents : 'a sequence *)

val argv : string array
(*@ ensures Sequence.length argv.contents >= 1 *)
```

These clauses hold at the end of the surrounding module evaluation.
