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
val argv : string array
(*@ ensures Array.length argv >= 1 *)
```

These clauses hold at the end of the surrounding module evaluation.
