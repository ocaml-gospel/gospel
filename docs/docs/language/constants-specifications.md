---
sidebar_position: 5
---

# Constants specifications

OCaml constants specifications are simple. They consist in a list of clauses
starting with the `ensures` keyword, and followed by a formula.

```ebnf title="Constant specification syntax"
constant_specification = ("ensures" expr)*
```

Here is an example:

```ocaml
val argv : string array
(*@ ensures Array.length argv >= 1 *)
```

These clauses hold at the end of the evaluation of the module.
