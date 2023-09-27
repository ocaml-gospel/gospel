---
sidebar_position: 2
---

# Lexical Conventions

Gospel borrows most of [OCaml's lexical
conventions](https://caml.inria.fr/pub/docs/manual-ocaml/lex.html).
This means that the whitespace rules are the same. The same kind
of variable names are allowed, integers are written the same way,
etc. There are however a number of exceptions:

- There are extra reserved keywords:
  ```
  axiom        checks   coercion   consumes    diverges   ensures    ephemeral
  equivalent   exists   forall     invariant   model      modifies   old
  predicate    pure     raises     requires    variant
  ```

- There are reserved symbols:
  ```
  <->    /\    \/
  ```

- There is an extra literal modifier for literals of type `int`. Unmodified
  literals (e.g. `42`) are of type `integer`, but Gospel adds a `i` modifier to
  write literals of type `int` (e.g. `42i`).
