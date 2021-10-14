---
sidebar_position: 2
---

# Lexical conventions

Gospel borrows most [OCaml lexical
conventions](https://caml.inria.fr/pub/docs/manual-ocaml/lex.html), with the
following exceptions:

- There are reserved keywords:
  ```
  axiom     checks    consumes    diverges    ephemeral    equivalent    model
  modifies  pure      raises      requires    variant      coercion      ensures
  exists    forall    invariant   predicate
  ```

- there are reserved symbols:
  ```
  <->    /\    \/
  ```

In the rest of this documentation, `lident` (resp. `uident`) stands for an
identifier with a lowercase (resp. uppercase) first character.

