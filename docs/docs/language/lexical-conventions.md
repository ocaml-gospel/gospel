---
sidebar_position: 2
---

# Lexical conventions

Gospel borrows most of [OCaml's lexical
conventions](https://caml.inria.fr/pub/docs/manual-ocaml/lex.html).
This means that the whitespace rules are the same, that the same kind
of variable names are allowed, that integers are written the same way,
etc. There are however a number of exceptions:

- There are reserved keywords:
  ```
  axiom     checks    consumes    diverges    ephemeral    equivalent    model
  modifies  pure      raises      requires    variant      coercion      ensures
  exists    forall    invariant   predicate
  ```

- There are reserved symbols:
  ```
  <->    /\    \/
  ```

In the rest of this documentation, `lident` (resp. `uident`) stands for an
identifier with a lowercase (resp. uppercase) first character.

