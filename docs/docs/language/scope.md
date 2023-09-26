---
sidebar_position: 8
---

# Symbols in Scope

Gospel checks that only symbols that are meaningful are in scope.

In pre-condition clauses `checks` and `requires`, the only symbols in scope are:
- function arguments,
- logical values (ghost functions, predicates, axioms),
- OCaml values tagged as `pure`.

In post-condition clauses `ensures` and `raises`, the only symbols in scope are:
- function results,
- all symbols in scope in pre-conditions.

In particular, OCaml values that are not tagged as `pure` are not in scope in
Gospel expressions.
