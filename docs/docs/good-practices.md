---
sidebar_position: 5
---

# Tips and Good Practices

## Function Clauses Ordering

Always orgainse the clauses in the same order for the best results:

- `requires` preconditions
- `checks` preconditions
- `modifies` and `consumes` effects
- `ensures` postconditions
- `raises` exceptional postconditions

:::tip

Write clauses following the chronological order of the program execution.

:::

## Ephemeral

The [`ephemeral`](language/type-specifications#mutable-types) keyword can appear
in [type specifications](language/type-specifications) to specify that a type
has some mutable state. When the mutability is already visible in the interface
(through models or definition), we advise to *not* use the `ephemeral` clause.

:::tip

Only use `ephemeral` when there is mutability that cannot be guessed otherwise.

:::
