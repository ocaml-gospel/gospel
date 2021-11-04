---
sidebar_position: 4
---

# Type specifications

OCaml types can be annotated with Gospel specifications in order to model their
contents and express invariants. Consider the following example of a container
data-structure.

```ocaml
type 'a t
(*@ model capacity: int
    mutable model contents: 'a Set.t
    invariant Set.cardinal contents <= capacity *)
```

The specification of this type contains three elements:
 - the first two lines are models. They represent the type `t` in the logical
   domain. `capacity` is an immutable model of type `int` representing the
   maximum length of the data-structure, and `contents` is a mutable set
   representing its contents. Note that `Set.t` references a logical set
   provided by Gospel's [standard library](../stdlib). Models do not
   give any information on the actual implementation of `t`.
 - the last line is a clause denoting a type invariant. At all times, values of
   type `t` should contain less elements that their maximum capacity.

Type specifications can contain models, invariants, and mutability information.

```ebnf title="Type specification syntax"
type_specification = type-specification-clause*
type_specification_clause =
  "ephemeral"
  | "mutable"? "model" identifier ":" type_expression
  | "invariant" expr
```

## Models

Type models are logical projections of OCaml types. They help specify the type
invariants and contents at several locations of the program, without actually
exposing them and leaking implementation details.

The keyword `model` is used to introduce a new model. It may be preceded by the
keyword `mutable` to denote that the model may be mutated during the execution
of the program, e.g. by a function call. It is then followed by a type annotated
name for that model, in a fashion similar to OCaml's record fields.

```ocaml {2,3}
type 'a t
(*@ model capacity: int
    mutable model contents: 'a Set.t
    invariant Set.cardinal contents <= capacity *)
```

## Mutable types

Gospel lets you specify when a type may contain some mutable state by using the
keyword `ephemeral` in its annotation:

```ocaml {3}
type t
(*@ model capacity: int
    ephemeral *)
```

Of course, a type that has a mutable model is considered mutable, so the
`ephemeral` may be omitted whenever at least one declared model is mutable.

## Invariants

Type annotations may also contain invariants that hold at every entry and exit
point of every function that manipulates their values. Formulae expressing these
properties may be added after the `invariant` keyword:

```ocaml {4}
type 'a t
(*@ model capacity: int
    mutable model contents: 'a Set.t
    invariant Set.cardinal contents <= capacity *)
```

Note that functions may break these invariants internally, but must restore them
so that they still hold at the function exit.
