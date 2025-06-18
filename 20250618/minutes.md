# Gospel type-checker dev meeting minutes 2025-06-18

Attending: Tiago Soares, Mário Perreira, Nicolas Osborne, François Pottier, Andrei Paskevich

Agenda: discuss rewriting PR

## First order functions

We'll keep support for second-order functions with the hypothesis that the functional argument is pure.

So Ortac/QCheck-STM will still be able to test higher-order functions.

On the syntax level, pure will stay the default. This choice may be revisited later when we add support for impure functional argument.

Functional argument will be specified by nested specification.

Clear semantic of `pure` should be decided:
- no dependency on a mutable state
- can modify an hidden state, but pure mapping

## Pattern matching

- Discussion on the motivation: use abstract types rather than ADTs.

How to talk about some specifics on trees

In the long run we may need to re-introduce pattern-matching.

## Updated syntax

Useful for exceptional behaviour (be able to place the call in a match)

Need a clear way to explain which name are allowed where (pre / post condition)

We've discussed an alternative syntax in order to keep binding the names at the top of the specification but be able to formulate a list of clauses in exceptional cases:

```ocaml
val pop : 'a t -> 'a
(*@ x = pop s
    consumes ...
    ensures x = ...
    raises Empty (* or another keyword *)
      produces ...
      ensures ...
    raises .... (* for other exceptions *) *)
```

This would introduce a notion of order in how we write specification: normal behaviour first then exceptional ones if any.

## Prop and Bool

Explicit distinction between prop and bool.
What about if-then-else?

## Merging

Mário will be merging the PR when he is happy.
