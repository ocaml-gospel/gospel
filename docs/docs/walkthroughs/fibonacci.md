---
sidebar_position: 2
---

# Fibonacci Numbers

In this example, we'll look into specifying an efficient implementation of a
function computing Fibonacci numbers. This example is adapted from the article
_The Spirit of Ghost Code_[^1].

[^1]: Jean-Christophe Filliâtre, Léon Gondelman, Andrei Paskevich. _The Spirit
    of Ghost Code_. Formal Methods in System Design, Springer Verlag, 2016, 48
    (3), pp.152-174.
    ⟨[10.1007/s10703-016-0243-x](https://dx.doi.org/10.1007/s10703-016-0243-x)⟩.
    ⟨[hal-01396864](https://hal.archives-ouvertes.fr/hal-01396864v1)⟩

## The Problem

Recall that [Fibonacci numbers](https://en.wikipedia.org/wiki/Fibonacci_number)
are defined as follows:

$$
F_0 = 0\\
F_1 = 1\\
F_i = F_{i-1} + F_{i-2}
$$

To begin, let's introduce this definition into the Gospel world by using an
uninterpreted logical function along with an axiom defining it.

```ocaml
(*@ function fibonacci (n: integer) : integer *)
(*@ axiom a:
         fibonacci 0 = 0
      /\ fibonacci 1 = 1
      /\ forall n. n >= 2 -> fibonacci n = fibonacci (n-1) + fibonacci (n-2) *)
```

Below is an implementation of such a function. When `a` and `b` are two
consecutive Fibonacci numbers, the following function computes the `n`th number
ahead of `a`. Hence, `fib n 0 1` is the `n`th Fibonacci number.

```ocaml implementationSyntax
let rec fib n a b =
  if n < 0 then invalid_arg "n must be non-negative";
  if n = 0 then a
  else fib (n-1) b (a+b)
```

Its signature is simple:

```ocaml
val fib : int -> int -> int -> int
```

## A Simple Contract

Let's write a first contract for this interface.

```ocaml
val fib : int -> int -> int -> int
(*@ r = fib n a b
    checks n >= 0
    requires exists i.
               i >= 0 /\ a = fibonacci i /\ b = fibonacci (i+1)
    ensures forall i.
               i >= 0 /\ a = fibonacci i /\ b = fibonacci (i+1)
               -> r = fibonacci (i+n) *)
```

The contract is pretty straightforward:
  - The first precondition states that `n` must be non-negative. If `n`
    is negative, the function raises an exception, so this is a strong
    requirement.
  - The second precondition states that `a` and `b` are consecutive Fibonacci
    numbers.
  - The postcondition specifies that if `a` is the `i`th Fibonacci number, and
    `b` is the `i+1`th Fibonacci number, then `r` (the result of the
    computation) is the `i+n`th Fibonacci number.

Although this specification isn't complicated, it's
quite verbose and repetitive. The term `i >= 0 /\ a = fibonacci i /\ b =
fibonacci (i+1)` is repeated, and it actually refers to the same value of `i`
in both occurrences. We can certainly do better.

## Simplify by Using a Ghost Argument

Let's imagine for a moment that our `fib` function takes an additional
argument, `i`, representing the index of `a` in the Fibonacci sequence (the same
`i` that we've been using in the specification so far). Its contract could look
like:

```ocaml
val fib : i:int -> int -> int -> int -> int
(*@ r = fib ~i n a b
    checks n >= 0
    requires i >= 0 /\ a = fibonacci i /\ b = fibonacci (i+1)
    ensures r = fibonacci (i+n) *)
```

We don't need to quantify over `i` anymore, since it's provided to the
function, but we still need to state the preconditions that apply to it (see the
second clause). We don't need to repeat the previous condition on `i` either.
If it holds in the prestate, then it also holds in the poststate because nothing
here is mutable.

This contract is much easier to write, and more importantly, it's much easier to
read and to reason about. We cheated a bit though: `fib` does not take this `i`
argument, so modifying it for the sole purpose of specification seems quite
intrusive.

To overcome this issue, Gospel provides *ghost parameters*. It lets you
introduce logical arguments (or return values) that don't exist initially, so
you can use them in the specifications and hopefully make them easier to
understand.

Thus, we can rewrite our last attempt by using this feature and benefit from
`i` being an argument without actually modifying the OCaml interface or
implementation.

```ocaml
val fib : int -> int -> int -> int
(*@ r = fib [i: integer] n a b
    checks n >= 0
    requires i >= 0 /\ a = fibonacci i /\ b = fibonacci (i+1)
    ensures r = fibonacci (i+n) *)
```

We're done! Using ghost parameters allows you to write elegant and concise
contracts in places that would otherwise require complex constructs and
repetitions.
