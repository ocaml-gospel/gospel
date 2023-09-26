---
sidebar_position: 7
---

# Logical Declarations

## Functions, Predicates and Axioms

It's often convenient to introduce shortcuts for terms and formulas to avoid
repetitions. *Predicates* let you write named formulae definitions in Gospel
comments. Here is a typical example:

```ocaml
(*@ predicate is_sorted (a: int array) =
      forall i j. 0 <= i <= j < Array.length a
                  -> a.(i) <= a.(j) *)
```

We can then reuse the predicate `is_sorted` inside any Gospel annotations such
as function contracts:

```ocaml
val merge: int array -> int array -> int array
(*@ c = merge a b
    requires is_sorted a
    requires is_sorted b
    ensures is_sorted c *)
```

Similarly, one can define a shortcut for terms using Gospel's *functions*.

```ocaml
(*@ function powm (x y m: integer) : integer = mod (pow x y) m *)
```

Both predicate definitions and function definitions may be
recursive. A recursive definition requires the `rec` keyword like in OCaml:

```ocaml
(*@ predicate rec is_sorted_list (l: int list) = match l with
      | [] | _ :: [] -> true
      | h :: (y :: _ as t) -> h <= y /\ is_sorted_list t *)
```

Logical functions and predicates can be left uninterpreted, ie without declaring
their bodies, for instance:

```ocaml
(*@ predicate is_sorted (a: int array) *)
(*@ function powm (x y m: integer) : integer *)
```

Finally, it is also possible to define *axioms*, for instance:

```ocaml
(*@ axiom unbounded_integers : forall i. exists j. j > i *)
```

## Logical Function Contracts

Similarly to OCaml functions, contracts can be added to logical declarations.

```ocaml {3}
(*@ function rec fibonacci (n: integer) : integer =
      if n <= 1 then n else fibonacci (n-2) + fibonacci (n-1) *)
(*@ requires n >= 0 *)
```

Such a contract doesn't prevent you from calling `fibonacci` on negative
integers. For instance, `fibonacci (-1)` is a valid Gospel term. However, we
know nothing about its value: the definition of `fibonacci` holds only when its
precondition is true.

The above is equivalent to an uninterpreted function together with an axiom, as
follows:

```ocaml
(*@ function fibonacci (n: integer) : integer *)

(*@ axiom fibonacci_def : forall n. n >= 0 ->
      fibonacci n =
        if n <= 1 then n
        else fibonacci (n-2) + fibonacci (n-1) *)
```

Logical symbols can also come with postconditions. For instance, we can assert
that Fibonacci numbers are non-negative:

```ocaml {4}
(*@ function rec fibonacci (n: integer) : integer =
      if n <= 1 then n else fibonacci (n-2) + fibonacci (n-1) *)
(*@ requires n >= 0
    ensures result >= 0 *)
```

:::info

Note that as opposed to OCaml function contracts, logical function contracts don't
have a header. Consequently, Gospel automatically introduces a variable called `result`
in the context to refer to the value returned by the
function in a postcondition.

:::

The postcondition of `fibonacci` is equivalent to adding an axiom along with an
uninterpreted counterpart.

```ocaml
(*@ axiom fibonacci_post : forall n. n >= 0 -> fibonacci n >= 0 *)
```

Note that the postcondition holds only when the precondition holds.

:::danger

Gospel doesn't perform any verification beyond type-checking. If you wish to
verify that the definition indeed complies with its contract, you must use an
external tool such as [Why3Gospel](https://github.com/ocaml-gospel/why3gospel).

:::

## Termination Arguments

Using recursive definitions in the logical domain can introduce inconsistencies.
For instance, consider the following recursive function:

```ocaml
(*@ function rec f (n: integer): integer = f n + 1 *)
```

As explained above, it's perfectly fine to mention `f 0` in a formula. Although
we don't know its value, we know that `f 0 = f 0 + 1`, thus `0 = 1`, which is
obviously inconsistent.

In order to prevent this, it's a good practice to provide a termination
argument for each recursive definition. Gospel provides one way of doing this
via *variants*.

```ocaml {4}
(*@ function rec fibonacci (n: integer) : integer =
      if n <= 1 then n else fibonacci (n-2) + fibonacci (n-1) *)
(*@ requires n >= 0
    variant n *)
```

:::danger

Similarly to contracts, Gospel doesn't perform any verification that the
variant indeed ensures the termination, it will only checks that it's of type
integer. It's up to an external tool to exploit the expression given as
`variant` to check termination.

:::

