---
title: FAQ
sidebar_position: 6
---

# Frequently asked questions

Your question is not answered here? Feel free to [open a
discussion](https://github.com/ocaml-gospel/gospel/discussions/new) so we can
answer it and add it here.

## Can I use my OCaml functions in specifications?

**Sometimes.** Logic and programs are two separated worlds, and using programs
in our logic can introduce inconsistencies, so you cannot use OCaml values in
your specifications. If you need to write functions as shortcuts in your
specifications, you may want to add [logical functions or
predicates](language/logical) instead.

However, some OCaml functions are also safe to use in the specifications. We
call them *pure*. You can read the [dedicated documentation
section](language/function-contracts#pure-functions) about pure values to learn
more about them.

<hr />

## Are formulae the same as OCaml Boolean expressions?

**No.** We chose a syntax close to OCaml expressions because we believe it makes
it easy for OCaml developers to use Gospel, but Gospel formulae are not OCaml
expressions, and some of them are not even executable or decidable.

The following specification is not executable, as you cannot test a property on
every possible array:

```ocaml {3}
val total_weight : weight:('a array -> int) -> ('a array) list -> int
(*@ y = total_weight ~weight l
    requires forall a. weight a >= 0
    ensures y = List.fold_left (fun acc a -> acc + weight a) 0 l *)
```

<hr />


## Are formulae typechecked by the OCaml compiler?

**No.** The OCaml compiler does not know about Gospel specifications at all, so
it simply ignores them.

**But...** we, however, provide a typechecker for Gospel annotations. It reads
your interface file and performs typechecking and some sanity checks to verify
that your specification is well-formed. You can invoke the Gospel typechecker by
invoking `gospel check` on your interface file.

<hr />

## Are formulae tested at runtime?

**No.** Gospel does not provide runtime assertion checking for your code, it can
only check if your specifications are well-formed. If you are interested in
runtime assertion checking for your code, you might want to use
[Ortac](https://github.com/ocaml-gospel/ortac) on top of Gospel.

<hr />

## Are formulae verified by Gospel?

**No.** Gospel does not feature deductive verification, it can only check if
your specifications are well-formed. External tools will let you
verify Gospel specifications though:
- [Why3gospel](https://github.com/ocaml-gospel/why3gospel) if your
  implementation is written using [Why3](http://why3.lri.fr).
- [Cameleer](https://github.com/ocaml-gospel/cameleer) if your implementation
  is written using OCaml.

<hr />

## What if I write `1 / 0` in a formula?

**You can.** Indeed, the logic of Gospel is _total_, which means that
`1 / 0` is a legal term of type `integer`. Yet, we do not know its
value, nor if its value is the same as `2 / 0`. Similarly, we can
access an array out of its bounds but the result is unspecified.

Using such unspecified terms in specifications is considered bad
practice, and is even likely to be errors in your specifications.
Note that when using runtime assertion checking tools such as
[Ortac](https://github.com/ocaml-gospel/ortac), the evaluation of
terms such as `1 / 0` will be signaled as a runtime error.

<hr />

## What are ghost arguments?

Ghost arguments are function arguments that do not exist in the OCaml interface,
but that you can add to simplify your specification. To understand how they can
be useful and how to use them, you may want to read the [Fibonacci
walk-through](walkthroughs/fibonacci).

<hr />

## What are ghost types for?

Ghost types do not exist in the original OCaml interface, but you can add some
to simplify your specification by referring to values that have no code
counterpart or are not exposed. You can read the [Union-find
example](walkthroughs/union-find) to find an example specification using this
feature.

<hr />

## Why would I need ghost values?

Why would you need ghost values, since no code can call them, right? Ghost
values will indeed never be used by programs, as they don't even exist in the
code.

However, consider an interface containing a ghost type, taken as (ghost)
argument by some of your OCaml functions:

```ocaml
(*@ type t *)

val f : int -> int
(*@ y = f [t : t] x
    pure
    (* ... *) *)
```

Now, every specification that refers to `f` needs to pass the `t` argument to
that function. You can propagate that requirement again, but you may also need
to actually instantiate such a value and pass it directly:

```ocaml
(*@ function make : unit -> t *)

val g : int -> int
(*@ y = g x
    ensures let t = make () in
            f t x = y *)
```

Of course, `make` has to be declared (here as a ghost function) first to be able
to use it in the specification of `g`.
