# Gospel

[![OCaml-CI Build Status](https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2Focaml-gospel%2Fgospel%2Fmaster&logo=ocaml)](https://ci.ocamllabs.io/github/ocaml-gospel/gospel)

**Disclamer:** This project is still experimental. No support will be provided
at this point, and its behaviour is still unstable.

Gospel is a tool-agnostic formal specification language for OCaml.

## Syntax example

We briefly describe the specification language using an example, taken from a
vector interface.

The abstract type `t` of vectors below is identified as `ephemeral` (elements
can be mutated in-place) and is modeled using a polymorphic sequence, introduced
using the `mutable model` syntax. Properties about the data type and associated
models can be captured using the `invariant` keyword.

```OCaml
(** The polymorphic type of vectors.
    This is a mutable data type. *)
type 'a t
(*@ ephemeral *)
(*@ mutable model view: 'a seq *)
(*@ invariant length view <= Sys.max_array_length *)
```

To provide specification for function declarations, the parameters and the
returned value must be named first. Preconditions are stated in `requires`
clause, while postconditions are introduced after `ensures`.

```OCaml
val create: ?capacity:int -> dummy:'a -> 'a t
(*@ a = create capacity dummy
      requires let capacity = match capacity with
                 | None -> 0 | Some c -> c in
               0 <= capacity <= Sys.max_array_length
      ensures  length a.view = 0 *)
```

Whenever type `int` is mentioned, it refers to the OCaml type `int` of native
machine integer (e.g. 63-bit signed integers on a 64-bit platform). A type
`integer` for mathematical integers is also provided. Here is an example:

```OCaml
val make: ?dummy:'a -> int -> 'a -> 'a t
(*@ a = make ?dummy n x
      requires 0 <= n <= Sys.max_array_length
      ensures  length a.view = n
      ensures  forall i: integer. 0 <= i < n -> a.view[i] = x *)
```

Whenever a function has side effects, this is indicated using a `modifies`
clause. Here is an example:

```OCaml
val resize: 'a t -> int -> unit
(*@ resize a n
      checks   0 <= n <= Sys.max_array_length
      modifies a
      ensures  length a.view = n
      ensures  forall i. 0 <= i < min (length (old a.view)) n ->
                 a.view[i] = (old a.view)[i] *)
```

This last example also features the `checks` clause. This is an alternative to
`requires`. Contrary to the latter, a `checks` clause is checked at run-time,
and raises an `Invalid_argument` exception when it is not satisfied.

Last, an `equivalent` clause is sometimes used to describe the behavior of an
OCaml function using an equivalent piece of OCaml code. Here is an example:

```OCaml
val iter : ('a -> unit) -> 'a t -> unit
(*@ iter f a
      equivalent "for i = 0 to length a - 1 do f (get a i) done" *)
```

A forthcoming documentation of this specification language will hopefully
provide more details and clarify the semantics.
