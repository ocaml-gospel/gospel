---
sidebar_position: 2
---

# Your First Specification

Let us get started with a simple specification example, and specify a generic
interface for polymorphic, limited capacity containers.

```ocaml
type 'a t
(** The type for containers. *)

exception Full

val create: int -> 'a t
(** [create capacity] is an empty container whose maximum capacity
    is [capacity]. *)

val is_empty: 'a t -> bool
(** [is_empty t] is [true] iff [t] contains no elements. *)

val clear: 'a t -> unit
(** [clear t] removes all values in [t]. *)

val add: 'a t -> 'a -> unit
(** [add t x] adds [x] to the container [t], or raises [Full] if
    [t] has reached its maximum capacity. *)

val mem: 'a t -> 'a -> bool
(** [mem t x] is [true] iff [t] contains [x]. *)
```

Gospel specifications live in special comments, starting with the `@` character.
These comments may be attached to type declarations or value declarations. They
provide a specification for the signature item they are attached to.

## Models and Invariants for `'a t`

Let's start by specifying the abstract type `'a t`. As a container with fixed
capacity, we can model it with two pieces of information: a fixed integer
capacity and a set of `'a` values, representing its contents. Note that the
capacity is not mutable even though the contents are. This logical model of
the container directly translates into Gospel:

```ocaml
type 'a t
(** The type for containers. *)
(*@ model capacity: int
    mutable model contents: 'a set *)
```

Notice that documentation comments and Gospel specifications can coexist and
often even help understand each other! However, for the sake of brevity, we will
omit docstrings in the rest of this section.

One may also note that the capacity must be positive and that the number of values
in the `contents` set may not exceed `capacity`. Those are type invariants:

```ocaml
type 'a t
(*@ model capacity: int
    mutable model contents: 'a set
    with t
    invariant t.capacity > 0
    invariant Set.cardinal t.contents <= t.capacity *)
```

The `Set` module is part of the [Gospel standard library](../stdlib). Although
it tries to mimic familiar interfaces from the OCaml standard library, those two
should not be confused. Only logical declarations can appear in specifications!

Now that we annotated our type with its models and invariants, we can attach
specifications to the functions to show how they interact with the container.

## Your First Function Contract: `create`

The function `create` returns a container when provided a capacity. We may want
to specify three pieces of information:

- The provided capacity is positive.
- The returned container's capacity is indeed the one received as an
  argument.
- The container is empty.

Let's write a Gospel formalisation of that contract. The contract starts with a
header that names the arguments and the return value. We'll call the argument
`c` and the return value `t`. Now we can mention them in the rest of the
specification. The first property is a precondition of the function (we use the
keyword `requires`), while the second and third ones are post-conditions (the
keyword is `ensures`):

```ocaml
val create: int -> 'a t
(*@ t = create c
    requires c > 0
    ensures t.capacity = c
    ensures t.contents = Set.empty *)
```

## Simple Accessors: `is_empty` and `mem`

Now on to `is_empty` and `mem`.

`is_empty t` is true if and only if `t` is empty; this is a post-condition. This
function also (hopefully) has no side-effect: it doesn't modify `t`,
depend on any internal state, or raise exceptions. In Gospel's
language, this function is *pure*.

```ocaml
val is_empty: 'a t -> bool
(*@ b = is_empty t
    pure
    ensures b <-> t.contents = Set.empty *)
```

The specification for `mem` is similar:

```ocaml
val mem: 'a t -> 'a -> bool
(*@ b = mem t x
    pure
    ensures b <-> Set.mem x t.contents *)
```

## Mutating Arguments and Raising Exceptions

Finally, let's specify `clear` and `add`, which are functions that mutate the
container.

The function `clear` removes all elements from its argument, meaning it's empty
after the call. Obviously, it modifies the `contents` model of its argument.
After its execution, the container should be empty. Note that we are only
allowed to mention `is_empty` in the specification because it's a *pure*
function. Attempting to use a non-pure OCaml function in a specification will
result in a Gospel error.

```ocaml
val clear: 'a t -> unit
(*@ clear t
    modifies t.contents
    ensures is_empty t *)
```

A first attempt at specifying `add` is similar to the previous examples. We use
Gospel's `old` primitive to refer to the container's state prior to the
function execution:

```ocaml
val add: 'a t -> 'a -> unit
(*@ add t x
    modifies t.contents
    ensures t.contents = Set.add x (old t.contents) *)
```

However, notice that this specification is incomplete. One specificity of this
function is that it can raise `Full`, so let's complete that contract with this
piece of information. If `add` raises `Full`, we can deduce that `t.contents`
already contains `t.capacity` elements.

```ocaml
exception Full
val add: 'a t -> 'a -> unit
(*@ add t x
    modifies t.contents
    ensures t.contents = Set.add x (old t.contents)
    raises Full -> Set.cardinal (old t.contents) = t.capacity
                /\ t.contents = old t.contents *)
```

Since we have a `modifies` clause, the contents of `t` may be mutated even when
`Full` is raised. The last line of specification forbids such a behavior.

Notice how we didn't need to repeat that `S.cardinal t.contents <= t.capacity`
in every contract. As a type invariant, this property implicitly holds in every
function's prestate and poststate.

## Type-Checking Your Specification

We're done! Our module interface is fully specified and independent of any
implementation. The full example is available in
[container.mli](./container.mli) in case you want to play with it.
Let's finish by verifying that these are well-typed and call Gospel's
type-checker:

```shell
$ gospel check ./container.mli
```
