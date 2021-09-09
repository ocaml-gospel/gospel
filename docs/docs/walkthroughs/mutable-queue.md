---
sidebar_position: 3
---

# Mutable queues

This example aims to provide a formal specification for a mutable queue data
structure, similar to OCaml's standard library `Queue`.

This work is adapted from the article _GOSPEL -Providing OCaml with a Formal
Specification Language_[^1], which also
features an implementation of the following specification that was formally
verified using Why3gospel.

[^1]: Arthur Charguéraud, Jean-Christophe Filliâtre, Cláudio Lourenço, Mário
    Pereira. GOSPEL -Providing OCaml with a Formal Specification Language. FM
    2019 - 23rd International Symposium on Formal Methods, Oct 2019, Porto,
    Portugal. ⟨[hal-02157484](https://hal.inria.fr/hal-02157484)⟩

## Modeling the queue datastructure

Let's start by defining the type of queues: the `'a t` type represents a
polymorphic queue, storing elements of type `'a`. To enable reasoning about the
elements of a queue, we attach one model to its type declaration:

```ocaml
type 'a t
(*@ mutable model view: 'a seq *)
```

The model `view` represents the mathematical sequence of elements stored in
the queue. The type `'a seq` is the type of logical sequences defined in the
Gospel standard library and is for specifications only. The `mutable`
keyword states that the `view` field can change over time.

:::tip

This shows how Gospel annotations provide extra insight and are also relevant
for documentation: the `mutable` keyword states that the type `'a t` is mutable,
which cannot be deduced from its OCaml declaration alone.

:::

## Pushing into the queue

Let us now declare and specify a `push` operation for these queues:

```ocaml
val push: 'a -> 'a t -> unit
(*@ push v q
    modifies q
    ensures  q.view = Seq.cons v (old q.view) *)
```

- The first line of the specification is the header; it names the two arguments
  of `push`: `v` and `q`.
- The `modifies` clause states that the function `push` may mutate the contents
  of `q`.
- Finally, the `ensures` clause introduces a post-condition that describes the
  model `view` of `q` after a call to `push`: the new `view` extends the old
  value of `view` with the element `v` at the front. We use the keyword `old` to
  refer to the value of an expression (here, `q.view`) in the pre-state, *i.e.*,
  before the function call.

Note that the module `Seq` is part of the Gospel standard library and should not
be confused with module `Seq` from the OCaml standard library.

## Various flavors of `pop`

Let us now move to another function, `pop`, and illustrate three ways of
handling assumptions from the client in Gospel specifications.

### Exceptional version

In this version, similar to the one provided by the OCaml standard library,
`pop` raises an `Empty` exception if its argument is an empty queue. We specify
this behaviour as follows:

```ocaml
exception Empty

val pop: 'a t -> 'a
(*@ v = pop q
    modifies q
    ensures  old q.view = q.view ++ Seq.cons v empty
    raises   Empty -> q.view = old q.view = empty *)
```

We have two post-conditions:

- The first one, introduced with `ensures`, states the post-condition that holds
  whenever the function `pop` returns a value `v`.
- The second one, introduced by `raises`, states the exceptional post-condition
  that holds whenever the call raises the exception `Empty`.

Similarly to the `push` case, the clause `modifies` indicates that this function
call may mutate `q`. Note that this applies to the exceptional case as well, and
that's why we state that `q` is both empty and not modified.

### Unsafe version

Now, let us consider an unsafe variant of `pop` that should only be called on a
non-empty queue, leaving the responsibility of that property to the client code.
The function does not raise `Empty` anymore but instead expects a non-empty
argument. We can thus add the following pre-condition to the contract using the
keyword `requires`:

```ocaml {3}
val unsafe_pop: 'a t -> 'a
(*@ v = pop q
    requires q.view <> empty
    modifies q
    ensures  old q.view = q.view ++ (Seq.cons v empty) *)
```

### Defensive version

Instead of assuming that the precondition is guaranteed by the caller, we can
also adopt a more defensive approach where `pop` raises `Invalid_argument`
whenever an empty queue is provided. Gospel provides a way to declare such a
behavior, using `checks` instead of `requires`:

```ocaml {3}
val pop: 'a t -> 'a
(*@ v = pop q
      checks   q.view <> empty
      modifies q
      ensures  old q.view = q.view ++ (Seq.cons v empty) *)
```

The `checks` keyword means that function itself checks the pre-condition
`q.view <> empty` and raises `Invalid_argument` whenever it does not hold. Note
that `q.view` is just a logical model and may not exist at all in the
implementation. However, the function checks a property that, from a logical
point of view, results in `q.view` not being empty.

## A small break: `is_empty`

Our next function needs a simpler specification. Consider the following
declaration for an emptiness test, together with its contract:

```ocaml
val is_empty: 'a t -> bool
(*@ b = is_empty q
      ensures b <-> q.view = empty *)
```

The post-conditions captures that the function returns `true` if and only if the
queue is empty.

Although very simple, note that the above specification implies an important
property: since no `modifies` clause is present, the argument `q` is read-only:
we know that a call to `is_empty` does not modify `q.view`.

## Creating queues

The next function features the creation of a queue. Its declaration and
specification are as follows:

```ocaml
val create: unit -> 'a t
(*@ q = create ()
      ensures q.view = empty *)
```

The newly created queue has no elements: its `view` model equals the `empty`
sequence, as stated by the post-condition.

It is worth mentioning that the specification implicitly assumes `q` to be
disjoint from every previously allocated queue. This is an important design
choice of Gospel that follows a rule of thumbs: writing functions that return
non-fresh, mutable values is considered bad practice in OCaml.

## Merging queues

Let us conclude this specification with a functions to merge two queues. Several
approaches are possible; we will showcase three of them.

### In-place transfer

Let us start with a concatenation that transfers all elements from one queue to
another, with the following specification:

```ocaml
val transfer: 'a t -> 'a t -> unit
(*@ transfer src dst
    modifies src, dst
    ensures  src.view = empty
    ensures  dst.view = old dst.view ++ old src.view *)
```

Here, the contract states that both queues are modified. The queue `src` is
emptied after the call and its elements are appended to the queue `dst`. Notice
the use of `old` in the second post-condition, which helps us refer to the state
of the queues before they are passed to the function.

### Destructive operations

One could think of a slightly different version of `transfer`,
`destructive_transfer`, that invalidates `src` when called. In other words, the
value of `src` should be considered dirty and must not be used anymore in the
rest of the program. Such use-cases are frequent in system programming, for
instance when dealing with file descriptors after `fclose` calls. Gospel
provides `consumes` clauses to capture this semantic:

```ocaml {3}
val destructive_transfer: 'a t -> 'a t -> unit
(*@ destructive_transfer src dst
    consumes src
    modifies dst
    ensures  dst.view = old dst.view ++ old src.view *)
```

Note that we do not need to give informations on `src` in the post-state, since
its value must not be used anymore.

### A constructive version

Finally, a perhaps simpler version may consist in a `concat` function creating a
fresh queue with the elements of the queues passed as arguments:

```ocaml
val concat: 'a t -> 'a t -> 'a t
(*@ new = concat q1 q2
    ensures new.view = q1.view ++ q2.view *)
```

In this version, no `modifies` appears, meaning that none of the arguments are
modified by the function, thus specifying their model in the post-state is not
necessary.
