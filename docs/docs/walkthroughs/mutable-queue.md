---
sidebar_position: 3
---

# Mutable Queues

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

## Modeling the Queue Datastructure

Let's start by defining the type of queues. The `'a t` type represents a
polymorphic queue, storing elements of type `'a`. To enable reasoning about the
elements of a queue, we attach one model to its type declaration:

```ocaml
type 'a t
(*@ mutable model view: 'a sequence *)
```

The model `view` represents the mathematical sequence of elements stored in
the queue. The type `'a sequence` is the type of logical sequences defined in the
Gospel standard library and is for specifications only. The `mutable`
keyword states that the `view` field can change over time.

:::tip

This shows how Gospel annotations provide extra insight and are also relevant
for documentation. The `mutable` keyword states that the type `'a t` is mutable,
which cannot be deduced from its OCaml declaration alone.

:::

## Pushing Into the Queue

Now let's declare and specify a `push` operation for these queues:

```ocaml
val push: 'a -> 'a t -> unit
(*@ push v q
    modifies q
    ensures  q.view = Sequence.cons v (old q.view) *)
```

- The first line of the specification is the header; it names the two arguments
  of `push`: `v` and `q`.
- The `modifies` clause states that the function `push` may mutate the contents
  of `q`.
- Finally, the `ensures` clause introduces a postcondition that describes the
  model `view` of `q` after a call to `push`. The new `view` extends the old
  value of `view` with the element `v` at the front. We use the keyword `old` to
  refer to the value of an expression (here, `q.view`) in the prestate, *i.e.*,
  before the function call.

Note that the module `Sequence` is part of the Gospel standard library and
should not be confused with module `Seq` from the OCaml standard library.

## Various Flavors of `pop`

Now let's move to another function, `pop`, and illustrate three ways of
handling assumptions from the client in Gospel specifications.

### Exceptional Version

In this version, similar to the one provided by the OCaml standard library,
`pop` raises an `Empty` exception if its argument is an empty queue. We specify
this behaviour as follows:

```ocaml
exception Empty

val pop: 'a t -> 'a
(*@ v = pop q
    modifies q
    ensures  old q.view = q.view ++ Sequence.cons v Sequence.empty
    raises   Empty -> q.view = old q.view = Sequence.empty *)
```

We have two postconditions:

- The first one, introduced with `ensures`, states the postcondition that holds
  whenever the function `pop` returns a value `v`.
- The second one, introduced by `raises`, states the exceptional postcondition
  that holds whenever the call raises the exception `Empty`.

Similarly to the `push` case, the clause `modifies` indicates that this function
call may mutate `q`. Note that this applies to the exceptional case as well, and
that's why we state that `q` is both empty and not modified.

### Unsafe Version

Now, let us consider an unsafe variant of `pop` that should only be called on a
non-empty queue, leaving the responsibility of that property to the client code.
The function does not raise `Empty` anymore but instead expects a non-empty
argument. We can thus add the following precondition to the contract using the
keyword `requires`:

```ocaml {3}
val unsafe_pop: 'a t -> 'a
(*@ v = unsafe_pop q
    requires q.view <> Sequence.empty
    modifies q
    ensures  old q.view = q.view ++ (Sequence.cons v Sequence.empty) *)
```

### Defensive Version

Instead of assuming that the precondition is guaranteed by the caller, we can
also adopt a more defensive approach where `pop` raises `Invalid_argument`
whenever an empty queue is provided. Gospel provides a way to declare such a
behavior, using `checks` instead of `requires`:

```ocaml {3}
val pop: 'a t -> 'a
(*@ v = pop q
      checks   q.view <> Sequence.empty
      modifies q
      ensures  old q.view = q.view ++ (Sequence.cons v Sequence.empty) *)
```

The `checks` keyword means that function itself checks the pre-condition
`q.view <> empty` and raises `Invalid_argument` whenever it does not hold. Note
that `q.view` is just a logical model and may not exist at all in the
implementation. However, the function checks a property that, from a logical
point of view, results in `q.view` not being empty.

## A Small Break: `is_empty`

Our next function needs a simpler specification. Consider the following
declaration for an emptiness test, together with its contract:

```ocaml
val is_empty: 'a t -> bool
(*@ b = is_empty q
      ensures b <-> q.view = Sequence.empty *)
```

The postconditions capture that the function returns `true` if and only if the
queue is empty.

Although very simple, the above specification implies an important
property. Since no `modifies` clause is present, the argument `q` is read-only.
We know that a call to `is_empty` does not modify `q.view`.

## Creating Queues

The next function features the creation of a queue. Its declaration and
specification are as follows:

```ocaml
val create: unit -> 'a t
(*@ q = create ()
      ensures q.view = Sequence.empty *)
```

The newly created queue has no elements. Its `view` model equals the `empty`
sequence, as stated by the postcondition.

It's worth mentioning that the specification implicitly assumes `q` to be
disjoint from every previously allocated queue. This is an important design
choice of Gospel that follows this general principle: writing functions that
return non-fresh, mutable values is considered a bad practice in OCaml.

## Merging Queues

Let's conclude this specification with a function to merge two queues. Several
approaches are possible, so we'll showcase three of them.

### In-Place Transfer

Start with a concatenation that transfers all elements from one queue to
another:

```ocaml
val transfer: 'a t -> 'a t -> unit
(*@ transfer src dst
    modifies src, dst
    ensures  src.view = Sequence.empty
    ensures  dst.view = old dst.view ++ old src.view *)
```

Here the contract states that both queues are modified. The queue `src` is
emptied after the call and its elements are appended to the queue `dst`. Notice
the use of `old` in the second postcondition. This helps us refer to the queues'
state before they're passed to the function.

### Destructive Operations

One could think of a slightly different version of `transfer`:
`destructive_transfer`, which invalidates `src` when called. In other words, the
value of `src` should be considered *dirty*, so it must not be used in the
rest of the program. Such use cases are frequent in system programming, like
when dealing with file descriptors after closing them. Gospel
provides `consumes` clauses to capture this semantic:

```ocaml {3}
val destructive_transfer: 'a t -> 'a t -> unit
(*@ destructive_transfer src dst
    consumes src
    modifies dst
    ensures  dst.view = old dst.view ++ old src.view *)
```

Note that we don't need to give information on `src` in the poststate, since
its value must not be used anymore.

### A Constructive Version

Finally, perhaps a simpler version may consist in a `concat` function that
creates a fresh queue with the elements of the queues passed as arguments:

```ocaml
val concat: 'a t -> 'a t -> 'a t
(*@ new = concat q1 q2
    ensures new.view = q1.view ++ q2.view *)
```

In this version, no `modifies` appears. This means that none of the arguments
are modified by the function, so specifying their models in the poststate isn't
necessary.
