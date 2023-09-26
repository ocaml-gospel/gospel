---
sidebar_position: 4
---

# Union-find

In this example, we will highlight an advanced use case of ghost declarations in
Gospel through the specification of a union-find datastructure.

[Union-find](https://en.wikipedia.org/wiki/Disjoint-set_data_structure) is a
fairly simple datastructure used to keep track of a collection of elements
partitioned into disjoint sets. Its most simple interface is as follows:

```ocaml
type 'a element
(** The type for set elements. *)

val make : 'a -> 'a element
(** [make x] is the singleton containing an element wrapping [x]. *)

val find : 'a element -> 'a element
(** [find e] is the representative of the subset containing [e]. *)

val union : 'a element -> 'a element -> unit
(** [union x y] merges the subset containing [x] with the subset
    containing [y]. *)
```

## Why is this hard to specify?

Union-find structures store a partition of a set. Notice that a typical
interface for union-find doesn't materialise this greater set (let's call it
our *universe*). Instead, the programmer has access to individual elements of
the set, so they may merge them or access a representative of its partition.

However, for the sake of specification, not having this global set is a problem.
How can we state that the subsets are disjoint? How do we refer to the set of
elements that exist in the union-find universe? Can we even tell that the
representative of an element (returned by `find`) is indeed part of a subset
that was `union`ed with it at some point? It seems that we cannot do any of
these by attaching contracts to our functions and type only.

This example shows how Gospel's *ghost declarations* help describe such complex
behaviours.

 ## Introducing the Ghost Universe

Seemingly, the root of our problems is that union-find relies on the implicit
universe of elements. It doesn't exist in the OCaml's interface, so let's
introduce it as a ghost-type declaration. We can also add that this universe is
mutable. As long as elements are added, for instance, it will be modified.

```ocaml
(*@ type 'a universe *)
(*@ ephemeral *)
```

Now, our three functions still apply to set elements, but they also apply _in
the context of a universe_ since you create a new singleton subset in the
context of the greater set, or find the representative of an element in the rest
of the universe. This translates into our three functions taking a value of type
`'a universe` as its argument. Of course, `'a universe` is a ghost type and you
don't want to modify the functions' signatures anyway, so this argument is
ghost, too[^1]. We already get a sense that `make` and `union` will modify the
universe.

[^1]: If you're not comfortable with ghost arguments, you may want to [read our
      Fibonacci walk-through](fibonacci) first.

```ocaml
val make : 'a -> 'a element
(*@ e = make [u: 'a universe] v
    modifies u *)

val find : 'a element -> 'a element
(*@ e = find [u: 'a universe] x *)

val union : 'a element -> 'a element -> unit
(*@ union [u: 'a universe] x y
    modifies u *)
```

Since we now have a type for universes, and functions that take values of such
a type, we probably need a constructor for this type. Let's introduce that as a
ghost value:

```ocaml
(*@ val make_universe: unit -> 'a universe *)
```

Having this abstract type on is not very useful so far. We're able to state
that the functions apply in the context of a universe and they may mutate
it, but that's pretty much it. Let's be more precise than that.

## Elements: Gotta Catch 'em All

The first interesting property to capture is that the subsets
are indeed disjoint. For instance, `make` shouldn't create an element that's
already in the universe, otherwise you'd have two different subsets
containing the same element.

In order to specify this, we need to talk about the set of existing
elements in the universe. Let's introduce this as a logical model attached to
our `universe` type:

```ocaml
(*@ type 'a universe *)
(*@ mutable model dom : 'a element set *)
```

:::tip

Since at least one model is mutable, we may now omit the `ephemeral` keyword
(although it's also valid to keep it if you prefer). For instance, you may keep
it if you want to indicate that the type is also mutable in a way that is not
visible in the models.

:::

Now let's add more information on how the functions interact with it. The
constructor should ensure two things:
- The returned element is a fresh element.
- The universe's domain is augmented with the singleton containing the
  provided value.

```ocaml {4,5}
val make : 'a -> 'a element
(*@ e = make [u: 'a universe] v
    modifies u.dom
    ensures not (Set.mem e (old u.dom))
    ensures u.dom = Set.add e (old u.dom) *)
```

The `find` function obviously needs an element of the universe, and it also
returns an element that's part of the universe:

```ocaml {3,4}
val find : 'a element -> 'a element
(*@ e = find [u: 'a universe] x
    requires Set.mem x u.dom
    ensures Set.mem e u.dom *)
```

Finally, `union` requires that the provided elements are part of the universe
too. Please note: it doesn't modify the universe domain (no element is added nor
removed); however, since we added the `modify u` clause, we need to state that
explicitly, or the contract may imply that `u.dom` was modified (in an
unspecified way).

```ocaml {3-4,6}
val union : 'a element -> 'a element -> unit
(*@ union [u: 'a universe] x y
    requires Set.mem x u.dom
    requires Set.mem y u.dom
    modifies u
    ensures u.dom = old u.dom *)
```

## Find Your Representative

We can now talk about all the elements in our partition, but we still haven't
mentioned the elements' representatives. Each element in the universe has a
representative in the set, so we can represent this using a `'a element -> 'a
element` function. We can also add two invariants:

- The representative of an element must live in the same universe as the
  element itself.
- The `rep` function is idempotent: the representative of an element is its own
  representative.

```ocaml {3-5}
(*@ type 'a universe *)
(*@ mutable model dom : 'a element set
    mutable model rep : 'a element -> 'a element
    with u
    invariant forall e. Set.mem e u.dom -> Set.mem (u.rep e) u.dom
    invariant forall e. Set.mem e u.dom -> u.rep (u.rep e) = u.rep e *)
```

:::caution

Notice how in both cases, speaking of the element's representative only makes
sense for elements that live in the universe. However, Gospel's logic is total,
so `rep` is also defined outside of the universe; however, it's unspecified
there.

:::

Now let's add clauses to our functions that indicate how they interact with
`rep`. After a call to our constructor, the created element is obviously its own
representative, and all other representatives are left unchanged:

```ocaml {6}
val make : 'a -> 'a element
(*@ e = make [u: 'a universe] v
    modifies u.dom
    ensures not (Set.mem e (old u.dom))
    ensures u.dom = Set.add e (old u.dom)
    ensures u.rep = (old u.rep)[e -> e] *)
```

:::note

The `_[_ -> _]` operator is defined in the [standard library](../stdlib). The
notation `f[x -> y]` is a shorthand notation for the function defined as `fun i
-> if i = x then y else f i`

:::

The `find` function doesn't modify the representatives, but they return the
input element's representative:

```ocaml {5}
val find : 'a element -> 'a element
(*@ e = find [u: 'a universe] x
    requires Set.mem x u.dom
    ensures Set.mem e u.dom
    ensures e = u.rep x *)
```

Finally, the postcondition for `union` is a bit more involved. We need to
capture that the new representative of an element may change.

First, it's left unchanged if the element is not in `x` nor `y` subset.

Let's start by introducing a predicate that will help us decide if two elements
are in the same subset (or equivalence class). This is the case if and only if
they have the same subset representative:

```ocaml
(*@ predicate equiv (u: 'a universe) (x y: 'a element) =
      u.rep x = u.rep y *)
```

We can now use this to state that elements that aren't equivalent to `x` or `y`
have the same representative:

```ocaml {7-9}
val union : 'a element -> 'a element -> unit
(*@ union [u: 'a universe] x y
    requires Set.mem x u.dom
    requires Set.mem y u.dom
    modifies u
    ensures u.dom = old u.dom
    ensures forall e.
      not (old (equiv u x e \/ equiv u y e))
      -> u.rep e = old (u.rep e) *)
```

And finally, elements that were in the unioned subsets now must all have the
same representative, and that element is either the old `x` representative, or
the old `y` representative.

```ocaml {9-13}
val union : 'a element -> 'a element -> unit
(*@ union [u: 'a universe] x y
    requires Set.mem x u.dom
    requires Set.mem y u.dom
    modifies u
    ensures u.dom = old u.dom
    ensures forall e. not (old (equiv u x e \/ equiv u y e))
                      -> u.rep e = old (u.rep e)
    ensures exists r. (r = old (u.rep x) \/ r = old (u.rep y))
      /\ forall e. old (equiv u x e \/ equiv u y e)
                   -> u.rep e = r *)
```

We could go further and add more functions, like an equality function
over elements or a `get` function to extract an element's value,
but we'll keep it there for this tutorial. Hopefully, this gives you a
better overview of the purpose of ghost types in Gospel specifications and how
they can help you refer to meta-elements not present or not exposed in the code.
