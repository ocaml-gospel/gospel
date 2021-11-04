---
sidebar_position: 4
---

# Union-find

In this example, we will highlight an advanced use-case of ghost declarations in
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
interface for union-find does not materialise this greater set (let us call it
our *universe*). Instead, the programmer has access to individual elements of
the set, and may merge them or access a representative of the partition it is
in.

However, for the sake of specification, not having this global set is a problem.
How can we state that the subsets are disjoint? How do we refer to the set of
elements that exist in the union-find universe? Can we even tell that the
representative of an element (returned by `find`) is indeed part of a subset
that was `union`ed with it at some point? It seems that we cannot do any of this
by attaching contracts to our functions and type only.

This example shows how Gospel's *ghost declarations* help describe such complex
behaviours.

## Introducing the ghost universe

Seemingly, the root of our problems is that union-find relies on the implicit
universe of elements. It does not exist in the OCaml's interface, so let us
introduce it as a ghost type declaration. We can also add that this universe is
mutable: so long as elements are added, for instance, it will be modified.

```ocaml
(*@ type 'a universe *)
(*@ ephemeral *)
```

Now, our three functions still apply to set elements, but they also apply _in the
context of an universe_: you create a new singleton subset in the context of the
greater set, or find the representative of an element in the rest of universe
for instance. This translates into our three functions taking a value of type `'a
universe` as argument. Of course, `'a universe` is ghost, and you do not want to
modify the signature of the functions anyway, so this argument is ghost too[^1].
We can also already get a sense that `make` and `union` will modify the
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
type, we probably need a constructor for this type. Let us introduce that as a
ghost value:

```ocaml
(*@ val make_universe: unit -> 'a universe *)
```

Having this abstract type on is not very useful so far. We are able to state
that the functions apply in the context of a universe, and that they may mutate
it, but that's pretty much it. Let us be more precise than that.

## Elements: gotta catch 'em all

A first interesting property that we would like to capture is that the subsets
are indeed disjoint. For instance, `make` should not create an element that is
already in the universe, otherwise you would have two different subsets
containing the same element.

In order to specify this, we need to be able to talk about the set of existing
elements in the universe. Let us introduce this as a logical model attached to
our `universe` type:

```ocaml
(*@ type 'a universe *)
(*@ mutable model dom : 'a element set *)
```

:::tip

Since at least one model is mutable, we may now omit the `ephemeral` keyword,
although it is valid to keep it if you prefer. For instance, you may keep it if
you want to indicate that the type is also mutable in a way that is not visible
in the models.

:::

Now let us add more information on how the functions interact with it. The
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

The `find` function obviously needs an element of the universe, and also returns
an element that is part of the universe:

```ocaml {3,4}
val find : 'a element -> 'a element
(*@ e = find [u: 'a universe] x
    requires Set.mem x u.dom
    ensures Set.mem e u.dom *)
```

Finally, `union` requires that the provided elements are part of the universe
too. Note that it does not modify the universe domain (no element is added nor
removed), but since we added the `modify u` clause, we need to state that
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

## Find your representative

We can now talk about all the elements in our partition, but we still haven't
mentioned elements representatives at all. Each element in the universe has a
representative in the set, so we may represent this using a `'a element -> 'a
element` function. We can also add two invariants:

- the representative of an element must live in the same universe as the
  element itself.
- the `rep` function is idempotent: the representative of an element is its own
  representative.

```ocaml {3-5}
(*@ type 'a universe *)
(*@ mutable model dom : 'a element set
    mutable model rep : 'a element -> 'a element
    invariant forall e. Set.mem e dom -> Set.mem (rep e) dom
    invariant forall e. Set.mem e dom -> rep (rep e) = rep e *)
```

:::caution

Notice how in both cases, speaking of the representative of an element only
makes sense for elements that are in the universe. However, Gospel's logic is
total, so `rep` is also defined outside of the universe, but it is unspecified
there.

:::

Let us now add clauses to our functions to indicate how they interact with
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

The `find` function does not modify the representatives, but return the
representative of the input element:

```ocaml {5}
val find : 'a element -> 'a element
(*@ e = find [u: 'a universe] x
    requires Set.mem x u.dom
    ensures Set.mem e u.dom
    ensures e = u.rep x *)
```

Finally, the postcondition for `union` is a bit more involved. We need to
capture that the new representative of an element may change.

First, it is left unchanged if the element is not in `x` nor `y` subset.

Let's start by introducing a predicate that will help us decide if two elements
are in the same subset (or equivalence class). This is the case iff they have
the same subset representative:

```ocaml
(*@ predicate equivalent (u: 'a universe) (x y: 'a element) =
      u.rep x = u.rep y *)
```

We can now use that to state that elements that are not equivalent to `x` or `y`
have the same representative:

```ocaml {7-9}
val union : 'a elements -> 'a elements -> unit
(*@ union [u: 'a universe] x y
    requires Set.mem x u.dom
    requires Set.mem y u.dom
    modifies u
    ensures u.dom = old u.dom
    ensures forall e.
      not (old (equivalent u x e \/ equivalent u y e))
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
    ensures forall e. not (old (equivalent u x e \/ equivalent u y e))
                      -> u.rep e = old (u.rep e)
    ensures exists r. (r = old (u.rep x) \/ r = old (u.rep y))
      /\ forall e. old (equivalent u x e \/ equivalent u y e)
                   -> u.rep e = r *)
```

We could go further and add more functions, for instance an equality function
over `element`s, or a `get` function to extract the value contained in an
element, but we'll keep it there for this tutorial. Hopefully, you now have a
better overview of the purpose of ghost types in Gospel specifications and how
they can help you refer to meta-elements that are not present in the code, or
not exposed.
