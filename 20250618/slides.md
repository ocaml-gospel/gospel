
# Table of Contents

1.  [First class functions](#orgf53d8d6)
2.  [Pattern Matching](#orgfb3c52c)
3.  [Updated syntax](#org3af9d5b)
4.  [Updated Syntax (Exceptional Specifications)](#orgf69913d)
5.  [Problem](#org470b5b2)
6.  [Prop and Bool](#org6131e4a)


<a id="orgf53d8d6"></a>

# First class functions

When using OCaml values in Gospel specifications, we use their logical
representation.

What should the logical representation of a function be?

Preliminary solution: functions passed as arguments are assumed to be
pure and can be used directly in functions if their arguments and
return values are isomorphic to their Gospel models.

    val init : int -> (int -> int) -> int list
    (*@
        requires x >= 0
        deterministic f
        { True } f a { \lambda r. r = f' a }
        let l = init x f [f' : integer -> integer] in
          ensures l = Sequence.init x f' *)

This example wouldn't work since `int` is not isomorphic.


<a id="orgfb3c52c"></a>

# Pattern Matching

Right now, pattern matching no longer exists in Gospel.  This means
that we need a way to reason over the following data types:

-   Tuples
    We can use `let x, y, z = (x, y, z) in t` to destruct tuples.

-   The `option` type
    We can use `let* x = t1 in t2` where `t1` must have type `'a option`.
    This is sugar for `\forall x, t1 = Some x -> t2`

-   The `result` type
    ?

Keep this for now and see if there are any examples where pattern
matching is needed.


<a id="org3af9d5b"></a>

# Updated syntax

Syntax for Gospel specifications has been overhauled so that it can
permit nested exceptional specifications:

    type 'a stack
    (*@ model : 'a sequence *)

    val pop : 'a stack -> 'a
    (*@ x = pop s
        modifies s
        requires s <> Sequence.empty
        ensures old s = Sequence.cons x s *)

    val pop : 'a stack -> 'a
    (*@ modifies s
        requires s <> Sequence.empty
        let x = pop s in
          ensures old s = Sequence.cons x s *)


<a id="orgf69913d"></a>

# Updated Syntax (Exceptional Specifications)

    val pop : 'a stack -> 'a
    (*@ x = pop s
        modifies s
        raises Not_found -> s = Sequence.empty
        ensures ... *)

    val pop : 'a stack -> 'a
    (*@ x = pop s
        modifies s
        ensures ...
        raised Not_found
          ensures s = Sequence.empty
        raised ...
          ... *)


<a id="org470b5b2"></a>

# Problem

We do not have good way of saying "This function raises an exception,
but we do not specify its behaviour".  For example, with the old
syntax, we could say:

    val pop : 'a stack -> 'a
    (*@ x = pop s
        modifies s
        raises Not_found
        ensures ... *)

With the updated syntax we would need to say:

    val pop : 'a stack -> 'a
    (*@ modifies s
        match pop s with
        |Not_found -> ensures true
        |x -> ... *)

Is there a better way?


<a id="org6131e4a"></a>

# Prop and Bool

We now have an explicit distinction between `prop` and `bool`.  This means
that operators such as `/\`, `\/`, `->`, etc&#x2026; only work for values of type
`prop`.  For values of type `bool`, we use `||` and `&&`.

What about `if`?  Should it work for `prop` or `bool`?  Should we have two
`if`'s?
