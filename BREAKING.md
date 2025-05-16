# Ownership clauses


<a id="orgf92a25c"></a>

## Valid values in ownership clauses

Ownership clauses no longer allow non-mutable OCaml values.  This
includes the values of the following types:

-   Abstract types that are not annotated with `mutable`.
-   Record types where none of the fields are annotated as mutable.
-   Record types where none of the fields use mutable types.
-   Type name applications where the type is not mutable nor are any of
    its arguments.
-   Tuples where none of their components are mutable
-   Arrow types.
-   Type variables.

There cannot be duplicate values in ownership clauses.


<a id="org0783e4a"></a>

## Duplicates

There cannot be any duplicates in any ownership clauses.  This means
that if a value is present in a `consumes` clause, for example, it
cannot also be defined in a `modifies` or `preserves` clause.


<a id="orgae7d1c4"></a>

## Global values

Global values, like before, can be inserted in a Gospel ownership
clause.  The only difference is that any global value that is not in
an ownership clause cannot be used within the specification.


<a id="org273c2c2"></a>

# Syntax

Specification headers are now inserted in the "middle" of the
specification instead of the beginning.  This means that any existing
Gospel specification with a header is invalid.  More specifically,
before we define the names of the function variables, we will write
any `modifies`, `preserves`, `requires`, `pure` and `diverges` clauses.  After
this, there are three possibilities:

-   We have a `let ... in` where we bind the names of the arguments and
    return variables.
    -   If there are no more clauses following the `let`, the user has to
        omit the `in`.
    -   The user can either bind several return values if the function
        returns a tuple or only bind one.
    -   The user can use wildcard patterns `_` and unit patterns `()` on
        return values.

-   We have a `match ... with` where we bind the names of the arguments
    variables.  This construct is used when we want to have exceptional
    specifications.
    -   The exceptional cases have a similar syntax to their OCaml
        counterparts: `|exception E args -> ...` where the three dots are
        postconditions.  There can only be at most one such case for each
        exception.

    -   The non-exceptional case can bind variables in the same way one
        can with a normal `let`.  There can be at most one such case.

-   We can also not have anything else: for example, if the user just
    wants to write `modifies x`, where `x` is a top level variable, they do
    not have to write a header.


<a id="orgff52bc5"></a>

# OCaml names

*Mostly copy-pasted from my previous post, the last point is new*
OCaml names cannot be used in Gospel terms.  Exceptions to this rule:

-   OCaml type variables can be used in Gospel terms, although the
    semantics of this are still a bit unsatisfactory.

-   When we have an OCaml type with a named Gospel model, a Gospel
    record type is added with the same fields.  For example:

        type 'a array
        (*@ mutable
            model contents : 'a sequence
            model length : integer *)

    Is sugar for:

        (*@ type 'a array = { contents : 'a sequence; length : integer } *)

        type 'a array
        (*@ mutable model : 'a array *)

    This means that the `array` type is available in Gospel, but we can
    only access the `contents` and `length` field.  Another consequence is
    that we are not allowed to state that only the `contents` is
    mutable and not the `length`.

-   OCaml variables can be used within the specification, but only as
    their models.  For example:

        val length : int array -> int
        (*@ let r = add arr in
              ensures x.length = r *)

    In this specification (assuming the `model` of `int` is `integer`), `arr`
    is of type `integer array` (where `array` is the `Gospel` record type)
    and `r` is of type `integer`.  If we use in a specification a value of
    a type that does not have a Gospel `model`, we raise an exception.

-   OCaml top level values can be used within specifications but only
    if they are listed in an ownership clause.  For example.

        val top_array : int array

        val top_length : unit -> int
        (*@ preserves top_array
              let r = top_length n in
              ensures r = top_array.length *)

    Is valid, however, if we were to remove the `preserves` clause, this
    would raise an exception.  As before, the type of `top_array` within
    the Gospel specification corresponds to its `model`.

-   OCaml modules can be used within Gospel terms, but only to access
    valid variables as listed previously.

-   Opening an OCaml module in a Gospel comment exposes only the Gospel
    names defined in that module, whereas opening an OCaml module in the
    OCaml code exposes both the Gospel and OCaml names.


<a id="orgc7aa3a7"></a>

# Gospel names

Gospel names can **never** be used in OCaml code.  This is more of a bug
fix but since it could potentially invalidate some (very bad!) Gospel
specifications I listed it here.


<a id="orge728ed7"></a>

# Terms

-   The `old` tag cannot be used on OCaml values that do not exist
    preconditions (e.g. return values).  I don't really think this is a
    good idea but it was the easiest way to implement it and we can
    change it later.

-   Pattern matching is not supported


<a id="org70cb7e5"></a>

# Types

-   The only supported type declarations are abstract and record types.
-   Type invariants for OCaml types are only allowed to refer to the
    `model` fields.
-   The `ephemeral` keyword was replaced with `mutable`.
-   Type specifications do not allow the `mutable` keyword to used in a
    field by field basis.
-   The syntax for type specifications for Gospel types is a bit
    different.  Instead of

        (*@ type t = { x : integer; y : integer } *)
        (*@ with x invariant x.x = x.y *)

    We have

        (*@ type t = { x : integer; y : integer }
            with x invariant x.x = x.y *)

    Additionally, we do not allow `model` fields or `mutable` annotations in
    specifications for Gospel types.

-   The `model` fields cannot contain fresh type variables.  For example:

        type 'a t
        (*@ model : 'b sequence *)

    Would be invalid


<a id="org76dbb47"></a>

# OCaml values

Ghost OCaml values are not supported.


<a id="org84f6020"></a>

# Prop and Bool

We have added an explicit distinction between `prop` and `bool`.  Logical
operators such as `/\` and `\/` are used with `prop` and `&&` and `||` for `bool`.
