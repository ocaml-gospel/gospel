# VOCaL

VOCaL -- The Verified OCaml Library
## OCaml Interface Specification Language

Specification elements are added to .mli files

.mli files feature hand-written natural language comments and formal specification

specification is written inside `` (*@ xxxx *)``

### Short Example

The following is an excerpt of [Vector.mli](/src/Vector.mli) example.

Symbols from the specification library can be imported via the `use` keyword.
```OCaml
(*@ use Seq *)
```
The `Seq` module provides a type for mathematical sequences, together with
direct-access and concatenation operations. This type can be used
to mathematically model the elements of a container data structure.

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

* mathematical integers in specification

```OCaml
(** [create] returns a fresh vector of length [0].
   All the elements of this new vector are initially
   physically equal to [dummy] (in the sense of the [==] predicate).
   When [capacity] is omitted, it defaults to 0. *)
val create: ?capacity:int -> dummy:'a -> 'a t
(*@ a = create capacity dummy
      requires let capacity = match capacity with
                 | None -> 0 | Some c -> c in
               0 <= capacity <= Sys.max_array_length
      ensures  length a.view = 0 *)
```

```OCaml
(** [make dummy n x] returns a fresh vector of length [n] with all elements
    initialized with [x]. If [dummy] is omitted, [x] is also used as a
    dummy value for this vector. *)
val make: ?dummy:'a -> int -> 'a -> 'a t
(*@ a = make ?dummy n x
      requires 0 <= n <= Sys.max_array_length
      ensures  length a.view = n
      ensures  forall i: integer. 0 <= i < n -> a.view[i] = x *)

(** [init n f] returns a fresh vector of length [n],
   with element number [i] initialized to the result of [f i].
   In other terms, [init n f] tabulates the results of [f]
   applied to the integers [0] to [n-1].

   Raise [Invalid_argument] if [n < 0] or [n > Sys.max_array_length]. *)
val init: dummy:'a -> int -> (int -> 'a) -> 'a t
(*@ a = init ~dummy n f
      requires 0 <= n <= Sys.max_array_length
      ensures  length a.view = n
      ensures  forall i: int. 0 <= i < n -> a.view[i] = f i *)
```

* `checks`
* `modifies`

```OCaml
(** [resize a n] sets the length of vector [a] to [n].

   The elements that are no longer part of the vector, if any, are
   internally replaced by the dummy value of vector [a], so that they
   can be garbage collected when possible.

   Raise [Invalid_argument] if [n < 0] or [n > Sys.max_array_length]. *)
val resize: 'a t -> int -> unit
(*@ resize a n
      checks   0 <= n <= Sys.max_array_length
      modifies a
      ensures  length a.view = n
      ensures  forall i. 0 <= i < min (length (old a.view)) n ->
                 a.view[i] = (old a.view)[i] *)
```

## Copyright
under the conditions stated in file LICENSE.

## Bugs report

```OCaml
let x = x
```
