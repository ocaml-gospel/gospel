# VOCaL

VOCaL -- The Verified OCaml Library
## OCaml Specification Language

### Short Example

file [/src/Vector.mli](/src/Vector.mli)

```OCaml
(*@ use List *)
(*@ use Seq  *)
```

```OCaml
(** The polymorphic type of vectors.
    This is a mutable data type. *)
type 'a t
(*@ ephemeral *)
(*@ mutable model view: 'a seq *)
(*@ invariant length view <= Sys.max_array_length *)
```

```OCaml
(** [create] returns a fresh vector of length [0].
   All the elements of this new vector are initially
   physically equal to [dummy] (in the sense of the [==] predicate).
   When [capacity] is omitted, it defaults to 0. *)
val create: ?capacity:int -> dummy:'a -> 'a t
(*@ a = create ?capacity ~dummy
      requires let capacity = match capacity with
                 | None -> 0 | Some c -> c in
               0 <= capacity <= Sys.max_array_length
      ensures  length a.view = 0 *)
```

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