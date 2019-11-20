(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ predicate is_pre_order (cmp: 'a -> 'a -> int) =
    (forall x. integer_of_int (cmp x x) = 0) /\
    (forall x y. integer_of_int (cmp x y) <= 0 <->
                   integer_of_int (cmp y x) >= 0) /\
    (forall x y z.
       (integer_of_int (cmp x y) <= 0 ->
         integer_of_int (cmp y z) <= 0 -> integer_of_int (cmp x z) <= 0) /\
       (integer_of_int (cmp x y) <= 0 ->
         integer_of_int (cmp y z) <  0 -> integer_of_int (cmp x z) <  0) /\
       (integer_of_int (cmp x y) <  0 ->
         integer_of_int (cmp y z) <= 0 -> integer_of_int (cmp x z) <  0) /\
       (integer_of_int (cmp x y) <  0 ->
         integer_of_int (cmp y z) <  0 -> integer_of_int (cmp x z) <  0)) *)
