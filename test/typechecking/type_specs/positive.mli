(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type nat = integer
     with x invariant x >= 0
    and positive_fraction = fraction
      with x invariant
        forall r. let _ = { res = r; f = x } in r > 0
    and negative_fraction = fraction
      with x invariant
        forall r. let _ = { res = r; f = x } in r < 0
    and compute = { res : integer; f : fraction }
      with x invariant x.res = x.res = (x.f.n1 / x.f.n2)
    and fraction = { n1 : integer; n2 : nat } *)
