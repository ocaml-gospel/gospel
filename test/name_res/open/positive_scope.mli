(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

module M : sig
  (*@ type t *)

  (*@ function f (x : t) : integer *)

  (*@ predicate p (x : integer) *)
end

module N : sig
  module M : sig
    (*@ type t *)

    (*@ function f (x : t) : integer *)

    (*@ predicate p (x : integer) *)
  end
end

(*@ axiom test : M.(forall x : t. (p (f x))) /\ N.M.(forall x : t. (p (f x))) *)
