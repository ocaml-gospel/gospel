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
  (*@ type t1 *)

  (*@ type t2 = { x : integer } *)

  (*@ function f : integer *)

  (*@ predicate p *)
end

module N : sig
  (*@ open M *)

  (*@ type t1' = t1 *)

  (*@ function f' (r : t2) : integer = f + r.x *)

  (*@ predicate p' = p *)

  module O : sig
    (*@ type t1'' = t1 *)

    (*@ type t2'' = { x : integer } *)

    (*@ function f'' (r : t2'') : integer = f + r.x *)

    (*@ predicate p'' = p *)
  end
end

module O : sig
  (*@ open N.O *)

  (*@ type t1 = t1'' *)

  (*@ function f (r : t2'') : integer = f'' r + r.x *)

  (*@ predicate p = p'' *)
end

module P : sig
  (*@ open N *)
  (*@ open O *)

  (*@ type t1 = t1'' *)

  (*@ function f (r : t2'') : integer = f'' r + r.x *)

  (*@ predicate p = p'' *)
end
