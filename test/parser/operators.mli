(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** Positive tests for using Gospel operators as normal function applications.
*)

(*@ predicate iff (x : prop) (y : prop) = (<->) True True *)

(*@ axiom if_and : forall p1 p2. (<->) (iff p1 p2) (( /\ ) ((->) p1 p2) ((->) p2 p1)) *)

(*@ axiom aif : forall p. (\/) (if true then p True True else False) False *)

(*@ function application (f : integer -> integer -> integer)
                (x : integer) (y : integer) : integer =
                f x y *)

(*@ function plus : integer = application (+) 0 0 *)
(*@ function minus : integer = application (-) 0 0 *)
(*@ axiom div_1_twice : forall n1 n2. ( * ) n1 n2 = (/) 1 ((/) 1 ((/) n1 n2)) *)

(*@ function or_b (b1 : bool) (b2 : bool) : bool = (||) b1 b2 *)

(*@ function and_b (b1 : bool) (b2 : bool) : bool = (&&) b1 b2 *)

(*@ axiom a3 : forall x y. (<->) ((<>) x y) (not (=) x y) *)

(*@ axiom app_assoc :
      forall s1 s2 s3.
      (=)
      ((++) s1 ((++) s2 s3))
      ((++) ((++) s1 s2) s3) *)

(*@ function hd (s : 'a sequence) : 'a = ([_]) s 0   *)
(*@ function range1 (s : 'a sequence) (i : integer) : 'a sequence = ([.._]) s i *)
(*@ function range2 (s : 'a sequence) (i : integer) : 'a sequence = ([_..]) s i *)
(*@ function range3 (s : 'a sequence) (i : integer) : 'a sequence = ([_.._]) s i i *)

module M : sig
  (*@ function (+) (n : integer) (m : integer) : integer = (+) n m *)
  (*@ function (-) (n : integer) (m : integer) : integer = (-) n m *)
end

(*@ axiom sub_add : forall m n. M.(-) m (M.(+) n m) = M.(-) (M.(+) n m) m *)

(*@ function set : 'a set = { v | True } *)
