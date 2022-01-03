(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

module MA : sig
  type ta = C of int

  (*@ function int_of_integer (x:integer): int *)
  (*@ function integer_of_int (x:int): integer *)

  module MB : sig
    (*@ function fb (x:integer) (y:integer): ta =
          C (int_of_integer (x + y))
    *)

    (*@ function int_of_float (x:float): int *)

    (*@ function float_of_int (x:int) : float *)

    module MC : sig
      type tc = { x : int; y : float }

      (*@ function fc (y: tc): ta = C y.x *)

      (*@ function fcc (a: ta): tc = match a with
            C b -> {x=b;y=float_of_int b}*)
    end

    module MD : sig
      (*@ function td (a:ta) (b:MC.tc) : integer = match a with
        | C c -> (integer_of_int c) + (integer_of_int b.MC.x)
      *)
    end
  end
end

module ME = MA

module type MTF = sig
  type ft

  val f : ft -> ft

  (*@ function ff (x:ft): ft *)
  (*@ predicate fp1 (x:ft) *)
  (*@ predicate fp2 (x:ft) *)
end

module MF : MTF

val default : MF.ft -> MF.ft
(*@ x = default y
    requires MF.fp1 y
    ensures MF.fp2 x
*)
