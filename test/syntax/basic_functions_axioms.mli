(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom a1: true *)

(*@ axiom a2: 1 = 0 *)

(*@ function f1 (x:int) : int *)

(*@ function f2 (x:int) : int = x *)

(*@ function f (x:integer): bool = x = 2 *)

(*@ function f (x:integer): bool = true *)

(*@ function f (x:bool): bool = x *)

(*@ predicate pred (x:bool) = x *)

(*@ function p (x:integer):integer = x *)
(*@ requires x = 1
    variant x
    ensures x = 2
    ensures x > 2
    ensures x > 1 *)

(*@ function rec f (x:bool): bool = f x *)

(*@ function rec f (x: bool) (y: int): bool = f x y *)

(*@ function g (a: int): integer =
      if (f true a) then 1 else 2 *)

(*@ function int_of_integer (x:integer): int *)

(*@ function h (a:int) (b:bool) (c:'a): bool =
      if a = int_of_integer 2
      then f b (int_of_integer 3)
      else g (int_of_integer 4) = 5
*)

(*@ function h (a:int) (b:bool) (c:'a): bool =
      if a = int_of_integer 2
      then f (pred b) (int_of_integer 3)
      else g (int_of_integer 4) = 5
*)

(*@ function h (b:bool): bool = pred b *)

(*@ function h: bool = [@ athing]true *)

(*@ function to_integer (i: int): integer *)

(*@ function i (a:int): int =
      int_of_integer (to_integer a + 1) *)

(*@ function i (a:int):int =
      int_of_integer (to_integer a + 1) *)
(*@ requires to_integer a > 0
    ensures let old_a [@ athing] = to_integer (old a) in
            to_integer a = old_a + 1 *)

type 'a t1 = C of 'a * int

(*@ function i (a:'a t1): int =
      match a with
      | C (_,y) -> y
*)

type 'a t2 = C2 of 'a | C3 of bool | C4 of int * 'a

(*@ function gnr: 'a *)

(*@ function g (x y:'a) (i: int): 'a *)

(*@ function f (x: 'a t2): 'a =
    match x with
    | C2 x -> x        (* TODO it does not give the right result if x is replaced by true *)
    | C3 b -> gnr
    | C4 (i,x) -> g x x i *)

(*@ axiom ax1: forall x y. y = f x *)

val f : int -> int -> int
(*@ r = f x y
    requires x > 0
    requires y + 2 < 0
    ensures r = x + y *)

type 'a t3 = A

(*@ function f (x: int) : int t3 = A *)

(*@ function integer_of_int (x:int) : integer *)

type t4 = A | B
type 'a t5 = { x : int; y : 'a }

(* @ function f (x: t4 t5) : integer =
   match x with
   | {x;y=A} -> integer_of_int x + 1
   | {x;y=B} -> integer_of_int x + 2 *)

type 'a t6 = { xx : 'a; yy : int }

(* @ function f (xx: 'a): 'a t6 =
   let a = {yy = int_of_integer 2; xx} in
   {a with yy = int_of_integer 3} *)

(*@ function g (a: t4) (b: t4 t5) : t4 t6 =
  match b with
  | {x; y = A} -> {xx=B;yy=x}
  | {y = B; x} -> {yy=int_of_integer 10; xx = b.y }
*)

(*@ function g (a: t4) (b: t4 t5) : t4 t6 =
  match b with
  | {y = B; x} -> {yy=int_of_integer 10; xx = b.y }
  | {x; y} -> {xx=y;yy=x}
*)
