(*@ open Gospelstdlib *)
(*@ open Ocamlstdlib *)

val f : int -> int -> int
(*@ r = f x y
    requires x > 0
    requires y + 2 < 0
    requires x + 1 < 0
    ensures r = x + y
    ensures r > 2
    ensures r = 3 *)

exception X
exception Y of int


val f : int -> int -> int
(*@ r = f x y
    raises X -> x = 2
    raises Y i -> i = y + 2 -3 / x
    requires x > 0
    requires y + 2 < 0
    requires x + 1 < 0
    ensures r = x + y
    ensures r > 2
    ensures r = 3 *)
