(*@ function  (-_)   (x: integer): integer *)
(*@ function  (+)    (x1: integer)  (x2: integer): integer *)
(*@ function  ( * )  (x1: integer)  (x2: integer): integer *)
(*@ function  (/)    (x1: integer)  (x2: integer): integer *)
(* @ function  (mod) (x1: integer)  (x2: integer): integer *)
(*@ predicate (<)    (x:integer) (y:integer) *)

(*@ function  (-)  (x y : integer) : integer = x + -y *)
(*@ predicate (>)  (x y : integer) = y < x *)
(*@ predicate (<=) (x y : integer) = x < y || x = y *)
(*@ predicate (>=) (x y : integer) = y <= x *)

(*@ function abs (x:integer) : integer = if x >= 0 then x else -x *)

(*@ function min (x y : integer) : integer
    = if x <= y then x else y *)

(*@ function max (x y : integer) : integer
    = if x <= y then y else x *)

(*@ function succ (x: integer) : integer = x + 1 *)
(*@ function pred (x: integer) : integer = x - 1 *)

(*@ function max_int : integer *)
(*@ function min_int : integer *)

(* fst, snd, (!) *)
