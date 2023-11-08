type t = O | S of t

val succ : t -> t
(*@ y = succ x
    ensures y = S x *)

val test1 : t -> t -> t
(*@ r = test1 x y
    requires x <> O && y = O
    ensures  match x, y with
             | _, S _ -> false
             | O, _   -> false
             | S a, O -> r = S (S a) *)

(* pattern of type unit *)
val f_unit : int array -> unit
(*@ x1 = f_unit a
     modifies a
     ensures match x1 with () -> true *)

(*@ function fun_unit (x: unit): string =
    match x with
    | () -> "out" *)

type t0 = B of int * int

(*@ function f (x: t0) : unit =
    match x with B _ -> () *)

type t1 = A1 | A2 | B of t1

(*@ function f (x: t1): integer =
    match x with
    | A1
    | A2 -> 0
    | B A1
    | B A2 -> 0
    | B (B A1)
    | B (B A2) -> 0
    | B (B (B A1))
    | B _ -> 0 *)

type t2 = E | I of int

val f2 : t2 -> int
(*@ r = f2 i
    ensures match i with
      | E    -> true
      | I 1i -> true
      | I 2i -> true
      | I 6i -> true
      | _    -> true *)

(*@ type t_integer = N of integer *)
(*@ function f3 (x : t_integer) : integer =
      match x with
      | N 0
      | N 1
      | N _ -> 0 *)

type t4 = A of string | B

(*@ function f4 (x: t4): integer =
      match x with
      | B
      | A "1" -> 0
      | A "333"
      | A _ -> 0 *)

val f5 : bool -> int

(*@ r = f5 x
    ensures match x with
      | false -> r <> 1
      | true -> true
    ensures match true with _ -> true
    ensures match true with x -> true
*)

type t6 = A | B of t6 * float

val f6 : t6 -> int
(*@ r = f6 x
    ensures match x with
    | A -> false
    | B (B _, _) -> true
    | B (A, z) -> false *)

type 'a t7 = 'a * int

val f7 : char t7 -> int
(*@ r = f7 a
    ensures match a with
      | '\000', 0i
      | 'z', 0i -> false
      | _ -> true
*)

val f8 : 'a option -> int
(*@ r = f8 o
    ensures match o with
    | None -> r = 2
    | Some x -> r <> 2 *)

val f9 : char -> int
(*@ r = f9 a
    ensures
      match a with
      | '\000' .. '\031'
      | '\127' .. '\255' -> false
      | ' '    .. '~'    -> false

    ensures
      match a with
      | '\000'..'a' -> false
      | 'b'..'\255' -> true
*)

val f10 : bool -> int
(*@ r = f10 x
    ensures
      match x with
      | true -> r <> 1
      | false -> true
    ensures
      match true with
      | _ -> true
    ensures
      match true with
      | x -> true
*)

type t11 = A of (int * int) | B of int * int

(*@ function f11 (x: t11): integer =
    match x with
    | A _ -> 0
    | B _ -> 0 *)

type t12 = P of char * t12

(*@ function f (a : t12) : bool =
      match a with
      | P ('\000'..'b', _) -> true
      | P ('b'..'\255', x) -> false
*)

type t13 = { n : int; s : string }

(*@ function f13 (x: t13): integer =
    match x with
    | { n=0i; s="beta"|"omega" } -> 0
    | { n=m } -> 0 *)

(*@ function f13' (x: t13): integer =
    match x with
    | { s="beta"|"omega" ; n=0i } -> 0
    | { n=m ; s = y } -> 0 *)

val f14 : 'a list -> int
(*@ r = f14 l
    requires match l with
      | [] -> true
      | x::y::_ -> false
      | x::_ -> false
    requires match l with
      | [] -> true
      | x::(y::_ as ll) -> false
      | x::_ -> false
    requires match Some l with
      | None -> false
      | Some [] -> true
      | Some (x :: _ as a) -> false
*)

type t15 = A | B

val f15 : t15 * t15 * t15 * t15 -> int

(*@ r = f15 x
    requires match x with
     | A,A,A,A
     | B,B,B,B
     | _,A,A,A
     | _,B,B,B
     | _,_,A,A
     | _,_,B,B
     | _,_,_,A
     | _,_,_,B -> true
*)

type t16 = A | B of t16 | C of t16 * t16

val f16 : t16 -> int
(*@ r = f16 x
  requires match x with
           | A when true  -> true
           | B y when y=A -> true
           | B y when false -> true && y=A
           | C (y,z) when y=A && z=A -> true
           | C (y,z) when (y=A -> z=A) -> true
           | C (y,z) when (match y with A -> true | _ -> false) -> true
           | _ -> true
*)
