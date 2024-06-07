type t
(*@ model m : integer *)

(* Declare a symbol with normal fixity *)
(*@ function f (i : integer) : integer *)

(* Override some symbol from the Gospelstdlib as the parser is relatively
   strict about what is accepted *)

(* Declare a prefix symbol *)
(*@ function (+_) (i : integer) : integer *)

(* Declare an infix symbol *)
(*@ function (++) (i : integer) (j : integer) : integer *)

(* Declare a mixfix symbol *)
(*@ function ([_.._]) (i : integer) (j : integer) (k : integer) : integer *)

(* Use a normal symbol in a gospel term *)
val f0 : t -> t
(*@ r = f0 i
    ensures r.m = f i.m *)

(* Use a prefix symbol in a gospel term *)
val f1 : t -> t
(*@ r = f1 i
    ensures r.m = + i.m *)

(* Use partial application of a prefix symbol in a gospel term *)
val f2 : t -> t
(*@ r = f2 i
    ensures let h = (+_) in r.m = h i.m *)

(* Use an infix symbol in a gospel term *)
val f3 : t -> t -> t
(*@ r = f3 i j
    ensures r.m = i.m ++ j.m *)

(* Use a partial application of an infix symbol in a gospel term *)
val f4 : t -> t -> t
(*@ r = f4 i j
    ensures let h = (++) i.m in r.m = h j.m *)

(* Use a mixfix symbol in a gospel term *)
val f5 : t -> t -> t -> t
(*@ r = f5 i j k
    ensures r.m = i.m [ j.m .. k.m ] *)

(* Use a partial application of a mixfix symbol in a gospel term *)
val f5 : t -> t -> t -> t
(*@ r = f5 i j k
    ensures let h = ([_.._]) i.m j.m in r.m = h k.m *)
