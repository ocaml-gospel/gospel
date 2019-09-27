
(* the type ('a, 'b) map is defined internally in GOSPEL and can be
   written as 'a -> 'b *)

(*@ function ( [<-] ) (m: 'a -> 'b) (x:'a) (y: 'b) : 'a -> 'b *)

(*@ function ( [_] ) (m: 'a -> 'b) (x: 'a) : 'b *)
