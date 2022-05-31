val ( == ) : 'a -> 'a -> bool
(*@ r = (==) x y
      ensures r <-> x = y *)

val ( == ) : 'a -> 'a -> bool
(*@ r = x == y
      ensures r <-> x = y *)

(* EXPECTED
   [0] OK
*)
