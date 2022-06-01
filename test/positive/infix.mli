val ( == ) : 'a -> 'a -> bool
(*@ r = (==) x y
      ensures r <-> x = y *)

val ( == ) : 'a -> 'a -> bool
(*@ r = x == y
      ensures r <-> x = y *)

(* {gospel_expected|
   [0] OK
   |gospel_expected} *)
