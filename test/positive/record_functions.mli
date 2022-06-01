type t = int -> int
type u = (int -> int) ref
type v = private { x : int -> int }
(*@ with self invariant self.x 0i = 0 *)

(* {gospel_expected|
   [0] OK
   |gospel_expected} *)
