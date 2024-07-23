(*@ type t = { a : integer; b : integer; c : integer } *)

(* OCaml would report missing fields *)
(*@ function f (x : t) : integer =
      match x with
      | { a } -> a
*)
