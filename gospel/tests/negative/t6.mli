
(*@ function p (x:integer):integer = x
  requires x > 0
  variant x = 0
  ensures x = 2
  ensures x > 2
  ensures x > 1
*)

(* ERROR: the term in the variant clause should be of type integer *)
