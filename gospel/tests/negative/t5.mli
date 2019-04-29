
(*@ function p (x:integer):integer = x
  requires x
  variant x
  ensures x = 2
  ensures x > 2
  ensures x > 1
*)

(* ERROR: the term in the requires clause should be of type bool or prop *)
