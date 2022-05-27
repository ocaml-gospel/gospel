type t = A of char
(*@ function f2 (x : t) : bool =
      match x with
      | A ('\000'..'\010') -> false
      | A ('\011'..'a')
      | A ('c'..'\255') -> false
*)
