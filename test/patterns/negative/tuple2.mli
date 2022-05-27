type tc = char * int
(*@ function f (a : tc) : bool =
      match a with
      | '\000'..'b', 0i -> true
      | 'b'..'\255', x -> false
*)
