val f : bool -> int
(*@ r = f x
    ensures
      match x with
      | true
      | true
      | true -> r <> 1
      | true -> true
    ensures
      match true with
      | _ -> true
    ensures
      match true with
      | x -> true
*)
