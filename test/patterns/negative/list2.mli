val f : 'a list option -> int
(*@ r = f l
    requires match l with
      | None -> false
      | Some (x :: _ as a) -> false
*)
