val hd : 'a list -> 'a
(*@ x = hd l
      raises Failure _ -> l = [] *)

val find : ('a -> bool) -> 'a list -> 'a
(*@ r = find f l
      raises Not_found -> forall x. Sequence.mem x l.list_content -> not f x *)

val invalid_arg : string -> 'a
(*@ raises  Invalid_argument _ -> true
    ensures false *)
