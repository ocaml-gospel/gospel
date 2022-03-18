type ('a, 'b) t

val f :
  ('b -> 'b -> int) -> ('a -> 'a -> int) -> ('a, 'b) t -> ('a, 'b) t -> int
(*@ comparison *)
