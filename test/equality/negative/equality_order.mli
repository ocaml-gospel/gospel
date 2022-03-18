type ('a, 'b) t

val f :
  ('b -> 'b -> bool) -> ('a -> 'a -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
(*@ equality *)
