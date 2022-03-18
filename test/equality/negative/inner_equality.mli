type ('a, 'b) t

val f :
  ('a list -> 'a list -> bool) -> ('a list, 'b) t -> ('a list, 'b) t -> bool
(*@ equality *)
