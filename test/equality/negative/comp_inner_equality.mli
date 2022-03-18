type ('a, 'b) t

val f : ('a list -> 'a list -> int) -> ('a list, 'b) t -> ('a list, 'b) t -> int
(*@ comparison *)
