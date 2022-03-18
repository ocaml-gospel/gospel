type 'a t

val f : 'a t -> 'a t -> ('a -> 'a -> int) -> int
(*@ comparison *)
