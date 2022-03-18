val f : int -> int -> int
(*@ comparison *)

type t

val g : t -> t -> int
(*@ comparison *)

type 'a u

val h : 'a u -> 'a u -> int
(*@ comparison *)

val h' : 'b u -> 'b u -> int
(*@ comparison *)

val h'' : ('a -> 'a -> int) -> 'a u -> 'a u -> int
(*@ comparison *)

type v = t

val i : v -> t -> int
(*@ comparison *)

type ('a, 'b) w

val h : ('a, 'b) w -> ('a, 'b) w -> int
(*@ comparison *)

val h : ('a -> 'a -> int) -> ('a, 'b) w -> ('a, 'b) w -> int
(*@ comparison *)

val h : ('b -> 'b -> int) -> ('a, 'b) w -> ('a, 'b) w -> int
(*@ comparison *)

val h :
  ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('a, 'b) w -> ('a, 'b) w -> int
(*@ comparison *)

val h : ('a -> 'a -> int) -> ('a, 'a) w -> ('a, 'a) w -> int
(*@ comparison *)

val h :
  ('a -> 'a -> int) -> ('a -> 'a -> int) -> ('a, 'a) w -> ('a, 'a) w -> int
(*@ comparison *)

val h : ('a -> 'a -> int) -> ('a, 'b) w list -> ('a, 'b) w list -> int
(*@ comparison *)

val h : ('a -> 'a -> int) -> ('a, 'b list) w -> ('a, 'b list) w -> int
(*@ comparison *)

val h : ('b -> 'b -> int) -> ('a, 'b list) w -> ('a, 'b list) w -> int
(*@ comparison *)
