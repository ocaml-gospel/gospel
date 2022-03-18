val f : int -> int -> bool
(*@ equality *)

type t

val g : t -> t -> bool
(*@ equality *)

type 'a u

val h : 'a u -> 'a u -> bool
(*@ equality *)

val h' : 'b u -> 'b u -> bool
(*@ equality *)

val h'' : ('a -> 'a -> bool) -> 'a u -> 'a u -> bool
(*@ equality *)

type v = t

val i : v -> t -> bool
(*@ equality *)

type ('a, 'b) w

val h : ('a, 'b) w -> ('a, 'b) w -> bool
(*@ equality *)

val h : ('a -> 'a -> bool) -> ('a, 'b) w -> ('a, 'b) w -> bool
(*@ equality *)

val h : ('b -> 'b -> bool) -> ('a, 'b) w -> ('a, 'b) w -> bool
(*@ equality *)

val h :
  ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a, 'b) w -> ('a, 'b) w -> bool
(*@ equality *)

val h : ('a -> 'a -> bool) -> ('a, 'a) w -> ('a, 'a) w -> bool
(*@ equality *)

val h :
  ('a -> 'a -> bool) -> ('a -> 'a -> bool) -> ('a, 'a) w -> ('a, 'a) w -> bool
(*@ equality *)

val h : ('a -> 'a -> bool) -> ('a, 'b) w list -> ('a, 'b) w list -> bool
(*@ equality *)

val h : ('a -> 'a -> bool) -> ('a, 'b list) w -> ('a, 'b list) w -> bool
(*@ equality *)

val h : ('b -> 'b -> bool) -> ('a, 'b list) w -> ('a, 'b list) w -> bool
(*@ equality *)
