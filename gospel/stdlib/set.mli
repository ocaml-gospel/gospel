

(*@ type 'a set *)

(*@ predicate mem (x: 'a) (s: 'a set) *)

(*@ function ( {} ) : 'a set *)

(*@ function ( {:_:} ) (x: 'a) : 'a set *)

(*@ function union (x:'a set) (y:'a set) : 'a set *)

(*@ function sum (f:'a -> integer) (x: 'a set) : integer *)
