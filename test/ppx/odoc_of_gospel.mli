(** Module informal documentation *)

(** An axiom declaration *)

(*@ axiom a : true *)

(** A logical function declaration without definition *)

(*@ function f : integer -> integer *)

(** A logical function definition *)

(*@ function g (i : integer) : integer = i + 1 *)

(** A logical function declaration with assertions *)

(*@ function h (i : integer) : integer = i - 1 *)
(*@ requires i > 0
    ensures result >= 0 *)

(** A logical predicate definition *)

(*@ predicate p (i : integer) = i = 42 *)

(** A ghost type declaration *)

(*@ type casper *)

type 'a t
(** A program type declaration with specifications *)
(*@ model m : 'a sequence
    with x
    invariant true *)

val prog_fun : int -> int
(** A program function with specifications *)
(*@ y = prog_fun x
    requires true
    ensures true *)

val multiple_gospel_attribute : int -> int
(*@ y = multiple_gospel_attribute x *)
(*@ requires true *)
(*@ ensures true *)
