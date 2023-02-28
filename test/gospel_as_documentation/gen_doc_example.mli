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

(** A program type declaration with specifications *)
type 'a t
(*@ model m : 'a sequence
    invariant true *)

(** A program function with specifications *)
val prog_fun : int -> int
(*@ y = prog_fun x
    requires true
    ensures true *)
