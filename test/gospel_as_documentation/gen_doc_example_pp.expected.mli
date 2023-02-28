(** Module informal documentation *)

(** An axiom declaration *)
(**
 {@gospel[
Gospel declaration:
 axiom a : true ]}
                    *)

(** A logical function declaration without definition *)
(**
 {@gospel[
Gospel declaration:
 function f : integer -> integer ]}
                                     *)

(** A logical function definition *)
(**
 {@gospel[
Gospel declaration:
 function g (i : integer) : integer = i + 1 ]}
                                                *)

(** A logical function declaration with assertions *)
(**
 {@gospel[
 function h (i : integer) : integer = i - 1 
 requires i > 0
    ensures result >= 0 ]}
                         *)

(** A logical predicate definition *)
(**
 {@gospel[
Gospel declaration:
 predicate p (i : integer) = i = 42 ]}
                                        *)

(** A ghost type declaration *)
(**
 {@gospel[
Gospel declaration:
 type casper ]}
                 *)

(** A program type declaration with specifications *)
type 'a t
(**
 {@gospel[
Gospel specification:
 model m : 'a sequence
    invariant true ]}
                    *)

(** A program function with specifications *)
val prog_fun : int -> int
(**
 {@gospel[
Gospel specification:
 y = prog_fun x
    requires true
    ensures true ]}
                  *)

