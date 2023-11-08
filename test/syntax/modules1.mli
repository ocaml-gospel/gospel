module type TA = sig
  type 'a t
  type 'b t2
  (*@ function f (x: 'a t) : float *)
end

module B (A : TA) : sig
  type 'a t = 'a A.t
end

type t
type int

(* type 'b t1 = {x:'b}
 *
 * module A : TA with type 'a t := 'a t1 *)

(* @ function t1 (x:'a t1) : float = A.f x *)
