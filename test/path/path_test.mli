val x : int
val y : int -> int

type t

val w : t

val z : t -> t
(*@ r = z arg
    ensures arg = r *)

module M : sig

  val x : int
  val y : int -> int

 (*@ function f (n : int) : int *)
 (*@ axiom a : forall n. f n = n + 1 *)

  module Nested : sig
    val x : int
    val y : int -> int
  end
end

module N : sig
  
  val x : int
  val y : int -> int
  
end

(* {gospel_expected|
   Path_test
   Path_test
   Path_test
   Path_test
   Path_test
   Path_test.M
   Path_test.M
   Path_test.M
   Path_test.M
   Path_test.M.Nested
   Path_test.M.Nested
   Path_test.M
   Path_test
   Path_test.N
   Path_test.N
   Path_test
   |gospel_expected} *)
