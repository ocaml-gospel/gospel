Testing the paths of typed identifiers by `gospel dumpast`, 

First, create a test artifact:

  $ cat > path_test.mli << EOF
  > val x : int
  > val y : int ->  int
  > 
  > type t
  > 
  > val w : t
  > 
  > val z : t ->  t
  > (*@ r = z arg
  >     ensures arg = r *)
  > 
  > module M : sig
  > 
  >   val x : int
  >   val y : int ->  int
  > 
  >  (*@ function f (n : int) : int *)
  >  (*@ axiom a : forall n. f n = n + 1 *)
  > 
  >   module Nested : sig
  >     val x : int
  >     val y : int ->  int
  >   end
  > end
  > 
  > module N : sig
  >   
  >   val x : int
  >   val y : int ->  int
  >   
  > end
  > EOF
  $ gospel dumpast path_test.mli | tr '\n' ' ' | tr ';}' '\n' | grep -Eo '_name = .*'  | awk '{$1=$1};1' | sed 's/_name = \(\)/\1/'
  Path_test.x
  x_1
  x_1
  Path_test.y
  __arg0
  result
  __arg0
  result
  Path_test.w
  w_1
  w_1
  Path_test.z
  arg
  r
  arg
  r
  infix =
  a_1
  a_1
  arg
  r
  Path_test.M
  Path_test.M.x_2
  x_3
  x_3
  Path_test.M.y_1
  __arg0_1
  result_1
  __arg0_1
  result_1
  Path_test.M.f
  n
  Path_test.M.a_2
  n_1
  infix =
  a_1
  a_1
  Gospelstdlib.integer_of_int
  Path_test.M.f
  n_1
  Gospelstdlib.infix +
  Gospelstdlib.integer_of_int
  n_1
  Path_test.M.Nested
  Path_test.M.Nested.x_4
  x_5
  x_5
  Path_test.M.Nested.y_2
  __arg0_2
  result_2
  __arg0_2
  result_2
  Path_test.N
  Path_test.N.x_6
  x_7
  x_7
  Path_test.N.y_3
  __arg0_3
  result_3
  __arg0_3
  result_3
Clean up:

  $ rm path_test.mli
