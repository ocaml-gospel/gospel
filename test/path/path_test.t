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
  $ gospel dumpast path_test.mli | grep '_name.*'
         { Tast.vd_name = Path_test.x; vd_type = int; vd_prim = [];
               { Symbols.vs_name = x_1;
         { Tast.vd_name = Path_test.y; vd_type = int -> int; vd_prim = [];
               { Symbols.vs_name = __arg0;
               { Symbols.vs_name = result;
         { Tast.vd_name = Path_test.w; vd_type = t; vd_prim = [];
               { Symbols.vs_name = w_1;
         { Tast.vd_name = Path_test.z; vd_type = t -> t; vd_prim = [];
               { Symbols.vs_name = arg;
               { Symbols.vs_name = r;
                       { Symbols.vs_name = arg;
                       { Symbols.vs_name = r;
                         { Symbols.ls_name = infix =;
                              (Ttypes.Tyvar { Ttypes.tv_name = a_1 }) };
                               (Ttypes.Tyvar { Ttypes.tv_name = a_1 }) }
                               { Symbols.vs_name = arg;
                                { Symbols.vs_name = r;
         { Tast.md_name = Path_test.M;
                      { Tast.vd_name = Path_test.M.x_2; vd_type = int;
                            { Symbols.vs_name = x_3;
                       { Tast.vd_name = Path_test.M.y_1; vd_type = int -> int;
                             { Symbols.vs_name = __arg0_1;
                             { Symbols.vs_name = result_1;
                         { Symbols.ls_name = Path_test.M.f;
                         [{ Symbols.vs_name = n;
                       { Tast.ax_name = Path_test.M.a_2;
                              [{ Symbols.vs_name = n_1;
                                   { Symbols.ls_name = infix =;
                                        (Ttypes.Tyvar { Ttypes.tv_name = a_1 })
                                         (Ttypes.Tyvar { Ttypes.tv_name = a_1 })
                                         { Symbols.ls_name =
                                               { Symbols.ls_name = f;
                                                     { Symbols.vs_name = n_1;
                                          { Symbols.ls_name =
                                                { Symbols.ls_name =
                                                      { Symbols.vs_name = n_1;
                       { Tast.md_name = Path_test.M.Nested;
                                    { Tast.vd_name = Path_test.M.Nested.x_4;
                                          { Symbols.vs_name = x_5;
                                     { Tast.vd_name = Path_test.M.Nested.y_2;
                                           { Symbols.vs_name = __arg0_2;
                                           { Symbols.vs_name = result_2;
         { Tast.md_name = Path_test.N;
                      { Tast.vd_name = Path_test.N.x_6; vd_type = int;
                            { Symbols.vs_name = x_7;
                       { Tast.vd_name = Path_test.N.y_3; vd_type = int -> int;
                             { Symbols.vs_name = __arg0_3;
                             { Symbols.vs_name = result_3;

Clean up:

  $ rm path_test.mli
