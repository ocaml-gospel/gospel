Testing the paths of typed identifiers by `gospel dumpast`, 

First, create a test artifact:

  $ cat > test.mli << EOF
  > module M : sig
  >   type t
  >   
  >   (*@ function f : t *)
  >   (*@ function g : t = f *)
  >   (*@ axiom a1 : f = g *)
  > 
  >   module Nested : sig
  >     type t
  >     
  >     (*@ function nf : t *)
  >     (*@ function ng : t *)
  >     (*@ axiom n1 : ng = nf *)
  >   end
  > 
  >   (*@ axiom a2 : Nested.nf = Nested.ng *)
  >   (*@ open Nested *)
  >   (*@ axiom a3 : nf = ng*)
  >   
  > end
  > 
  > module N : sig
  >      
  >   (*@ function g (n : M.t) : integer *)
  >   (*@ function f (n : integer) : integer = 
  >         g M.g + n *)
  >   (*@ function w (n : integer) : integer = 
  >         f n + n *)
  > 
  >   module Nested : sig
  > 
  >     (*@ function z : integer = g M.g *)
  >     
  >   end
  > 
  >   (*@ predicate x (n : integer) = n = Nested.z *)
  >   
  > end
  > EOF
  $ gospel dumpast test.mli | grep -e "ls_name = " -e "ts_ident = "
                         { Ttypes.ts_ident = Test.M.t; ts_args = [];
                         { Symbols.ls_name = Test.M.f; ls_args = [];
                                      { Ttypes.ts_ident = t; ts_args = [];
                         { Symbols.ls_name = Test.M.g; ls_args = [];
                                      { Ttypes.ts_ident = t; ts_args = [];
                                    { Symbols.ls_name = f; ls_args = [];
                                                 { Ttypes.ts_ident = t;
                                            { Ttypes.ts_ident = t;
                              { Symbols.ls_name = infix =;
                                    { Symbols.ls_name = f; ls_args = [];
                                                 { Ttypes.ts_ident = t;
                                            { Ttypes.ts_ident = t;
                                     { Symbols.ls_name = g; ls_args = [];
                                                  { Ttypes.ts_ident = t;
                                             { Ttypes.ts_ident = t;
                                       { Ttypes.ts_ident = Test.M.Nested.t_1;
                                       { Symbols.ls_name = Test.M.Nested.nf;
                                                    { Ttypes.ts_ident = t_1;
                                       { Symbols.ls_name = Test.M.Nested.ng;
                                                    { Ttypes.ts_ident = t_1;
                                            { Symbols.ls_name = infix =;
                                                  { Symbols.ls_name = ng;
                                                   { Symbols.ls_name = nf;
                              { Symbols.ls_name = infix =;
                                    { Symbols.ls_name = Nested.nf;
                                                 { Ttypes.ts_ident = t_1;
                                            { Ttypes.ts_ident = t_1;
                                     { Symbols.ls_name = Nested.ng;
                                                  { Ttypes.ts_ident = t_1;
                                             { Ttypes.ts_ident = t_1;
                              { Symbols.ls_name = infix =;
                                    { Symbols.ls_name = Nested.nf;
                                                 { Ttypes.ts_ident = t_1;
                                            { Ttypes.ts_ident = t_1;
                                     { Symbols.ls_name = Nested.ng;
                                                  { Ttypes.ts_ident = t_1;
                                             { Ttypes.ts_ident = t_1;
                        { Symbols.ls_name = Test.N.g_1;
                                { Ttypes.ts_ident = M.t; ts_args = [];
                                     { Ttypes.ts_ident = integer; ts_args = [];
                                { Ttypes.ts_ident = M.t; ts_args = [];
                         { Symbols.ls_name = Test.N.f_1;
                                 { Ttypes.ts_ident = integer; ts_args = [];
                                      { Ttypes.ts_ident = integer;
                                 { Ttypes.ts_ident = integer; ts_args = [];
                                    { Symbols.ls_name = Gospelstdlib.infix +;
                                            { Ttypes.ts_ident = integer;
                                             { Ttypes.ts_ident = integer;
                                                 { Ttypes.ts_ident = integer;
                                          { Symbols.ls_name = g_1;
                                                  { Ttypes.ts_ident = M.t;
                                                { Symbols.ls_name = M.g;
                                                        { Ttypes.ts_ident = t;
                                                  { Ttypes.ts_ident = integer;
                                                  { Ttypes.ts_ident = integer;
                                                   { Ttypes.ts_ident = integer;
                                            { Ttypes.ts_ident = integer;
                         { Symbols.ls_name = Test.N.w;
                                 { Ttypes.ts_ident = integer; ts_args = [];
                                      { Ttypes.ts_ident = integer;
                                 { Ttypes.ts_ident = integer; ts_args = [];
                                    { Symbols.ls_name = Gospelstdlib.infix +;
                                            { Ttypes.ts_ident = integer;
                                             { Ttypes.ts_ident = integer;
                                                 { Ttypes.ts_ident = integer;
                                          { Symbols.ls_name = f_1;
                                                  { Ttypes.ts_ident = integer;
                                                  { Ttypes.ts_ident = integer;
                                                  { Ttypes.ts_ident = integer;
                                                   { Ttypes.ts_ident = integer;
                                            { Ttypes.ts_ident = integer;
                                      { Symbols.ls_name = Test.N.Nested.z;
                                                   { Ttypes.ts_ident = integer;
                                                 { Symbols.ls_name = g_1;
                                                       { Symbols.ls_name = M.g;
                         { Symbols.ls_name = Test.N.x;
                                 { Ttypes.ts_ident = integer; ts_args = [];
                                 { Ttypes.ts_ident = integer; ts_args = [];
                                    { Symbols.ls_name = infix =;
                                                 { Ttypes.ts_ident = integer;
                                                  { Ttypes.ts_ident = integer;
                                           { Symbols.ls_name = Nested.z;
                                                   { Ttypes.ts_ident = integer;
Clean up:

  $ rm test.mli
