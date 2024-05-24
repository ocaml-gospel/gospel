Running `gospel check --verbose` to test `T.module.print_file` (calling
`Tast_printer.print_signature`, calling `TTerm_printer.print_term`)

  $ gospel check --verbose lib.mli
  
  *******************************
  ********** Parsed file ********
  *******************************
  type t[@@gospel {| model m : integer |}]
  [@@@gospel {| function f (i : integer) : integer |}]
  [@@@gospel {| function (+_) (i : integer) : integer |}]
  [@@@gospel {| function (++) (i : integer) (j : integer) : integer |}]
  [@@@gospel
    {| function ([_.._]) (i : integer) (j : integer) (k : integer) : integer |}]
  val f0 : t -> t[@@gospel {| r = f0 i
      ensures r.m = f i.m |}]
  val f1 : t -> t[@@gospel {| r = f1 i
      ensures r.m = + i.m |}]
  val f2 : t -> t[@@gospel
                   {| r = f2 i
      ensures let h = (+_) in r.m = h i.m |}]
  val f3 : t -> t -> t[@@gospel {| r = f3 i j
      ensures r.m = i.m ++ j.m |}]
  val f4 : t -> t -> t[@@gospel
                        {| r = f4 i j
      ensures let h = (++) i.m in r.m = h j.m |}]
  val f5 : t -> t -> t -> t[@@gospel
                             {| r = f5 i j k
      ensures r.m = i.m [ j.m .. k.m ] |}]
  val f5 : t -> t -> t -> t[@@gospel
                             {| r = f5 i j k
      ensures let h = ([_.._]) i.m j.m in r.m = h k.m |}]
  
  *******************************
  ****** GOSPEL translation *****
  *******************************
  (*@ open Stdlib *)
  
  (*@ open Gospelstdlib *)
  
  type t
    (*@ model ...
         *)
  
  (*@ function f ... *)
  
  (*@ function + ... *)
  
  (*@ function ++ ... *)
  
  (*@ function [_.._] ... *)
  
  val f0 : t -> t
  (*@ r = f0 i
      ensures ...
       *)
  
  val f1 : t -> t
  (*@ r = f1 i
      ensures ...
       *)
  
  val f2 : t -> t
  (*@ r = f2 i
      ensures ...
       *)
  
  val f3 : t -> t -> t
  (*@ r = f3 i j
      ensures ...
       *)
  
  val f4 : t -> t -> t
  (*@ r = f4 i j
      ensures ...
       *)
  
  val f5 : t -> t -> t -> t
  (*@ r = f5 i j k
      ensures ...
       *)
  
  val f5 : t -> t -> t -> t
  (*@ r = f5 i j k
      ensures ...
       *)
  
  *******************************
  ********* Typed GOSPEL ********
  *******************************
  module Lib
  
    Namespace: Lib
      Type symbols
         t
        
      Logic Symbols
        function + (_:integer) : integer
        function ++ (_:integer) (_:integer) : integer
        function [_.._] (_:integer) (_:integer) (_:integer) : integer
        function f (_:integer) : integer
        
      Field Symbols
        function m (_:t) : integer
        
      Exception Symbols
        
      Namespaces
        
      Type Namespaces
        
    Signatures
      (*@ open Stdlib *)
      
      (*@ open Gospelstdlib *)
      
      type t
           (*@ 
               model m : integer *)
      
      (*@ function f (i:integer): integer *)
      
      (*@ function + (i_1:integer): integer *)
      
      (*@ function ++ (i_2:integer) (j:integer): integer *)
      
      (*@ function [_.._] (i_3:integer) (j_1:integer) (k:integer): integer *)
      
      val f0 : t -> t
      (*@ r:t = f0 i_4:t
          ensures ((r:t).m = (f  (i_4:t).m):integer):prop*)
      
      val f1 : t -> t
      (*@ r_1:t = f1 i_5:t
          ensures ((r_1:t).m = ((+) (i_5:t).m):integer):prop*)
      
      val f2 : t -> t
      (*@ r_2:t = f2 i_6:t
          ensures let h:integer -> integer = ((+)):integer -> integer in ((
                  r_2:t).m = (apply 
                  h:integer -> integer (i_6:t).m):integer):prop*)
      
      val f3 : t -> t -> t
      (*@ r_3:t = f3 i_7:t j_2:t
          ensures ((r_3:t).m = ((i_7:t).m ++ (j_2:t).m):integer):prop*)
      
      val f4 : t -> t -> t
      (*@ r_4:t = f4 i_8:t j_3:t
          ensures let h_1:integer -> integer = ((++) (i_8:t).m):integer ->
                                                                integer in ((
                  r_4:t).m = (apply 
                  h_1:integer -> integer (j_3:t).m):integer):prop*)
      
      val f5 : t -> t -> t -> t
      (*@ r_5:t = f5 i_9:t j_4:t k_1:t
          ensures ((r_5:t).m = ):prop*)
      
      val f5_1 : t -> t -> t -> t
      (*@ r_6:t = f5_1 i_10:t j_5:t k_2:t
          ensures let h_2:integer -> integer =  in ((r_6:t).m = (apply 
                  h_2:integer -> integer (k_2:t).m):integer):prop*)
  
