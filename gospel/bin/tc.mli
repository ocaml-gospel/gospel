(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

type config = {
  bench_mode : bool;
  print_intermediate : bool;
  print_parsed : bool;
  parse_only : bool;
  parse_ocaml_only : bool;
  load_path : string list;
}

val run : config -> string list -> unit
