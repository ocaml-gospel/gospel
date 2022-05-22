(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

let run_file file =
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  Lexing.set_filename lexbuf file;
  print_endline (Gospel.Pps.run lexbuf);
  close_in ic

let run = List.iter run_file
