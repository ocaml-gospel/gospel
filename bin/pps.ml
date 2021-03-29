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
  Lexing.from_channel ic |> Gospel.Pps.run |> print_endline;
  close_in ic

let run = List.iter run_file
