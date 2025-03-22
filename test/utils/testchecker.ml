(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** This script receives an OCaml interface file and runs the Gospel type
    checker. It then prints the file's contents to the standard output (minus
    the expected output, if it exists) as well as the output from the Gospel
    type checker. *)

open Fmt

let result_start = "(* {gospel_expected|"

let print ppf file =
  let ch = open_in file in
  let rec aux () =
    match input_line ch with
    | s when String.equal result_start s ->
        (* If the line begins with [result_start], then the remaining lines will
          be ignored. *)
        ()
    | s ->
        pf ppf "%s@\n" s;
        aux ()
    | exception End_of_file -> ()
  in
  aux ();
  close_in ch

let test_file file =
  let stderr = str "%s_stderr" file in
  let command = str "gospel check %s > %s 2> %s" file Filename.null stderr in
  let status = Sys.command command in
  pr "%a" print file;
  if status <> 0 then
    pr "[@(* {%s@\n[%d] @[%a@]@\n|gospel_expected}@] *)@\n" result_start status
      print stderr

let () =
  let file = Sys.argv.(1) in
  test_file file
