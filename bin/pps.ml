(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

let size = 64

let run_file file =
  let l = String.length file in
  if String.sub file (l - 4) 4 = ".mli" then (
    let ic = open_in file in
    let lexbuf = Lexing.from_channel ic in
    Lexing.set_filename lexbuf file;
    Fmt.pr "@[%a@]\n" Gospel.Pps.run lexbuf;
    close_in ic)
  else
    let ic = open_in file in
    let buf = Bytes.create size in
    try
      while true do
        let i = input ic buf 0 size in
        if i = size then Bytes.to_string buf |> print_string
        else (
          Bytes.sub_string buf 0 i |> print_string;
          raise Exit)
      done
    with Exit -> close_in ic

let run = List.iter run_file
