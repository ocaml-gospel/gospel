(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Oparser

exception Ocaml_syntax_error of Location.t

let () = Location.register_error_of_exn (function
             | Ocaml_syntax_error loc ->
                Some (Location.errorf ~loc "OCaml syntax error")
             | _ -> None )

let gospelstdlib = "Gospelstdlib"
let gospelstdlib_file = "gospelstdlib.mli"

let with_loadpath load_path file =
  let exception Break of string in
  let try_open d =
    try
      let f = Filename.concat d file in
      if Sys.file_exists f then raise (Break f)
    with Sys_error _ -> () in
  if file = gospelstdlib_file then file
  else if Filename.is_relative file then
    try List.iter try_open load_path; raise Not_found
    with Break c -> c
  else if Sys.file_exists file then file
  else raise Not_found

let parse_ocaml_lb lb =
  try interface Olexer.token lb with
    Error -> begin
      let spos,fpos = lb.lex_start_p, lb.lex_curr_p in
      let loc = Location.{loc_start=spos; loc_end=fpos;loc_ghost=false}  in
      raise (Ocaml_syntax_error loc) end

let parse_ocaml file =
  let lb =
    if file = gospelstdlib_file then
      Lexing.from_string Gospelstdlib.contents
    else
      open_in file |> Lexing.from_channel
  in
  Location.init lb file;
  parse_ocaml_lb lb

let default_open =
  let open Uast in
  let open Oparsetree in
  let od nm =
    let id = Location.{txt = Longident.Lident nm; loc = none} in
    let od = {popen_lid = id; popen_override = Fresh;
              popen_loc = Location.none; popen_attributes = []} in
    Sig_ghost_open od in
  {sdesc = od gospelstdlib; sloc = Location.none}

(** Parse the attributes as GOSPEL specification. *)
let parse_gospel sign nm =
  let s = Uattr2spec.signature sign in
  if nm = gospelstdlib then s else default_open :: s

let path2module p =
  Filename.basename p |> Filename.chop_extension |> String.capitalize_ascii

let parse_ocaml_gospel path =
  let module_name = path2module path in
  let ocaml = parse_ocaml path in
  parse_gospel ocaml module_name
