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
open Uattr2spec

exception Ocaml_syntax_error of Location.t

let () = Location.register_error_of_exn (function
             | Ocaml_syntax_error loc ->
                Some (Location.errorf ~loc "OCaml syntax error")
             | _ -> None )

exception FileNotFound of string

let open_file load_path file =
  let exception Break of in_channel in
  let try_open d = try
      let f = Filename.concat d file in
      let c = open_in f in raise (Break c)
    with Sys_error _ -> () in
  if not (Filename.is_relative file) then open_in file
  else try List.iter try_open load_path;
           raise (FileNotFound file)
       with Break c -> c

let gospelstdlib = "Gospelstdlib"
let gospelstdlib_file = "gospelstdlib.mli"

let parse_ocaml_lb lb =
  try interface Olexer.token lb with
    Error -> begin
      let spos,fpos = lb.lex_start_p, lb.lex_curr_p in
      let loc = Location.{loc_start=spos; loc_end=fpos;loc_ghost=false}  in
      raise (Ocaml_syntax_error loc) end

(** Parse the given *.mli file -- it must be an interface.
 Raises FileNotFound if file does not exist. *)
let parse_ocaml load_path file =
  let lb =
    if file = gospelstdlib_file then
      Lexing.from_string Gospelstdlib.contents
    else
      let ch = open_file load_path file in
      Lexing.from_channel ch in
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
  if nm = gospelstdlib then signature sign else
    default_open :: signature sign

let parse_ocaml_gospel load_path file =
  parse_gospel (parse_ocaml load_path file)
