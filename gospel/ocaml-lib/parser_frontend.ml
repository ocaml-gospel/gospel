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

(** Parse the given *.mli file -- it must be an interface.
 Raises FileNotFound if file does not exist. *)
let parse_ocaml load_path file =
  let ch = open_file load_path file in
  let lb = Lexing.from_channel ch in
  Location.init lb file;
  try interface Olexer.token lb with
    Error -> begin
      let spos,fpos = lb.lex_start_p, lb.lex_curr_p in
      let loc = Location.{loc_start=spos; loc_end=fpos;loc_ghost=false}  in
      raise (Ocaml_syntax_error loc) end

let gospelstdlib = "Gospelstdlib"

let libs nm =
  if nm = gospelstdlib then [] else [gospelstdlib]

let default_opens nm =
  let open Uast in
  let open Oparsetree in
  let od nm =
    let id = Location.{txt = Longident.Lident nm; loc = none} in
    let od = {popen_lid = id; popen_override = Fresh;
              popen_loc = Location.none; popen_attributes = []} in
    Sig_ghost_open od in
  let sig_item nm = {sdesc = od nm; sloc = Location.none} in
  List.map sig_item (libs nm)

(** Parse the attributes as GOSPEL specification. Raises FileNotFound
   if file does not exist. *)
let parse_gospel sign nm =
  default_opens nm @ signature sign

(** Raises FileNotFound if file does not exist. *)
let parse_ocaml_gospel load_path file =
  parse_gospel (parse_ocaml load_path file)
