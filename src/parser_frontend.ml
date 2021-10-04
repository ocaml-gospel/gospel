(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Ppxlib

exception Ocaml_syntax_error of Location.t

let () =
  let open Location.Error in
  register_error_of_exn (function
    | Ocaml_syntax_error loc -> Some (make ~loc ~sub:[] "OCaml syntax error")
    | _ -> None)

let gospelstdlib = "Gospelstdlib"
let gospelstdlib_file = "gospelstdlib.mli"

let with_loadpath load_path file =
  let exception Break of string in
  let try_open d =
    try
      let f = Filename.concat d file in
      if Sys.file_exists f then raise (Break f)
    with Sys_error _ -> ()
  in
  if file = gospelstdlib_file then file
  else if Filename.is_relative file then
    try
      List.iter try_open load_path;
      raise Not_found
    with Break c -> c
  else if Sys.file_exists file then file
  else raise Not_found

let parse_ocaml_lb lb =
  let lb_pps = Pps.run lb |> Lexing.from_string in
  Location.init lb_pps lb.lex_start_p.pos_fname;
  try Parse.interface lb_pps
  with _ ->
    let loc_start, loc_end = (lb_pps.lex_start_p, lb_pps.lex_curr_p) in
    let loc = Location.{ loc_start; loc_end; loc_ghost = false } in
    raise (Ocaml_syntax_error loc)

let parse_ocaml file =
  let lb =
    if file = gospelstdlib_file then Lexing.from_string Gospelstdlib.contents
    else open_in file |> Lexing.from_channel
  in
  Location.init lb file;
  parse_ocaml_lb lb

module B = Ast_builder.Make (struct
  let loc = Location.none
end)

let default_open =
  let payload = PStr [ B.(pstr_eval (estring "open Gospelstdlib")) [] ] in
  let name = { txt = "gospel"; loc = Location.none } in
  B.attribute ~name ~payload |> B.psig_attribute

(** Parse the attributes as GOSPEL specification. *)
let parse_gospel ~filename signature name =
  (if name = gospelstdlib then signature else default_open :: signature)
  |> Uattr2spec.signature ~filename

let path2module p =
  Filename.basename p |> Filename.chop_extension |> String.capitalize_ascii

let parse_ocaml_gospel path =
  let module_name = path2module path in
  let ocaml = parse_ocaml path in
  parse_gospel ~filename:path ocaml module_name
