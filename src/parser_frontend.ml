(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

module W = Warnings
open Ppxlib

let stdlib = "Stdlib"
let stdlib_file = "stdlib.mli"
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
  let lb_pps = Pps.run `For_check lb |> Lexing.from_string in
  Location.init lb_pps lb.lex_start_p.pos_fname;
  try Parse.interface lb_pps
  with _ ->
    let loc_start, loc_end = (lb_pps.lex_start_p, lb_pps.lex_curr_p) in
    let loc = Location.{ loc_start; loc_end; loc_ghost = false } in
    W.error ~loc W.Syntax_error

let string_equal_sub s1 s2 i =
  let n = String.length s1 in
  let rec aux j = j = n - 1 || (s1.[j] = s2.[i + j] && aux (succ j)) in
  aux 0

(** This is bad, but it's not my fault. See
    https://github.com/ocaml/ocaml/blob/trunk/stdlib/remove_module_aliases.awk *)
let remove_after_module_aliases s =
  let magic_string = "(*MODULE_ALIASES*)" in
  let rec find i =
    if string_equal_sub magic_string s i then i else find (pred i)
  in
  let index = find (String.length s - String.length magic_string) in
  String.sub s 0 (index - 1)

let parse_ocaml file =
  let lb =
    if String.equal file gospelstdlib_file then
      Lexing.from_string Gospelstdlib.contents
    else if String.equal (Filename.basename file) stdlib_file then
      let ic = open_in file in
      in_channel_length ic
      |> really_input_string ic
      |> remove_after_module_aliases
      |> Lexing.from_string
    else open_in file |> Lexing.from_channel
  in
  Location.init lb file;
  parse_ocaml_lb lb

module B = Ast_builder.Make (struct
  let loc = Location.none
end)

let parse_gospel ~filename signature name =
  let open_gospelstdlib =
    let payload = PStr [ B.(pstr_eval (estring "open Gospelstdlib")) [] ] in
    let name = { txt = "gospel"; loc = Location.none } in
    B.attribute ~name ~payload |> B.psig_attribute
  in
  let open_stdlib =
    let payload = PStr [ B.(pstr_eval (estring "open Stdlib")) [] ] in
    let name = { txt = "gospel"; loc = Location.none } in
    B.attribute ~name ~payload |> B.psig_attribute
  in
  let s =
    if
      String.equal name gospelstdlib
      || String.equal name stdlib
      || String.equal name "CamlinternalFormatBasics"
    then signature
    else open_stdlib :: open_gospelstdlib :: signature
  in
  Uattr2spec.signature ~filename s

let path2module p =
  Filename.basename p |> Filename.chop_extension |> String.capitalize_ascii

let parse_ocaml_gospel path =
  let module_name = path2module path in
  let ocaml = parse_ocaml path in
  parse_gospel ~filename:path ocaml module_name
