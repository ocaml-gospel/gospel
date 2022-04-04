(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

module W = Gospel.Warnings
open Ppxlib
open Utils
open Uast
open Attrs

(* XXX: Use Lexing.set_position when moving to OCaml 4.11 *)
let set_position (lexbuf : Lexing.lexbuf) (position : Lexing.position) =
  lexbuf.lex_curr_p <- { position with pos_fname = lexbuf.lex_curr_p.pos_fname };
  lexbuf.lex_abs_pos <- position.pos_cnum

(* XXX: Use Lexing.set_filename when moving to OCaml 4.11 *)
let set_filename (lexbuf : Lexing.lexbuf) (fname : string) =
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname }

let parse_gospel ~filename parse attr =
  let spec, _ = get_spec_content attr in
  let lb = Lexing.from_string spec in
  set_position lb attr.attr_loc.loc_start;
  set_filename lb filename;
  try (spec, parse Ulexer.token lb)
  with Uparser.Error ->
    let loc =
      { loc_start = lb.lex_start_p; loc_end = lb.lex_curr_p; loc_ghost = false }
    in
    W.error ~loc W.Syntax_error

let type_declaration ~filename t =
  let spec_attr = get_spec_attr t.ptype_attributes in
  let parse attr =
    let ty_text, spec = parse_gospel ~filename Uparser.type_spec attr in
    { spec with ty_text; ty_loc = attr.attr_loc }
  in
  let spec = Option.map parse spec_attr in
  with_parsed_type_spec t spec

let val_description ~filename v =
  let spec_attr = get_spec_attr v.pval_attributes in
  let parse attr =
    let sp_text, spec = parse_gospel ~filename Uparser.val_spec attr in
    { spec with sp_text; sp_loc = attr.attr_loc }
  in
  let spec = Option.map parse spec_attr in
  with_parsed_val_spec v spec

let ghost_spec ~filename attr =
  let spec, loc = get_spec_content attr in
  let lb = Lexing.from_string spec in
  let sigs = try Parse.interface lb with _ -> W.error ~loc W.Syntax_error in
  match sigs with
  | [ { psig_desc = Psig_type (r, [ t ]); _ } ] ->
      let type_ = type_declaration ~filename t in
      let spec = parsed_of_type_spec type_ in
      let t =
        if spec = None then
          let spec =
            get_inner_spec attr
            |> Option.map (parse_gospel ~filename Uparser.type_spec)
            |> Option.map (fun (ty_text, spec) ->
                   { spec with ty_text; ty_loc = attr.attr_loc })
          in
          with_parsed_type_spec t spec
        else type_
      in
      to_parsed_floating (Type (r, [ t ]))
  | [ { psig_desc = Psig_value vd; _ } ] ->
      let val_ = val_description ~filename vd in
      let spec = parsed_of_val_spec val_ in
      let v =
        if spec = None then
          let spec =
            get_inner_spec attr
            |> Option.map (parse_gospel ~filename Uparser.val_spec)
            |> Option.map (fun (sp_text, spec) ->
                   { spec with sp_text; sp_loc = attr.attr_loc })
          in
          with_parsed_val_spec vd spec
        else val_
      in
      to_parsed_floating (Value v)
  | [ { psig_desc = Psig_open od; _ } ] -> to_parsed_floating (Open od)
  | _ -> assert false

let floating_spec ~filename a =
  try
    let fun_text, fun_ = parse_gospel ~filename Uparser.func a in
    let fun_ = { fun_ with fun_text } in
    let fun_ =
      if fun_.fun_spec = None then
        let fun_spec =
          get_inner_spec a
          |> Option.map (parse_gospel ~filename Uparser.func_spec)
          |> Option.map (fun (fun_text, (spec : fun_spec)) ->
                 { spec with fun_text; fun_loc = a.attr_loc })
        in
        Function { fun_ with fun_spec }
      else Function fun_
    in
    to_parsed_floating fun_
  with W.Error (_, W.Syntax_error) -> (
    try
      let ax_text, axiom = parse_gospel ~filename Uparser.axiom a in
      let parsed_ax = Axiom { axiom with ax_text; ax_loc = a.attr_loc } in
      to_parsed_floating parsed_ax
    with W.Error (_, W.Syntax_error) -> ghost_spec ~filename a)

let parse_specs filename =
  object
    inherit Ast_traverse.map as super

    method! value_description v =
      let v = super#value_description v in
      val_description ~filename v

    method! type_declaration t =
      let t = super#type_declaration t in
      type_declaration ~filename t

    method! signature_item i =
      let i = super#signature_item i in
      match i.psig_desc with
      | Psig_attribute a when is_spec a -> floating_spec ~filename a
      | _ -> i
  end

let signature ~filename = (parse_specs filename)#signature
