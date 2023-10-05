open Ppxlib

let is_gospel attr = attr.attr_name.txt = "gospel"

(* Prefix a string with whitespace the length of gospel special comment marker *)
let align = "   "
let eol_and_align = "\n   "

let gospel_txt_of_attributes attr =
  let rec aux attr =
    if is_gospel attr then
      match attr.attr_payload with
      | PStr
          [
            {
              pstr_desc =
                Pstr_eval
                  ( {
                      pexp_desc = Pexp_constant (Pconst_string (txt, loc, _));
                      _;
                    },
                    attrs );
              _;
            };
          ] ->
          { txt; loc } :: (List.map aux attrs |> List.flatten)
      | _ -> []
    else []
  in
  let rec last_loc_end = function
    | [] -> failwith "unreachable case in last_loc_end"
    | [ x ] -> x.loc.loc_end
    | _ :: xs -> last_loc_end xs
  in
  match aux attr with
  | [] -> None
  | x :: _ as xs ->
      let txt =
        align
        ^ String.concat eol_and_align ((List.map (fun { txt; _ } -> txt)) xs)
      and loc_end = last_loc_end xs in
      Some { txt; loc = { x.loc with loc_end } }

let wrap_gospel header txt =
  let header =
    match header with
    | `Declaration -> "Gospel declaration:\n"
    | `Specification -> "Gospel specification:\n"
  in
  Fmt.str "{@gospel[\n%s%s]}" header txt

let payload_of_string ~loc txt =
  let open Ast_helper in
  let content = Const.string ~loc txt in
  let expression = Pexp_constant content |> Exp.mk ~loc in
  let structure_item = Str.eval ~loc expression in
  PStr [ structure_item ]

let attr_label = function
  | `Declaration -> "ocaml.text"
  | `Specification -> "ocaml.doc"

let doc_of_gospel header attr =
  if is_gospel attr then
    let attr_name = { txt = attr_label header; loc = attr.attr_loc }
    and info = gospel_txt_of_attributes attr in
    Option.map
      (fun info ->
        let txt = wrap_gospel header info.txt in
        let attr_payload = payload_of_string ~loc:info.loc txt
        and attr_loc = attr.attr_loc in
        { attr_name; attr_payload; attr_loc })
      info
  else None

let doc_of_gospel_declaration = doc_of_gospel `Declaration
let doc_of_gospel_specification = doc_of_gospel `Specification

(* Attributes with a gospel tag in a signature are gospel declarations,
   that is gospel functions, predicates and axioms *)
let rec signature = function
  | [] -> []
  | ({ psig_desc = Psig_attribute a; psig_loc = loc } as x) :: xs -> (
      match doc_of_gospel_declaration a with
      | Some a ->
          x :: { psig_desc = Psig_attribute a; psig_loc = loc } :: signature xs
      | None -> x :: signature xs)
  | x :: xs -> x :: signature xs

(* Attributes with a gospel tag in the [attributes] node of the ast are
   attached to an OCaml declaration (a value or a type). That means they are
   specifications. *)
let attributes attrs = attrs @ List.filter_map doc_of_gospel_specification attrs

let merge =
  object
    inherit Ast_traverse.map as super
    method! signature s = super#signature s |> signature
    method! attributes attrs = super#attributes attrs |> attributes
  end

let preprocess_intf = merge#signature
let () = Driver.register_transformation ~preprocess_intf "odoc_of_gospel"
