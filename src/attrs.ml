open Ppxlib
open Uast
open Ast_builder.Default

let loc = Location.none
let gospel = "gospel"
let parsed_gospel = "parsed_gospel"
let typed_gospel = "typed_gospel"
let is_spec attr = attr.attr_name.txt = gospel
let is_parsed_spec attr = attr.attr_name.txt = parsed_gospel
let is_typed_spec attr = attr.attr_name.txt = typed_gospel
let noloc txt = { txt; loc = Location.none }

(** UNSAFE ZONE **)

let to_payload x =
  let s = Marshal.to_string x [ Compat_32 ] in
  PStr [ pstr_eval ~loc:Location.none (estring ~loc:Location.none s) [] ]

let to_parsed_attr x =
  attribute ~loc ~name:(noloc parsed_gospel) ~payload:(to_payload x)

let of_payload = function
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ({ pexp_desc = Pexp_constant (Pconst_string (spec, _, _)) }, _);
        };
      ] ->
      Marshal.from_string spec 0
  | _ -> invalid_arg "of_payload"

let parsed_of_attr a =
  if not (is_parsed_spec a) then invalid_arg "of_attr"
  else of_payload a.attr_payload

let parsed_of_attr_list l =
  try
    let a = List.find is_parsed_spec l in
    of_payload a.attr_payload
  with Not_found -> invalid_arg "of_attr_list"

let parsed_of_type_spec t : type_spec option =
  parsed_of_attr_list t.ptype_attributes

let parsed_of_val_spec v : val_spec option =
  parsed_of_attr_list v.pval_attributes

let with_parsed_type_spec t (s : type_spec option) =
  { t with ptype_attributes = to_parsed_attr s :: t.ptype_attributes }

let with_parsed_val_spec v (s : val_spec option) =
  { v with pval_attributes = to_parsed_attr s :: v.pval_attributes }

let to_parsed_floating (f : floating) : signature_item =
  psig_attribute ~loc
    (attribute ~loc ~name:(noloc parsed_gospel) ~payload:(to_payload f))

let of_parsed_floating s : floating =
  match s with
  | Psig_attribute a when is_parsed_spec a -> parsed_of_attr a
  | _ -> invalid_arg "of_parsed_floating"

(** END OF UNSAFE ZONE **)

let get_spec_attr = List.find_opt is_spec

let get_spec_content attr =
  match attr.attr_payload with
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ({ pexp_desc = Pexp_constant (Pconst_string (spec, _, _)) }, _);
        };
      ] ->
      (spec, attr.attr_loc)
  | _ -> assert false

let get_inner_spec attr =
  match attr.attr_payload with
  | PStr [ { pstr_desc = Pstr_eval (_, attrs) } ] -> get_spec_attr attrs
  | _ -> assert false
