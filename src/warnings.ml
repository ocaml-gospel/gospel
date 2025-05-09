open Ppxlib
open Utils

type kind =
  | Arity_mismatch of string * string * int * int
  | Bad_arity of string * int * int
  | Bad_type of string * string
  | Bad_subtype of string * string * string * string
  | Cycle of string * string
  | Cyclic_definition of string
  | Cyclic_definitions of string * string list
  | Desugared_consumes of string list
  | Desugared_produces of string list
  | Duplicated_argument of string
  | Duplicated_consumes of string list
  | Duplicated_header_value of string
  | Duplicated_modifies of string list
  | Duplicated_parameter of string
  | Duplicated_preserves of string list
  | Duplicated_produces of string list
  | Duplicated_record_label of string
  | Duplicated_type_definition of string
  | Illegal_character of char
  | Illegal_escape of string * string option
  | Incompatible_field of string list * string list * string
  | Invalid_arg_number of int * int
  | Invalid_header_name of string
  | Invalid_header_unit of string
  | Invalid_old
  | Invalid_record_labels
  | Invalid_ret_number of int * int
  | Modified_and_preserved of string list
  | No_model of string
  | Not_a_function of string
  | Syntax_error
  | Unbound_exception of string list
  | Unbound_module of string
  | Unbound_record_label of string list
  | Unbound_type of string list
  | Unbound_type_variable of string
  | Unbound_variable of string list
  | Unsupported of string
  | Unterminated_comment

type error = location * kind

exception Error of error

let error ~loc k = raise (Error (loc, k))
let unsupported ~loc s = error ~loc (Unsupported s)

open Fmt

let print_qid ppf l = pf ppf "%a" (list ~sep:(const string ".") string) l
let plural ppf n = if n = 1 then pf ppf "" else pf ppf "s"

let pp_kind ppf = function
  | Arity_mismatch (ty1s, ty2s, expected, received) ->
      pf ppf
        "Mismatch between a type %s@ and type %s@\n\
         Mismatch between arity %d and arity %d"
        ty1s ty2s expected received
  | Bad_arity (f, expected, got) ->
      pf ppf
        "The type constructor %s expected %d argument(s)@ but is applied to %d \
         argument(s) here"
        f expected got
  | Bad_subtype (ty1, ty2, ty1_sub, ty2_sub) ->
      pf ppf
        "Mismatch between type %s@ and type %s@\n\
         Type %s is incompatible with type %s"
        ty1 ty2 ty1_sub ty2_sub
  | Bad_type (t1, t2) -> pf ppf "Mismatch between type %s@ and type %s" t1 t2
  | Cycle (v, t) ->
      pf ppf
        "This expression has type '%s@ but an expression was expected of type \
         %s@ The type variable '%s occurs inside %s"
        v t v t
  | Cyclic_definition v -> pf ppf "The type abbreviation %s is cyclic" v
  | Cyclic_definitions (v, l) ->
      pf ppf "The type abbreviation %s contains a cycle@\n%a" v
        (list ~sep:arrow string) (l @ [ v ])
  | Desugared_consumes v ->
      pf ppf
        "The variable %a appears in a produces and in a preserves or modifies \
         clause"
        print_qid v
  | Desugared_produces v ->
      pf ppf
        "The variable %a appears in a produces and in a preserves or modifies \
         clause"
        print_qid v
  | Duplicated_argument arg -> pf ppf "Duplicated argument %s" arg
  | Duplicated_consumes arg ->
      pf ppf "The variable %a is listed as consumes twice" print_qid arg
  | Duplicated_header_value v ->
      pf ppf "The variable %s is defined twice in this header" v
  | Duplicated_modifies v ->
      pf ppf "The variable %a is listed as modified twice" print_qid v
  | Duplicated_preserves v ->
      pf ppf "The variable %a is listed as preserved twice" print_qid v
  | Duplicated_produces v ->
      pf ppf "The variable %a is listed as produced twice" print_qid v
  | Duplicated_parameter s ->
      pf ppf "The type parameter %s occurs several times" s
  | Duplicated_record_label l -> pf ppf "Two labels are named %s" l
  | Duplicated_type_definition s ->
      pf ppf "Multiple definitions of the type name %s@\n" s
  | Invalid_arg_number (expected, got) ->
      pf ppf "This header has %d argument%a but expected %d" got plural got
        expected
  | Invalid_header_name s ->
      pf ppf
        "Header name %s does not match the declared value in the OCaml \
         interface"
        s
  | Invalid_header_unit v ->
      pf ppf
        "This pattern matches on values of type unit, which is incompatible \
         with %s"
        v
  | Invalid_old -> pf ppf "The old tag cannot be used in this term"
  | Invalid_record_labels -> pf ppf "No record found with the provided labels"
  | Invalid_ret_number (expected, got) ->
      pf ppf "This header has %d return value%a but expected %d" got plural got
        expected
  | Illegal_character c -> pf ppf "Illegal character %c" c
  | Illegal_escape (s, explanation) ->
      pf ppf "Illegal backslash escape in string or character (%s)%a" s
        (option (fmt ": %s"))
        explanation
  | Incompatible_field (field, field_type, expected_type) ->
      pf ppf
        "The identifier %a belongs to the type %a but is mixed here with \
         fields of type %s"
        (list ~sep:(const string ".") string)
        field
        (list ~sep:(const string ".") string)
        field_type expected_type
  | Modified_and_preserved v ->
      pf ppf "The variable %a is present in a modifies and preserves clause"
        print_qid v
  | No_model v ->
      pf ppf "The type %s has no model, it cannot have an invariant" v
  | Not_a_function s ->
      pf ppf "Expected a functional value but received a value of type %s" s
  | Syntax_error -> pf ppf "Syntax error"
  | Unbound_exception s -> pf ppf "Unbound exception %a" print_qid s
  | Unbound_module s -> pf ppf "Unbound module %s" s
  | Unbound_record_label s -> pf ppf "Unbound record label %a" print_qid s
  | Unbound_type s -> pf ppf "Unbound type constructor %a" print_qid s
  | Unbound_type_variable s ->
      pf ppf "The type variable '%s is unbound in this type declaration" s
  | Unbound_variable s -> pf ppf "Unbound value %a" print_qid s
  | Unsupported s -> pf ppf "Not yet supported: %s" s
  | Unterminated_comment -> pf ppf "Unterminated comment"

let styled_list l pp = List.fold_left (fun acc x -> styled x acc) pp l

(** [pp_gen pp_sort pp_kind ppf loc k] display the message of the given sort
    (warning, error, etc.) at the location obtained after fixing [loc] (it might
    have been broken by preprocessing *)
let pp_gen pp_sort pp_kind ppf loc k =
  let input_filename = loc.loc_start.pos_fname in
  match input_filename with
  | "" ->
      pf ppf
        "Internal error: no filename location for the following error@.%a: \
         @[%a.@]"
        pp_sort k pp_kind k
  | "_none_" ->
      (* This location is used for builtins such as [list] *)
      pf ppf "%a: @[%a.@]" pp_sort k pp_kind k
  | _ ->
      let input = Pp_loc.Input.file input_filename in
      (* because of the preprocessor (gospel pps), we may obtain locations that:
         - have correct line numbers and correct offsets within a line (pos_cnum -
           pos_bol) (where "correct" means that they refer to the user-written file
           before preprocessing); but
         - where the [pos_cnum] and [pos_bol] fields refer to offsets within the
           file *after* preprocessing rather than *before*.

         We thus use helpers from [Pp_loc.Position] to recompute full positions from
         line/column numbers with respect to the input file before preprocessing.
      *)
      let repair_pos (p : Lexing.position) : Lexing.position =
        Pp_loc.Position.of_line_col p.pos_lnum (p.pos_cnum - p.pos_bol + 1)
        |> Pp_loc.Position.to_lexing ~filename:input_filename input
      in
      let start_pos, end_pos =
        (repair_pos loc.loc_start, repair_pos loc.loc_end)
      in
      pf ppf "%a@\n%a%a: @[%a@]"
        (styled `Bold Location.print)
        { loc_start = start_pos; loc_end = end_pos; loc_ghost = false }
        (Pp_loc.pp ~max_lines:10 ~input)
        [
          ( Pp_loc.Position.of_lexing start_pos,
            Pp_loc.Position.of_lexing end_pos );
        ]
        pp_sort k pp_kind k

let pp_warning ppf _ = styled_list [ `Magenta; `Bold ] string ppf "Warning"
let pp_error ppf _ = styled_list [ `Red; `Bold ] string ppf "Error"
let pp ppf (loc, k) = pp_gen pp_error pp_kind ppf loc k
