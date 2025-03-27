open Ppxlib
open Utils

type kind =
  | Bad_arity of string * int * int
  | Bad_type of string * string
  | Duplicated_argument of string
  | Duplicated_parameter of string
  | Duplicated_record_label of string
  | Illegal_character of char
  | Illegal_escape of string * string option
  | Incompatible_field of string list * string list * string
  | Invalid_record_labels
  | Syntax_error
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

let function_ ppf (f, t1, t2) = pf ppf "%s: %s -> %s" f t1 t2

let pp_kind ppf = function
  | Bad_arity (f, expected, got) ->
      pf ppf
        "The type constructor %s expects %d argument(s)@ but is applied to %d \
         argument(s) here"
        f expected got
  | Bad_type (t1, t2) ->
      pf ppf
        "This value has type %s@ but an expression was expected of type@ %s" t1
        t2
  | Duplicated_argument arg -> pf ppf "Duplicated argument %s" arg
  | Duplicated_parameter s ->
      pf ppf "The type parameter %s occurs several times" s
  | Duplicated_record_label l -> pf ppf "Two labels are named %s" l
  | Invalid_record_labels -> pf ppf "No record found with the provided labels"
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
  | Syntax_error -> pf ppf "Syntax error"
  | Unbound_module s -> pf ppf "Unbound module %s" s
  | Unbound_record_label s ->
      pf ppf "Unbound record label %a" (list ~sep:(const string ".") string) s
  | Unbound_type s ->
      pf ppf "Unbound type constructor %a"
        (list ~sep:(const string ".") string)
        s
  | Unbound_type_variable s ->
      pf ppf "The type variable '%s is unbound in this type declaration" s
  | Unbound_variable s ->
      pf ppf "Unbound value %a" (list ~sep:(const string ".") string) s
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
