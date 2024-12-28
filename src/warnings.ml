open Ppxlib
open Utils

type kind =
  | Ambiguous_pattern
  | Bad_arity of string * int * int
  | Bad_record_field of string
  | Bad_type of string * string
  | Bad_type_arity of string * int * int
  | Circular_open
  | Coercion_already_defined of (string * string * string) list
  | Coercion_cycle of (string * string * string) list
  | Cyclic_type_declaration of string
  | Duplicated_argument of string
  | Duplicated_record_field of string
  | Duplicated_variable of string
  | Field_application of string
  | Formula_expected
  | Free_variables of string list
  | Function_symbol_expected of string
  | Functor_application of string
  | Illegal_character of char
  | Illegal_escape of string * string option
  | Inlined_record_expected
  | Invalid_coercion_type of string
  | Invalid_int_literal of string * char option
  | Label_missing of string list
  | Module_not_found of string
  | Name_clash of string
  | Not_a_constructor of string
  | Old_in_precond of string
  | Old_in_fun_spec
  | Partial_application of string
  | Pattern_bad_type of string * string
  | Pattern_fully_guarded
  | Pattern_guard_not_exhaustive of string
  | Pattern_not_exhaustive of string
  | Pattern_redundant of string
  | Predicate_symbol_expected of string
  | Public_type_invariant of string
  | Return_unit_without_modifies of string
  | Symbol_not_found of string list
  | Syntax_error
  | Term_expected
  | Type_checking_error of string
  | Unbound_label of string
  | Unbound_variable of string
  | Unsupported of string
  | Unterminated_comment
  | Wrong_name of string * string

type error = location * kind

exception Error of error

let error ~loc k = raise (Error (loc, k))
let type_checking_error ~loc s = error ~loc (Type_checking_error s)
let unsupported ~loc s = error ~loc (Unsupported s)

open Fmt

let function_ ppf (f, t1, t2) = pf ppf "%s: %s -> %s" f t1 t2

let pp_kind ppf = function
  | Ambiguous_pattern ->
      pf ppf
        "Or-patterns are prohibited under guards to avoid ambiguities@ (see \
         OCaml compiler warning 57)"
  | Bad_arity (f, expected, got) ->
      pf ppf
        "The constructor %s expects %d argument(s)@ but is applied to %d \
         argument(s) here"
        f expected got
  | Bad_record_field f -> pf ppf "The record field %s does not exist" f
  | Bad_type (t1, t2) ->
      pf ppf "This term has type %s@ but a term was expected of type@ %s" t1 t2
  | Bad_type_arity (t, expected, got) ->
      pf ppf
        "The type %s expects %d argument(s)@ but was given %d argument(s) here"
        t expected got
  | Circular_open -> pf ppf "This open introduces a dependency cycle"
  | Coercion_already_defined fl ->
      pf ppf "A coercion between these types already exists:@\n  @[%a@]"
        (list ~sep:newline function_)
        fl
  | Coercion_cycle fl ->
      pf ppf "This coercion introduces a cycle:@\n  @[%a@]"
        (list ~sep:newline function_)
        fl
  | Cyclic_type_declaration t ->
      pf ppf "The type declaration for %s contains a cycle" t
  | Duplicated_argument arg -> pf ppf "Duplicated argument %s" arg
  | Duplicated_record_field f ->
      pf ppf "A record field with name %s already exists" f
  | Duplicated_variable s ->
      pf ppf "The variable %s is duplicated in this pattern" s
  | Field_application f -> pf ppf "The record field %s cannot be applied" f
  | Formula_expected -> pf ppf "A formula was expected"
  | Free_variables vl ->
      pf ppf "Unbound variables: %a" (list ~sep:comma string) vl
  | Function_symbol_expected s -> pf ppf "Not a function symbol: %s" s
  | Functor_application s -> pf ppf "Functor application not supported: %s" s
  | Illegal_character c -> pf ppf "Illegal character %c" c
  | Illegal_escape (s, explanation) ->
      pf ppf "Illegal backslash escape in string or character (%s)%a" s
        (option (fmt ": %s"))
        explanation
  | Inlined_record_expected ->
      pf ppf "This constructor expects an inlined record argument"
  | Invalid_coercion_type f ->
      pf ppf "The function %s does not have a valid coercion type" f
  | Invalid_int_literal (s, c) ->
      pf ppf "Invalid int literal: %s%a" s (option char) c
  | Label_missing labels ->
      pf ppf "Some record fields are undefined: %a" (list ~sep:sp string) labels
  | Module_not_found m -> pf ppf "No module with name %s" m
  | Name_clash s ->
      pf ppf "A declaration for %s already exists in this context" s
  | Not_a_constructor f -> pf ppf "The symbol %s is not a constructor" f
  | Old_in_precond precond ->
      pf ppf "old operator is not allowed in %s clauses" precond
  | Old_in_fun_spec ->
      pf ppf "old operator is not allowed in specifications for pure functions"
  | Partial_application s ->
      pf ppf "The symbol %s cannot be partially applied" s
  | Pattern_bad_type (t1, t2) ->
      pf ppf
        "This pattern matches values of type %s@ but a pattern was expected \
         which matches values of type@ %s"
        t1 t2
  | Pattern_fully_guarded ->
      pf ppf "All clauses in this pattern-matching are guarded"
  | Pattern_guard_not_exhaustive p ->
      pf ppf
        "This pattern-matching may not be exhaustive because of the guard.@\n\
         Here is an example of a case that may not be matched:@\n\
        \  %s"
        p
  | Pattern_not_exhaustive p ->
      pf ppf
        "This pattern-matching is not exhaustive.@\n\
         Here is an example of a case that is not matched:@\n\
        \  %s"
        p
  | Pattern_redundant p ->
      pf ppf
        "The pattern-matching is redundant.@\n\
         Here is a case that is unused:@\n\
        \  %s"
        p
  | Predicate_symbol_expected s -> pf ppf "Not a predicate symbol: %s" s
  | Public_type_invariant t -> pf ppf "Invariant on public type %s" t
  | Return_unit_without_modifies f ->
      pf ppf
        "The function %s returns unit@ but its specifications does not contain \
         any modifies clause"
        f
  | Symbol_not_found sl ->
      pf ppf
        "Symbol %a not found in scope@ (see \"Symbols in scope\" documentation \
         page)"
        (list ~sep:(const string ".") string)
        sl
  | Syntax_error -> pf ppf "Syntax error"
  | Term_expected -> pf ppf "A term was expected"
  | Type_checking_error msg -> pf ppf "Type checking error: %a" text msg
  | Unbound_label s -> pf ppf "Unbound record field: %s" s
  | Unbound_variable s ->
      pf ppf "The variable %s does not appear in this pattern" s
  | Unsupported s -> pf ppf "Not yet supported: %s" s
  | Unterminated_comment -> pf ppf "Unterminated comment"
  | Wrong_name (field, constr) ->
      pf ppf
        "The field %s is not part of the record argument for the constructor %s"
        field constr

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
      pf ppf "%a@\n%a%a: @[%a.@]"
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
