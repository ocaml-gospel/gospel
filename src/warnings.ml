open Ppxlib
open Fmt

type kind =
  | Old_on_read_only of string
  | Return_unit_without_modifies of string

let pp_kind ppf = function
  | Old_on_read_only var ->
      pf ppf "There is a reference to old %s, but is it read only." var
  | Return_unit_without_modifies fct ->
      pf ppf
        "The function %s returns a unit but its specifications does not \
         contains any modifies clause."
        fct

let styled_list l pp = List.fold_left (fun acc x -> styled x acc) pp l

let pp ppf ~loc k =
  pf ppf "%a\n%a@\n@[%a@]@\n"
    (styled `Bold Location.print)
    loc
    (styled_list [ `Yellow; `Bold ] string)
    "Warning" pp_kind k

let old_on_read_only ~loc var = Old_on_read_only var |> pp stderr ~loc

let return_unit_without_modifies ~loc fct =
  Return_unit_without_modifies fct |> pp stderr ~loc
