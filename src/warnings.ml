open Ppxlib
open Fmt

type kind = Return_unit_without_modifies of string

let pp_kind ppf = function
  | Return_unit_without_modifies fct ->
      pf ppf
        "The function `%s' returns a unit but its specifications does not \
         contains any `modifies' clause."
        fct

let styled_list l pp = List.fold_left (fun acc x -> styled x acc) pp l

let pp ppf ~loc k =
  pf ppf "%a\n%a@\n@[%a@]@\n"
    (styled `Bold Location.print)
    loc
    (styled_list [ `Yellow; `Bold ] string)
    "Warning" pp_kind k

let return_unit_without_modifies ~loc fct =
  Return_unit_without_modifies fct |> pp stderr ~loc
