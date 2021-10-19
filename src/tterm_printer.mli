open Tterm

val print_vs : Format.formatter -> vsymbol -> unit
val print_ls_decl : Format.formatter -> lsymbol -> unit
val print_ls_nm : Format.formatter -> lsymbol -> unit
val print_pattern : Format.formatter -> pattern -> unit
val print_binop : Format.formatter -> binop -> unit
val print_quantifier : Format.formatter -> quant -> unit
val print_term : Format.formatter -> term -> unit
