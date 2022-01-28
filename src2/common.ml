type arg_label = Lnone | Loptional of string | Lnamed of string
type 'a labelled = { label : arg_label; symbol : 'a }
