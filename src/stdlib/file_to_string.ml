open Format

let input = Sys.argv.(1)
let output = Sys.argv.(2)
let ic = open_in_bin input
let contents = really_input_string ic (in_channel_length ic)
let () = close_in ic
let oc = open_out output
let fmt = formatter_of_out_channel oc
let () = fprintf fmt "let contents = %S@." contents
let () = close_out oc
