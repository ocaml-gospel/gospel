
open Format

let input = Sys.argv.(1)
let ic = open_in_bin input
let contents = really_input_string ic (in_channel_length ic)
let () = close_in ic
let () = printf "let contents = %S@." contents
