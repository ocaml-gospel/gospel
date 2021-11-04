open Fmt
open Tast

let signature f x = pf f "%s\n" (show_signature x)
