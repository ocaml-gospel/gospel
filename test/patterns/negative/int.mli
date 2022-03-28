type t = A | B of int

(*@ function f1 (x: t): unit =
    match x with
    | A -> ()
    | B 1i -> ()
    | B 2i -> () *)
