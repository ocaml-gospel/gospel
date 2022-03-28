type t = A1 | A2 | B of t

(*@ function f (x: t): unit =
    match x with
    | A1 | A2
    | B A2 | B A1
    | B (B A1) | B (B A2)
    | B (B (B A1))
    | B (B (B A2)) -> () *)
