type t = A | B of t * float

(*@ function f1 (x: t): unit =
    match x with
    | A -> ()
    | B (B (B _, _), _)
    | B (B (A, 0.1), 0.)
    | B (B (A, 1.0), 0.)
    | B (B (A, 2.3), 1.)
    | B (A, 1.)
    | B (A, 3.)
    | B (A, x) -> () *)
