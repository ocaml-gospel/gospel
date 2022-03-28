type a = A of int | B

(*@ function f (x: a): unit =
    match x with
    | B
    | A 0i
    | A 1i
    | A 2i
    | A 3i

    | A 5i
    | A 6i -> () *)
