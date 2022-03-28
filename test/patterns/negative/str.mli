type a = A of string | B

(*@ function f (x: a): unit =
    match x with
    | B
    | A ""
    | A "1"
    | A "22"
    | A "333"
    | A "4444"
    | A "55555"
    | A "666666" -> () *)
