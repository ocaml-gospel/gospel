type t = { n : int; s : string }

(*@ function f (x: t): unit =
    match x with
    | { n=p; s=("alpha"|"beta"|"gamma") } -> ()
    | { n=p; s="" } -> () *)
