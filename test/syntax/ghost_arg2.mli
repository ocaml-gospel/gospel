(*@ function fibonacci (n: integer) : integer *)

val fib : int -> int -> int -> int
(*@ r = fib [i: integer] n a b
    requires i >= 0
    checks n.v >= 0
    requires a.v = fibonacci i
    requires b.v = fibonacci (i+1)
    ensures r.v = fibonacci (i+n.v) *)
