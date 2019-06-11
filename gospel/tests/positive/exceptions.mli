
exception E

exception E1 of int

exception E2 of int * int

exception E3 of int list

exception E4 of int * int list

exception E5 of (int -> int)

exception E6 of (int -> float -> bool list)

exception E7 of {x : int}

exception E8 of {x : int -> float}

exception E9 of {x : int;
                 y : float}

exception E10 of {x : int -> int -> float;
                  y : float;
                  z : bool}

