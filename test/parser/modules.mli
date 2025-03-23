(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

module M1 : sig
  (*@ function f1 : integer *)
  (*@ function f2 (n : integer) : integer *)
  (*@ function f3 (n : integer) (m : integer) : integer *)

  module N : sig
    (*@ function f4 : integer = f1 *)
    (*@ function f5 (n : integer) : integer = f2 n *)
    (*@ function f6 (n : integer) (m : integer) : integer = f3 n m *)
  end

  module K : sig
    (*@ predicate p1 = f1 = N.f5 N.f4 *)
    (*@ predicate p2 (n : integer) = f3 n f1 = n *)
    (*@ predicate p3 (n : integer) (m : integer) =
          p2 (N.f5 N.f4) /\ p2 N.f4 /\ p1 *)
  
  end
end

module M2 : sig
  (*@ function f1 : integer = M1.N.f5 M1.N.f4 *)
  (*@ function f2 (n : integer) : integer = M1.f2 n *)
  (*@ function f3 (n : integer) (m : integer) : integer = f2 f1  *)

  module N : sig
    (*@ predicate p1 = f1 = M1.f1 *)
    (*@ predicate p2 (n : integer) = M1.K.p2 n *)
    (*@ predicate p3 (n : integer) (m : integer) = M1.K.p2 f1 /\ M1.K.p3 n m *)
  end

  module K : sig
    (*@ function f4 : integer *)
    (*@ function f5 (n : integer) : integer *)
    (*@ function f6 (n : integer) (m : integer) : integer *)
  end
end
