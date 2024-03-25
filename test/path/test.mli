module M : sig
  type t

  (*@ function f : t *)
  (*@ function g : t = f *)
  (*@ axiom a1 : f = g *)

  module Nested : sig
    type t

    (*@ function nf : t *)
    (*@ function ng : t *)
    (*@ axiom n1 : ng = nf *)
  end

  (*@ axiom a2 : Nested.nf = Nested.ng *)
  (*@ open Nested *)
  (*@ axiom a3 : nf = ng*)
end

module N : sig
  (*@ function g (n : M.t) : integer *)
  (*@ function f (n : integer) : integer =
        g M.g + n *)
  (*@ function w (n : integer) : integer =
        f n + n *)

  module Nested : sig
    (*@ function z : integer = g M.g *)
  end

  (*@ predicate x (n : integer) = n = Nested.z *)
end
