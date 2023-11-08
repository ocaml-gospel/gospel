module type A = sig
  type t
end

module type B = sig
  type t

  module C : A with type t = t
end
