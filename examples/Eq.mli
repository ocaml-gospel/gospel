module type S = sig
  type t
  (*@ ephemeral *)

  val st_eq : t -> t -> bool
  (*@ b = st_eq x y
        preserves x @ t
        preserves y @ t
        ensures b <-> x = y *)

  val ph_eq : t -> t -> bool
  (*@ b = ph_eq x y
        preserves x @ loc
        preserves y @ loc
        ensures b <-> x = y *)
end
