module type S = sig
  type 'a ref
  (*@ ephemeral
      model : 'a *)

  val ref : 'a -> 'a ref
  (*@ r = ref v
        produces r @ 'a ref
        ensures r = v *)

  val get : 'a ref -> 'a
  (*@ v = get r
        preserves r @ 'a ref
        ensures r = v *)

  val update : 'a ref -> 'a -> unit
  (*@ update r v
        modifies r @ 'a ref
        ensures r = v *)
end
