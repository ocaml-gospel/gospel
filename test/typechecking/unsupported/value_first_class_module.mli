module type S = sig
  val s : string
end

val f : (module S) -> string
